from string import Template
from .lorentz_algebra import calc_lorentz
from .lorentz_wavefunction import contract_wavefunctions
from .lorentz_printer import LorentzPrinter
from .lorentz_simplify import simplify_symbolic
from pathlib import Path
import sympy
import math
import pkgutil
import multiprocessing
from .message import progress

TYPE_DICT = {
    1: "CScalar",
    2: "CSpinor",
    3: "CVec4",
    4: "CRaritaSchwinger",
}

VECT_GAUGE_DICT = {
    0: "0",
    1: "ATOOLS::Spinor<SType>::R1()",
    2: "ATOOLS::Spinor<SType>::R2()",
    3: "ATOOLS::Spinor<SType>::R3()",
}


FORM_FACTOR_IMPL = '''
        std::string ffkey = "{}";
        p_ff = FF_Getter::GetObject(ffkey,key);
        if (p_ff == NULL) {{
          msg_Out()<<*key.p_mv<<std::endl;
          THROW(fatal_error,"Form factor not implemented '"+ffkey+"'");
        }}
'''

FORM_FACTOR_DECL = '''
    private:
        const Form_Factor *p_ff = NULL;
'''


def _get_current_in(spin, index, key, ferm_partner):
    impl = ""
    if spin % 2 != 0:
        impl += f'const {TYPE_DICT[spin]}<SType> & j{index} = *(jj[{key}]->Get<{TYPE_DICT[spin]}<SType>>());\n'
    else:
        impl += (f'const {TYPE_DICT[spin]}<SType> & j{index} = ((jj[{key}]->Get<{TYPE_DICT[spin]}<SType>>())->B() == {ferm_partner} ? '
                 f'(*(jj[{key}]->Get<{TYPE_DICT[spin]}<SType>>())) : (*(jj[{key}]->Get<{TYPE_DICT[spin]}<SType>>())).CConj());\n')
    if spin == 1:
        impl += f'const SComplex & j{index}0 = j{index}[0];\n'
        return impl
    for i in range(4):
        if spin == 4:
            for j in range(4):
                impl += f'const SComplex & j{index}{4*i + j} = j{index}[4*{VECT_GAUGE_DICT[i]}+{j}];\n'
        elif spin == 3:
            impl += f'const SComplex & j{index}{i} = j{index}[{VECT_GAUGE_DICT[i]}];\n'
        else:
            impl += f'const SComplex & j{index}{i} = j{index}[{i}];\n'
    return impl


def _get_current_out(spin, index):
    if spin == 1:
        return f'CScalar<SType>* j{index} = nullptr;\n'
    elif spin == 2:
        return f'CSpinor<SType>* j{index} = nullptr;\n'
    elif spin == 3:
        return f'CVec4<SType>* j{index} = nullptr;\n'
    elif spin == 4:
        return f'CRaritaSchwinger<SType>* j{index} = nullptr;\n'
    else:
        raise ValueError(f'Cannot handle spin {spin}')


def _get_mom_in(index, key):
    impl = f'const ATOOLS::Vec4D & p{index} = p_v->J({key})->P();\n'
    for i in range(4):
        impl += f'const double& p{index}{i} = p{index}[{VECT_GAUGE_DICT[i]}];\n'
    return impl


def _get_mom_out(key, all_keys):
    key_index = all_keys.index(key)
    in_keys = all_keys[:key_index] + all_keys[key_index+1:]
    impl = f"ATOOLS::Vec4D p{key} = "
    for in_key in in_keys:
        impl += f"-p{in_key}"
    impl += ';\n'
    for i in range(4):
        impl += f'const double& p{key}{i} = p{key}[{VECT_GAUGE_DICT[i]}];\n'
    return impl


def _filter_lorentz(struct, nmax):
    if len(struct.spins) > nmax:
        return False
    if any(spin < 0 or spin >= 4 for spin in struct.spins):
        return False
    return True


def _need_mom(struct, form_factor):
    return "P(" in struct.structure or not isinstance(form_factor, (int, float))


class _LorentzImpl:
    def __init__(self, struct):
        self._spins = struct.spins
        self._lorentz, self._form_factor = calc_lorentz(struct.structure)
        self._numeric = 1.0
        name = None
        if isinstance(self._lorentz, sympy.Expr):
            self._numeric, name, indices = simplify_symbolic(self._lorentz)
            if indices != []:
                self._lorentz = name[indices]
            else:
                self._lorentz = name
        self._rotations = contract_wavefunctions(self._lorentz, self._spins)

        self._printer = LorentzPrinter()
        self._printer.set_form_factor(self._form_factor)
        if name is not None:
            self._printer.set_numeric({str(name): self._numeric})

        self._need_ff = False
        self._need_mom = _need_mom(struct, self._form_factor)
        if self._printer.form_factor is not None:
            self._ff_name = self._printer.form_factor_name
            self._need_ff = True

        self.impl = ''
        self.ff_impl = ''
        self.ff_decl = ''

        # Collect the flow of fermion lines, assuming that the lines connect
        # consecutive pairs of fermions.
        self.ferm_partner = {}
        current_pair = []
        for index, spin in enumerate(self._spins):
            if spin % 2 == 0:
                current_pair.append(index)
                if len(current_pair) == 2:
                    self.ferm_partner[current_pair[0]] = current_pair[1]
                    current_pair = []

    def __call__(self):
        for i, (rotation, expr) in enumerate(self._rotations):
            self._handle_rotation(i, rotation, expr)

    def _handle_rotation(self, idx, rotation, expr):
        self._printer.set_index_spin(idx, self._spins[idx])
        self.impl += self._write_header(idx, rotation)
        self.impl += self._write_initialization(idx)
        self.impl += self._printer.doprint(expr) + '\n'
        if self._need_ff:
            self.ff_impl = FORM_FACTOR_IMPL.format(self._ff_name)
            self.ff_decl = FORM_FACTOR_DECL
            self.impl += f'(*j{idx}) *= {self._printer.form_factor};\n'
        self.impl += self._write_close(idx)

    def _write_header(self, index, rotation):
        header = f'// if outgoing index is {index}\n'
        header += f'if (p_v->V()->id.back()=={index}) {{\n'
        for i, spin in enumerate(self._spins):
            if i == index:
                continue

            ferm_partner = 1 if (i in self.ferm_partner.keys()) else -1
            header += _get_current_in(spin, i, rotation[i], ferm_partner)
            if self._need_mom:
                header += _get_mom_in(i, rotation[i])
        header += _get_current_out(self._spins[index], index)
        if self._need_mom:
            header += _get_mom_out(index, list(range(len(self._spins))))
        return header

    def _write_initialization(self, index):
        spin = self._spins[index]
        barred = -1*math.prod([-1 if spin % 2 == 0 else 1 for spin in self._spins[:index]])
        if spin == 1:
            return f'j{index} = CScalar<SType>::New();\n'
        elif spin == 2:
            # TODO: Handle on_type for massless optimization
            return f'j{index} = CSpinor<SType>::New(m_r[{index}],{int(barred)},0,0,0,0,3);\n'
        elif spin == 3:
            return f'j{index} = CVec4<SType>::New();\n'
        elif spin == 4:
            return f'j{index} = CRaritaSchwinger<SType>::New(m_r[{index}],{int(barred)},0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);\n'
        else:
            raise ValueError(f'Cannot handle spin {spin}')

    def _write_close(self, index):
        spin_or = '|'.join([f'j{id}.S()' for id, _ in enumerate(self._spins) if id != index])
        return f'j{index}->SetS({spin_or});\nreturn j{index};\n}}\n'


class LorentzWriter:
    def __init__(self, path, nmax):
        lorentz_template = pkgutil.get_data(__name__,
                                            "Templates/lorentz_calc_template.C")
        lorentz_template = lorentz_template.decode('utf-8')
        self._template = Template(lorentz_template)
        self._path = Path(path) if not isinstance(path, Path) else path
        self._nmax = nmax

    def _get_impl(self, struct):
        lorentz_impl = _LorentzImpl(struct)
        lorentz_impl()
        return lorentz_impl.impl, lorentz_impl.ff_impl, lorentz_impl.ff_decl

    def write_all(self, structs, ncores):
        structs = filter(lambda struct: _filter_lorentz(struct, self._nmax),
                         structs)
        with multiprocessing.Pool(ncores) as pool:
            pool.map(self.write, structs)

    def write(self, struct):
        progress(f"Calculating lorentz structure: {struct.name}")
        impl, ff_impl, ff_decl = self._get_impl(struct)
        subs = {
            'vertex_name': struct.name,
            'implementation': impl,
            'form_factor_impl': ff_impl,
            'form_factor_decl': ff_decl,
        }
        with open(f'{self._path}/{struct.name}.C', 'w') as output:
            output.write(self._template.substitute(subs))
