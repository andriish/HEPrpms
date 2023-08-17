
from __future__ import print_function

help_message = """
OpenLoops build system

Usage: scons [options] <loops>=proc1,proc2,...

<loops> determines the content of the process library
- t=tree, l=loop, s=loop_squared, p=pseudo_tree;
  if 'l' is present, any combination of these is allowed (inalphabetic order),
  otherwise 't' must be alone
- If a processes ends with a '.coll' it is treated as a process collection,
  i.e. the corresponding list of processes from the collection file on the web
  server is used. The special collection all.coll downloads all processes.
- Several <loops> arguments can be given, even with the same <loops>.

Options

-c (SCons built-in)
  Clean, i.e. delete object files and libraries.
  See also clean=... option.

gjobs=<n>
  Generate 'n' processes simultaneously.
  n > 1 implies --glog
  n < 1 uses the number of CPU core of the machine

glog=0/1
  Write generator output to a log file; implied if gjobs > 1.

force_download=0/1
  Download processes even if they seem to be up-to-date.

process_update=0/1
  If the downloader is used: select all downloaded processes
    (i.e. their version.info file contains 'process_update').
  If the generator is used: select all processes
    which have been generated or downloaded before.

release=<r>
  A string of max. length 8 to specify the OpenLoops library version number
  (this does not affect the process libraries).

generator=0/1/2
  0: off
  1: use the process generator
  2: use the process downloader

compile=0/1/2
  0: don't compile
  1: compile processes,
     compile generic libraries only if no process was specified
  2: compile processes and generic libraries

debug=0/1
  Add debug flags to Fortran compiler flags.

clean=procs,src
  Only effective in conjunction with -c.
  procs: delete the proclib and process_obj directories
         which contain libraries and object code of all processes.
  src: delete process source of the given processes,
       rsp. the process_src directory which contains
       the source of all processes if 'procs' is also given.

Also built-in SCons options can be used. However:
  - no_exec (-n) will not work properly,
    because the process generator is run in a sub-process.
    (the compile script cannot know what to do
    unless the generator finished generating the source code)
  - SCons options are not passed to the process generator.

For more options see ./pyol/config/default.cfg.
User defined options can be set in ./openloops.cfg#include "global_coli.h"
in the same way as in default.cfg.
"""

# TODO
# - Use kind types from kind_types module in CutTools.
# - When compiling a process: check for compatibility with the OpenLoops version.

import os
import sys
import subprocess

sys.path.insert(0, os.path.abspath(os.path.join('pyol', 'tools')))

import OLBaseConfig
import OLToolbox
from OLLibrary import CPPContainer, OLLibrary

# This is a workaround for a bug in SCons 3
# (tested with 3.0.1, no later version available as of now):
# Use HashableLiteral instead of Literal e.g. when setting RPATH to $ORIGIN
# to avoid the error
# TypeError `unhashable type: 'Literal'' trying to evaluate `${_concat(RPATHPREFIX, RPATH, RPATHSUFFIX, __env__)}'
import SCons
class HashableLiteral(SCons.Subst.Literal):
    def __hash__(self):
        return hash(self.lstr)

if '--help' in sys.argv or '-h' in sys.argv:
    print(help_message)
    Exit(0)

scons_cmd = [sys.argv[0]]
olpython = os.environ.get('OLPYTHON', None)
if olpython:
    scons_cmd = [olpython] + scons_cmd


process_arguments = list(filter(lambda el: el[0] in
                                OLBaseConfig.loops_specifications, ARGLIST))
commandline_options = list(filter(lambda el: el[0] not in
                                  OLBaseConfig.loops_specifications, ARGLIST))

config = OLBaseConfig.get_config(commandline_options)

if config['print_python_version']:
    print('SConstruct uses Python', sys.version)

SetOption('num_jobs', config['num_jobs'])

generator_options = ['--jobs=' + str(config['gjobs'])]
if config['glog']:
    generator_options.append('--log')

if config['force_download']:
    force_download_flag = ['--force']
else:
    force_download_flag = []

if config['release']:
    release_version = config['release']
else:
    release_version = ''

git_revision = str(OLToolbox.get_git_revision(mandatory = False))

# Install directory; only effect is that this is put into
# the openloops library as a string. TODO: override by command line argument;
# and actually install the libraries there
install_path =  os.path.abspath(os.path.join(config['process_lib_dir'],"../"))
max_string_length = max(config['max_string_length'], len(install_path) + 80)

generate_process_true = ((config['generator'] == 1) and
                         not GetOption('clean') and not GetOption('no_exec'))
download_process_true = ((config['generator'] == 2) and
                         not GetOption('clean') and not GetOption('no_exec'))

if config['compile'] == 0 or (config['compile'] == 1 and
                              (len(process_arguments) > 0 or
                               config['process_update'])):
    compile_libraries = []
else:
    compile_libraries = config['compile_libraries']

cpp_defines = list(map(lambda lib: 'USE_' + lib.upper(), config['link_libraries']))
cpp_defines += [('KIND_TYPES', 'kind_types'),
                ('DREALKIND', 'dp'),
                ('QREALKIND', 'qp'),
                'USE_' + config['fortran_tool'].upper(),
                ('OL_INSTALL_PATH', '\\"' + install_path + '\\"'),
                ('MAXSTRLEN', str(max_string_length)),
                'SING',
                'collierdd']
if config['expert_mode']:
    cpp_defines.append('EXPERT')


# ================= #
# Generic libraries #
# ================= #

lib_src_dirs = {}
lib_obj_dirs = {}
lib_mod_dirs = {}
for libname in ['olcommon', 'rambo', 'qcdloop', 'oneloop', 'cuttools', 'collier', 'openloops', 'trred']:
    lib_src_dirs[libname] = os.path.join(config['lib_src_dir'], libname, 'src')
    lib_obj_dirs[libname] = os.path.join(config['lib_src_dir'], libname, 'obj')
    lib_mod_dirs[libname] = os.path.join(config['lib_src_dir'], libname, 'mod')

# OLCommon
olcommon_dp_src = ['kind_types.F90', 'debug.F90', 'cwrappers.c']
olcommon_mp_src = ['common.F90']

# Rambo
rambo_dp_src = ['rambo.f']

# QCDLoop
#qcdloop_dp_src = [
    #'aacbc.f', 'aaccc.f', 'aacinv.f', 'aaxbx.f', 'aaxcx.f', 'aaxdx.f', 'aaxex.f', 'aaxinv.f',
    #'auxCD.f', 'ddilog.f', 'ff2dl2.f', 'ffabcd.f', 'ffca0.f', 'ffcb0.f', 'ffcb1.f', 'ffcb2.f',
    #'ffcb2p.f', 'ffcc0.f', 'ffcc0p.f', 'ffcc1.f', 'ffcdb0.f', 'ffcel2.f', 'ffcel3.f', 'ffcel4.f',
    #'ffcel5.f', 'ffceta.f', 'ffcli2.f', 'ffcrr.f', 'ffcxr.f', 'ffcxs3.f', 'ffcxs4.f', 'ffcxyz.f',
    #'ffdcc0.f', 'ffdcxs.f', 'ffdel2.f', 'ffdel3.f', 'ffdel4.f', 'ffdel5.f', 'ffdel6.f', 'ffdl2i.f',
    #'ffdl5p.f', 'ffdxc0.f', 'ffinit_mine.f', 'ffrcvr.f', 'fftran.f', 'ffxb0.f', 'ffxb1.f', 'ffxb2p.f',
    #'ffxc0.f', 'ffxc0i.f', 'ffxc0p.f', 'ffxc1.f', 'ffxd0.f', 'ffxd0h.f', 'ffxd0i.f', 'ffxd0p.f',
    #'ffxd1.f', 'ffxdb0.f', 'ffxdbd.f', 'ffxdi.f', 'ffxdpv.f', 'ffxe0.f', 'ffxe1.f', 'ffxf0.f',
    #'ffxf0h.f', 'ffxli2.f', 'ffxxyz.f', 'npoin.f', 'qlbox1.f', 'qlbox10.f', 'qlbox11.f', 'qlbox12.f',
    #'qlbox13.f', 'qlbox14.f', 'qlbox15.f', 'qlbox16.f', 'qlbox2.f', 'qlbox3.f', 'qlbox4.f', 'qlbox5.f',
    #'qlbox6.f', 'qlbox7.f', 'qlbox8.f', 'qlbox9.f', 'qlcLi2omx2.f', 'qlcLi2omx3.f', 'qlfndd.f',
    #'qlfunctions.f', 'qlI1.f', 'qlI2.f', 'qlI2fin.f', 'qlI3.f', 'qlI3fin.f', 'qlI3sub.f', 'qlI4.f',
    #'qlI4array.f', 'qlI4DNS41.f', 'qlI4fin.f', 'qlI4sub0m.f', 'qlI4sub1m.f', 'qlI4sub2m.f',
    #'qlI4sub2ma.f', 'qlI4sub2mo.f', 'qlI4sub3m.f', 'qlinit.f', 'qlkfn.f', 'qlLi2omprod.f',
    #'qlLi2omrat.f', 'qlLi2omx.f', 'qlLi2omx2.f', 'qllnomrat4.f', 'qllnrat.f', 'qlratgam.f',
    #'qlratreal.f', 'qlsnglsort.f', 'qlspencer.f', 'qltri1.f', 'qltri2.f', 'qltri3.f', 'qltri4.f',
    #'qltri5.f', 'qltri6.f', 'qltrisort.f', 'qlxpicheck.f', 'qlYcalc.f', 'qlzero.f', 'spence.f']
qcdloop_dp_src = [
    'box.cc', 'bubble.cc', 'cache.cc', 'qcdloop.cc', 'tadpole.cc', 'tools.cc', 'topology.cc',
    'triangle.cc', 'types.cc', 'wrapper.cc']

# OneLOop -- contains both, dp and qp routines
oneloop_dp_src = ['avh_olo_qp.f90']

# OpenLoops
openloops_mp_src = [
    'contractions.F90', 'converter.F90', 'counterterms.F90', 'helicity.F90',
    'i-operator.F90', 'kinematics.F90', 'laststep.F90', 'loopmom_tensor.F90',
    'looproutines.F90', 'Lpropagators.F90', 'Lvertices.F90', 'parameters.F90',
    'parameters_init.F90', 'scalarintegrals.F90', 'renormalisation_ew.F90',
    'renormalisation_qcd.F90', 'propagators.F90', 'vertices.F90', 'wavefunctions.F90',
    'helbookkeeping.F90','Hhelicity.F90','Hcontractions.F90', 'Hcounterterms.F90',
    'Hpropagators.F90', 'Hvertices.F90', 'Hlaststep.F90', 'HLvertices.F90',
    'HLpropagators.F90',
    'otf_reduction.F90','loopreduction.F90','loophandling.F90']

openloops_dp_src = [
    'helicity_init.F90', 'init_ui.F90', 'stability.F90', 'tensor_handling.F90']

if config['interface'] >= 1:
    openloops_dp_src.append('ol_interface.F90')
if config['interface'] >= 2:
    openloops_dp_src.append('blha_interface.F90')

openloops_version_src = 'version.F90'

# CutTools -- contains both, dp and qp routines
cuttools_dp_src = [
    'cts_combinatorics.f90', 'cts_constants.f90', 'cts_cutroutines.f90', 'cts_cuttools.f90',
    'cts_dynamics.f90', 'cts_kinematics.f90', 'cts_loopfunctions.f90', 'cts_tensors.f90',
    'cts_type.f90', 'mpnumdummy.f90']

# Collier
collier_inc_dp = ['checkparams_coli.h', 'common_coli.h', 'global_coli.h',
                  'params_coli.h']
collier_src_mp = []
collier_src_dp = [
    # Aux/
    'Combinatorics.F90', 'cache.F90', 'master.F90',
    # COLI/
    'coli_aux.F', 'coli_aux2.F90', 'coli_b0.F', 'coli_c0.F', 'coli_d0.F',
    'coli_d0reg.F', 'coli_stat.F90', 'reductionAB.F90', 'reductionC.F90',
    'reductionD.F90', 'reductionEFG.F90', 'reductionTN.F90',
    # DDlib/
    'DD_global.F90', 'DD_2pt.F', 'DD_3pt.F', 'DD_4pt.F',
    'DD_5pt.F', 'DD_6pt.F', 'DD_aux.F', 'DD_to_COLLIER.F',
    # dd-qp
    #'DD_global_qp.f90', 'DD_2pt_qp.f', 'DD_3pt_qp.f', 'DD_4pt_qp.f',
    #'DD_5pt_qp.f', 'DD_6pt_qp.f', 'DD_aux_qp.f', 'DD_interface_qp.f90',
    # tensors/
    'BuildTensors.F90', 'InitTensors.F90', 'TensorReduction.F90',
    # ./
    'COLLIER.F90', 'collier_aux.F90', 'collier_coefs.F90',
    'collier_global.F90', 'collier_init.F90', 'collier_tensors.F90']

# tr_red
tr_dp_src = ['b0.f90', 'c0_000.f90', 'c0_m00.f90', 'triangle_expansion.f90',
             'b0_mm.f90', 'c0_0mm.f90',  'c0_mmm.f90',  'triangle_aux.f90',
             'b0_m0m1.f90', 'c0_m0m1m1.f90',
             'triangle_reduction.f90', 'trred.f90']
tr_src_mp = []


if compile_libraries:
    cpp_container = CPPContainer(scons_cmd = scons_cmd,
                                 mp = config['precision'],
                                 version = release_version,
                                 process_api = config['process_api_version'],
                                 revision = git_revision,
                                 cpp_defs = cpp_defines,
                                 target = 'cpp_generic',
                                 target_prefix = os.path.join('..', 'obj', ''))

if 'olcommon' in compile_libraries:
    olcommon_lib = OLLibrary(
        name = 'olcommon',
        linklibs = ([] if sys.platform.startswith('freebsd') else ['dl']),
        target_dir = config['generic_lib_dir'],
        src_dir = lib_src_dirs['olcommon'],
        dp_src = olcommon_dp_src,
        mp_src = olcommon_mp_src,
        to_cpp = cpp_container)

if 'rambo' in compile_libraries:
    VariantDir(lib_obj_dirs['rambo'],
               lib_src_dirs['rambo'], duplicate = 0)
    rambo_lib = OLLibrary(name = 'rambo',
                          target_dir = config['generic_lib_dir'],
                          src_dir = lib_obj_dirs['rambo'],
                          dp_src = rambo_dp_src)

if 'qcdloop' in compile_libraries:
    VariantDir(lib_obj_dirs['qcdloop'],
               lib_src_dirs['qcdloop'], duplicate = 0)
    qcdloop_lib = OLLibrary(name = 'qcdloop',
                            target_dir = config['generic_lib_dir'],
                            src_dir = lib_obj_dirs['qcdloop'],
                            mod_dir = '',
                            cpp_paths = [lib_src_dirs['qcdloop']],
                            dp_src = qcdloop_dp_src)

if 'oneloop' in compile_libraries:
    VariantDir(lib_obj_dirs['oneloop'],
               lib_src_dirs['oneloop'], duplicate = 0)
    oneloop_lib = OLLibrary(name = 'oneloop',
                            target_dir = config['generic_lib_dir'],
                            mod_dependencies = ['olcommon'],
                            src_dir = lib_obj_dirs['oneloop'],
                            dp_src = oneloop_dp_src)

if 'cuttools' in compile_libraries:
    VariantDir(lib_obj_dirs['cuttools'],
               lib_src_dirs['cuttools'], duplicate = 0)
    cuttools_lib = OLLibrary(name = 'cuttools',
                             target_dir = config['generic_lib_dir'],
                             mod_dependencies = ['oneloop', 'olcommon'],
                             linklibs = [ll for ll in config['link_libraries']
                                         if ll == 'qcdloop'],
                             src_dir = lib_obj_dirs['cuttools'],
                             dp_src = cuttools_dp_src)

if 'collier' in compile_libraries:
    collier_lib = OLLibrary(name = 'collier',
                            target_dir = config['generic_lib_dir'],
                            mod_dependencies = ['olcommon'],
                            src_dir = lib_src_dirs['collier'],
                            mp_src = collier_src_mp,
                            dp_src = collier_src_dp,
                            to_cpp = cpp_container)

    # collier: preprocess include files, but don't add them to the list of source files
    cpp_container.add(src_dir = lib_src_dirs['collier'],
                      dp_src = collier_inc_dp)

if 'trred' in compile_libraries:
    trred_lib = OLLibrary(name = 'trred',
                          target_dir = config['generic_lib_dir'],
                          mod_dependencies = ['olcommon'],
                          src_dir = lib_src_dirs['trred'],
                          mp_src = tr_src_mp,
                          dp_src = tr_dp_src,
                          to_cpp = cpp_container)

    # collier: preprocess include files, but don't add them to the list of source files
    cpp_container.add(src_dir = lib_src_dirs['trred'],
                      dp_src = tr_dp_src)

if 'openloops' in compile_libraries:
    openloops_lib = OLLibrary(
        name = 'openloops',
        target_dir = config['generic_lib_dir'],
        mod_dependencies = sorted(list(set(config['link_libraries'])
            & set(['olcommon', 'collier', 'cuttools',
                   'oneloop', 'rambo', 'trred']))),
        linklibs = sorted(list(set(config['link_libraries']) & set(['rambo']))),
        src_dir = lib_src_dirs['openloops'],
        mp_src = openloops_mp_src,
        dp_src = openloops_dp_src,
        version_src = [openloops_version_src],
        to_cpp = cpp_container)

if '@all' in config['import_env']:
    imported_env = os.environ
else:
    imported_env = {}
    for envvar in config['import_env']:
        imported_env[envvar] = os.environ.get(envvar, '')

env = Environment(tools = ['default', 'textfile'] + [config['fortran_tool']],
                  ENV = imported_env,
                  CCFLAGS = config['ccflags'] + config['generic_optimisation'],
                  CXXFLAGS = config['cxxflags'],
                  FORTRANFLAGS = config['f77_flags'] + config['generic_optimisation'],
                  F90FLAGS = config['f90_flags'] + config['generic_optimisation'],
                  LINKFLAGS = config['link_flags'],
                  LIBPATH = [config['generic_lib_dir']],
                  DOLLAR = '\$$',
                  RPATH = [HashableLiteral('\$$ORIGIN')],
                  F90 = config['fortran_compiler'],
                  FORTRAN = config['fortran_compiler'],
                  CC = config['cc'])

if config['fortran_tool'] == 'gfortran':
    # SCons bug: FORTRANMODDIRPREFIX is missing in gfortran tool
    env.Replace(FORTRANMODDIRPREFIX = '-J')
    # determine gfortran version;
    # do not use CCVERSION, because it might not be from gcc
    gfort_exitcode = 1
    try:
        gfort_proc = subprocess.Popen(
            [config['fortran_compiler'], '-dumpversion'],
            stdout=subprocess.PIPE)
        gfort_out, gfort_err = gfort_proc.communicate()
        gfort_exitcode = gfort_proc.returncode
    except OSError:
        pass
    if not gfort_exitcode: # else ignore and continue without version check
        if tuple(map(int, gfort_out.decode('utf-8').strip().split('.')[:2])) < (4,6):
            print('ERROR: This OpenLoops version requires gfortran 4.6 ' +
                  'or later (found %s)' % env.subst('$CCVERSION'))
            Exit(1)

if compile_libraries:
    if not GetOption('clean'):
        if not cpp_container.run():
            print('*** cpp failed ***')
            Exit(1)

env_noautomatic = env.Clone()
env_noautomatic.AppendUnique(F90FLAGS = config['noautomatic'],
                             FORTRANFLAGS = config['noautomatic'])


if 'olcommon' in compile_libraries:
    libolcommon = olcommon_lib.compile(env = env, shared = config['shared_libraries'])
    env.Alias('olcommon', libolcommon)
    Default('olcommon')
    Clean(libolcommon, [lib_obj_dirs['olcommon'], lib_mod_dirs['olcommon']])

if 'rambo' in compile_libraries:
    librambo = rambo_lib.compile(env = env, shared = config['shared_libraries'])
    env.Alias('rambo', librambo)
    Default('rambo')
    Clean(librambo, [lib_obj_dirs['rambo'], lib_mod_dirs['rambo']])

if 'qcdloop' in compile_libraries:
    libqcdloop = qcdloop_lib.compile(env = env_noautomatic, shared = config['shared_libraries'])
    env.Alias('qcdloop', libqcdloop)
    Default('qcdloop')
    Clean(libqcdloop, [lib_obj_dirs['qcdloop']])

if 'oneloop' in compile_libraries:
    liboneloop = oneloop_lib.compile(env = env, shared = config['shared_libraries'])
    env.Alias('oneloop', liboneloop)
    Default('oneloop')
    Clean(liboneloop, [lib_obj_dirs['oneloop'], lib_mod_dirs['oneloop']])

if 'cuttools' in compile_libraries:
    libcuttools = cuttools_lib.compile(env = env_noautomatic, shared = config['shared_libraries'])
    env.Alias('cuttools', libcuttools)
    Default('cuttools')
    Clean(libcuttools, [lib_obj_dirs['cuttools'], lib_mod_dirs['cuttools']])

if 'trred' in compile_libraries:
    libtrred = trred_lib.compile(env = env, shared = config['shared_libraries'])
    env.Alias('trred', libtrred)
    Default('trred')
    Clean(libtrred, [lib_obj_dirs['trred'], lib_mod_dirs['trred']])

if 'collier' in compile_libraries:
    libcollier = collier_lib.compile(env = env, shared = config['shared_libraries'])
    env.Alias('collier', libcollier)
    Default('collier')
    Clean(libcollier, [lib_obj_dirs['collier'], lib_mod_dirs['collier']])

if 'openloops' in compile_libraries:
    libopenloops = openloops_lib.compile(env = env, shared = config['shared_libraries'])
    env.Alias('openloops', libopenloops)
    Default('openloops')
    Clean(libopenloops, [lib_obj_dirs['openloops'], lib_mod_dirs['openloops']])

if GetOption('clean') and compile_libraries:
    if not cpp_container.run(clean = True):
        print('*** cpp cleanup failed ***')
        Exit(1)



# ================= #
# Process libraries #
# ================= #


# parse process arguments
#   <loops>=proc1,proc2,coll/...
#   (several arguments possible, also with the same <loops>)
#   --> process_list = [(loops,proc1),(loops,proc2),(loops,collproc1),...]

version_db_url = (config['remote_process_url'] + '/%s/processes/' +
                  str(config['process_api_version']) + '/versions.db')
collection_url = config['remote_process_url'] + '/%s/collections'

def split_processlist(loops, procs):
    """Convert (loops=L, procs=P1,P2,P3,...) to [(L,P1),(L,P2),(L,P3),...].
    Replace collections (process ending with '/' or '.coll') by the list
    of processes from the collection file on the server."""
    proclist = sum([proclist.split(',') for proclist in procs.split()], [])
    proclist = [proc for proc in proclist if proc]
    collections = [coll[:-1] + '.coll' for coll in proclist if coll.endswith('/')]
    collections.extend([coll for coll in proclist if coll.endswith('.coll')])
    proclist = [(loops, proc) for proc in proclist
                if not (proc.endswith('/') or proc.endswith('.coll'))]
    for coll in collections:
        coll_repo = False
        for repo in config['process_repositories']:
            if coll == OLToolbox.repo_name(repo) + '.coll':
                coll_repo = repo
                break
        process_coll = []
        if coll == 'all.coll' or coll == 'public.coll':
            print('WARNING: you are about to download the entire process ',
                  'collection ', coll, '.', sep='')
            print('This is strongly discouraged.',
                  '>~15 GB disk space will be used.')
            print('Please consider installing a more specific collection or define your own')
            print('(list one processes per line in a .coll file).')
            if sys.stdout.isatty():
                if sys.version_info[0] == 2:
                    proceed = raw_input('Do you really want to proceed? (y/N) ')
                else:
                    proceed = input('Do you really want to proceed? (y/N) ')
                if proceed.lower().strip() != 'y':
                    print('Aborted.')
                    sys.exit(0)
        if coll == 'all.coll':
            for repo in config['process_repositories']:
                process_db = OLToolbox.ProcessDB(db=(version_db_url % repo))
                process_coll += process_db.content.keys()
        elif coll_repo:
            process_db = OLToolbox.ProcessDB(db=(version_db_url % coll_repo))
            process_coll += process_db.content.keys()
        else:
            found_collection = False
            first_repo = True
            for repo in config['process_repositories']:
                if first_repo:
                    # check if the collection is available locally
                    process_coll_add = OLToolbox.import_list(coll, fatal=False)
                else:
                    process_coll_add = None
                if process_coll_add is None:
                    # check if the collection is available in the repository
                    process_coll_add = OLToolbox.import_list(
                        os.path.join(collection_url % repo, coll), fatal=False)
                if process_coll_add is not None:
                    if not '<' in ''.join(process_coll_add):
                        # if it is not an html file with an error message
                        found_collection = True
                        process_coll.extend(process_coll_add)
                first_repo = False
            if not found_collection:
                print('ERROR: process collection ' + coll + ' not found.')
                Exit(1)
        proclist += [(loops, proc) for proc in process_coll]
    return proclist


def get_auto_loops(loops_processlib):
    """Determine 'auto' loops specifications and process API version
    from version.info in the process source directory."""
    loops = loops_processlib[0]
    processlib = loops_processlib[1]
    version_info = OLToolbox.import_dictionary(
        os.path.join(config['process_src_dir'], processlib, 'version.info'),
        fatal = False)
    process_api = -1
    if version_info:
        process_api = int(version_info['process_api_version'])
    if loops == 'auto':
        if not version_info:
            print('ERROR: auto loops specification not available for '
                  + processlib)
            Exit(1)
        loops = version_info['loops']
        if loops == 'auto' or loops not in OLBaseConfig.loops_specifications:
            print('ERROR: invalid loops specification for', processlib)
            Exit(1)
    return (loops, process_api, processlib)


def find_process_src(generate = True):
    """Find all generated / downloaded processes.
    if generate: return (loops,process) for all processes which exist
    (i.e. there is a directory which contains version.info)
    if only download (generate = False): return only those processes
    for which version.info contains 'hash'
    (i.e. they are controlled by the downloader)."""
    process_list = []
    if os.path.isdir(config['process_src_dir']):
        process_directories = os.listdir(config['process_src_dir'])
    else:
        return process_list

    for procdir in process_directories:
        version_info = OLToolbox.import_dictionary(
            os.path.join(config['process_src_dir'], procdir, 'version.info'),
            fatal = False)
        if version_info:
            loops = version_info['loops']
            process_hash = version_info.get('hash', None)
            if process_hash or generate:
                # download: only add if process_hash != None;
                # generate: always add
                process_list.append((loops, procdir))

    return process_list


def revoke_processes():
    """Revocation of deprecated processes.
    Remove processes (src, obj, lib) listed in 'revoke' on the server
    if the process source directory contains version.info with 'hash'."""
    revocation_list = OLToolbox.import_list(os.path.join(collection_url, 'revoke'), fatal = False)
    if revocation_list is None:
        revocation_list = []

    for proc in revocation_list:
        processlib_src_dir = os.path.join(config['process_src_dir'], proc)
        processlib_obj_dir = os.path.join(config['process_obj_dir'], proc)
        if os.path.isdir(processlib_src_dir):
            version_info = OLToolbox.import_dictionary(os.path.join(processlib_src_dir, 'version.info'), fatal = False)
            if version_info and 'hash' in version_info:
                print('revoking', proc)
                Execute(Delete(processlib_src_dir))
                if os.path.isdir(processlib_obj_dir):
                    Execute(Delete(processlib_obj_dir))
                revoke_libs = [os.path.join(config['process_lib_dir'], 'libopenloops_' + proc + '_' + lps + '.*')
                               for lps in OLBaseConfig.loops_specifications if lps != 'auto']
                revoke_libs = sum([Glob(patt) for patt in revoke_libs], [])
                if revoke_libs:
                    Execute(Delete(revoke_libs))


def download_processes(processes):
    """Download processes"""
    err = subprocess.call(
        [sys.executable, config['process_download_script']] +
        force_download_flag + processes +
        ['='.join(arg) for arg in commandline_options])
    if err:
        print('ERROR: process downloader failed.')
        Exit(1)


def generate_process(loops, processlib):
    """Generate a process library"""
    if subprocess.call(
          scons_cmd + ['-Q'] + generator_options +
          ['-f', config['code_generator_script'],
           'PROC=' + processlib, 'LOOPS=' + loops] +
          ['='.join(arg) for arg in commandline_options]) != 0:
        print('ERROR: code generator failed.')
        Exit(1)



process_list = sum([split_processlist(loops, procs) for (loops, procs) in process_arguments], [])

if config['process_update']:
    process_list.extend(find_process_src(generate_process_true))

if download_process_true:
    proc_ls = sorted(list(set([proc for loops, proc in process_list])))
    if proc_ls or config['process_update']:
        revoke_processes()
        download_processes(proc_ls)
        # Libraries may have been mapped to libraries with different names.
        # Read the library name mappings written by the downloader and
        # update proc_ls so that the build system knows what to compile.
        downloaded = OLToolbox.import_dictionary(
            os.path.join(config['process_src_dir'], 'downloaded.dat'))
        process_list = [(loops, downloaded.get(proc, proc))
                        for loops, proc in process_list]
        os.remove(os.path.join(config['process_src_dir'], 'downloaded.dat'))

process_list = map(get_auto_loops, process_list)

processes_seen = dict()
process_list_nodup = []
for (loops, process_api, processlib) in process_list:
    if processlib in processes_seen:
        if processes_seen[processlib] != loops:
            print('ERROR: cannot generate process library ' + processlib +
                  ' twice with different loop specification.')
            sys.exit(1)
    else:
        process_list_nodup.append((loops, process_api, processlib))
        processes_seen[processlib] = loops
process_list = process_list_nodup

env.Append(RPATH = [HashableLiteral('\$$ORIGIN/../lib')])


for (loops, process_api, processlib) in process_list:

    print('process library:', processlib + '_' + loops)

    # process library name and directories
    processlib_name = 'openloops_' + processlib.lower() + '_' + loops
    processlib_info = os.path.join(config['process_lib_dir'], 'lib' + processlib_name + '.info')
    processlib_src_dir = os.path.join(config['process_src_dir'], processlib)
    processlib_obj_dir = os.path.join(config['process_obj_dir'], processlib)

    # run the process code generator
    if generate_process_true:
        generate_process(loops, processlib)
    else:
        ol_api = config['process_api_version']
        if process_api != ol_api:
            print(('ERROR: Process API version {} of OpenLoops does not agree '
                  + 'with the API version {} of the process library {}.'
                  ).format(ol_api,process_api,processlib))
            Exit(1)

    # compile process
    if config['compile'] > 0 and not GetOption('clean'):

        # list of process library source files
        process_dp_src, process_mp_src, info_files = OLToolbox.get_processlib_src(
            loops, processlib, config)

        # prepend global libary info
        library_info_file = os.path.join(processlib_src_dir, 'info_' + processlib + '.txt')
        if os.path.isfile(library_info_file):
            info_files.insert(0, library_info_file)

        # set up process library source files for preprocessing
        mp = list(config['precision'])
        if config['process_qp_rescue']:
            mp.append('qp_rescue')
        if config['process_qp_checks']:
            mp.append('qp_checks')
        process_cpp_container = CPPContainer(
            scons_cmd = scons_cmd,
            mp = mp,
            cpp_defs = cpp_defines + [OLBaseConfig.loops_cppdefs[loopspec] for loopspec in loops],
            target = 'cpp_' + processlib,
            target_prefix = os.path.join('..', '..', processlib_obj_dir, ''))

        # set up process library
        process_lib = OLLibrary(name = processlib_name,
                                target_dir = config['process_lib_dir'],
                                # need to include oneloop mod dir for ifort
                                mod_dependencies = ['olcommon', 'openloops', 'oneloop'],
                                mod_dir = os.path.join(processlib_obj_dir, 'mod'),
                                mp_src = process_mp_src,
                                dp_src = process_dp_src,
                                to_cpp = process_cpp_container)

        # preprocess process library source files
        if not process_cpp_container.run():
            print('***', processlib, 'cpp failed ***')
            Exit(1)

        # delete all libraries for the process
        delete_libs = [os.path.join(config['process_lib_dir'],
                       'libopenloops_' + processlib.lower() + '_' + lps + '.*')
                       for lps in OLBaseConfig.loops_specifications if lps not in ('auto', loops)]
        delete_libs = sum([Glob(patt) for patt in delete_libs], [])
        if delete_libs:
            Execute(Delete(delete_libs))

        # compile process library
        libprocess = process_lib.compile(
            env = env,
            shared = config['shared_libraries'],
            env_mod = [
              ('^(virtual_\d|tensorsum_|loop_)',
               {'F90FLAGS': config['f90_flags'] + config['loop_optimisation']}),
              ('',
               {'F90FLAGS': config['f90_flags'] + config['born_optimisation']})]
        )
        # concatenate subprocess info files to a library info file
        libprocess_info = env.Substfile(processlib_info, info_files, LINESEPARATOR = '')
        libprocess += libprocess_info

        Alias('lib' + processlib, libprocess)
        Default('lib' + processlib)

    if GetOption('clean'):
        if config['compile'] > 0:
            # delete process object code
            Execute(Delete(processlib_obj_dir))
            delete_libs = [os.path.join(config['process_lib_dir'],
                        'libopenloops_' + processlib + '_' + lps + '.*')
                        for lps in OLBaseConfig.loops_specifications if lps != 'auto']
            delete_libs = sum([Glob(patt) for patt in delete_libs], [])
            if delete_libs:
                Execute(Delete(delete_libs))
        if 'src' in config['clean']:
            # delete process library source directory
            Execute(Delete(processlib_src_dir))



if GetOption('clean') and 'procs' in config['clean']:
    Execute(Delete(config['process_obj_dir']))
    Execute(Delete(config['process_lib_dir']))
    if 'src' in config['clean']:
        Execute(Delete(config['process_src_dir']))
