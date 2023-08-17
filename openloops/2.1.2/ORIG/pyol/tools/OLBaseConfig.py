#!******************************************************************************!
#! Copyright (C) 2014-2019 OpenLoops Collaboration. For authors see authors.txt !
#!                                                                              !
#! This file is part of OpenLoops.                                              !
#!                                                                              !
#! OpenLoops is free software: you can redistribute it and/or modify            !
#! it under the terms of the GNU General Public License as published by         !
#! the Free Software Foundation, either version 3 of the License, or            !
#! (at your option) any later version.                                          !
#!                                                                              !
#! OpenLoops is distributed in the hope that it will be useful,                 !
#! but WITHOUT ANY WARRANTY; without even the implied warranty of               !
#! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                !
#! GNU General Public License for more details.                                 !
#!                                                                              !
#! You should have received a copy of the GNU General Public License            !
#! along with OpenLoops.  If not, see <http://www.gnu.org/licenses/>.           !
#!******************************************************************************!


import os
import sys
if sys.version_info.major == 2:
    import ConfigParser as configparser
else:
    import configparser
import re

# prefix for default_config_file and user_config_file
prefix = globals().get('__file__', '')
if prefix:
    prefix = os.path.dirname(os.path.abspath(prefix)) # tools
    prefix = os.path.dirname(prefix) # pyol
    prefix = os.path.dirname(prefix)

default_config_file = os.path.join('pyol', 'config', 'default.cfg')
user_config_file = 'openloops.cfg'

loops_cppdefs = {'t': 'BORN', 'l': 'LOOP',
                 'p': 'PSEUDOTREE', 's': 'LOOPSQUARED'}

loops_specifications = ['auto', 't', 'l', 'lt', 'lp', 'ls',
                        'lpt', 'lst', 'lps', 'lpst']


def exit_error(err):
    print('CONFIG ERROR:', err)
    sys.exit(1)


def split_list(ls, converter=str):
    return list(map(converter, ls.replace(',',' ').split()))


def parse_bool(bl):
    if bl.lower() in ('y', 'yes', 'true', 't', '1', 'on', 'all'):
        return True
    elif bl.lower() in ('n', 'no', 'false', 'f', '0', 'off', 'none'):
        return False
    else:
        raise ValueError


def config_interpolate(config, opt):
    return re.sub(r'%\((\w+)\)s', lambda match: config[match.group(1)], opt)


def parse_option(config, opt, interpolate=False,
                 converter=str, one_of=None, subset_of=None):
    try:
        val = config[opt]
    except KeyError:
        exit_error('missing option: ' + opt)
    # remove comments
    val = val.split('#')[0].strip()
    if interpolate:
        val = config_interpolate(config, val)
    try:
        parsed_opt = converter(val)
    except ValueError:
        exit_error('invalid option value: ' + opt + ' = ' + config[opt])
    if one_of is not None:
        if parsed_opt not in one_of:
            exit_error('option \'' + opt + '\' must be one of ' +
                       ', '.join(map(str, one_of)))
    if subset_of is not None:
        if set(parsed_opt) - set(subset_of):
            exit_error('option \'' + opt + '\' must be a subset of ' +
                       ', '.join(map(str, subset_of)))
    config[opt] = parsed_opt


def get_config(args=[]):
    """
    Get configuration and return it as a dictionary.

    Read default configuration,
    override with user configurations if it exists,
    override with command line options from 'args'.
    args: a dictionary or a list of pairs
          (if the same option is given more than once,
           only the last set value is used)
    """
    config = configparser.SafeConfigParser()
    # default configuration
    with open(os.path.join(prefix, default_config_file), 'r') as fh:
        config.readfp(fh)
    # override with user configuration
    config.read([os.path.join(prefix, user_config_file)])
    config = dict(config.items('OpenLoops'))
    # override with command line options
    for key, val in dict(args).items():
        if key in config:
            config[key] = val
        else:
            exit_error('unknown option: ' + key)

    # parse options
    parse_option(config, 'num_jobs', converter=int)
    parse_option(config, 'supported_compilers', converter=split_list)
    parse_option(config, 'fortran_tool',
                 one_of=config['supported_compilers']+['auto'])
    parse_option(config, 'fortran_compiler')
    parse_option(config, 'cpp')
    parse_option(config, 'cc')
    parse_option(config, 'cxx')
    parse_option(config, 'ccflags', converter=split_list)
    parse_option(config, 'cxxflags', converter=split_list)
    parse_option(config, 'cmodel', one_of=['', 'small','medium','large'])
    parse_option(config, 'generator', converter=int, one_of=[0,1,2])
    parse_option(config, 'generator_mode', converter=int, one_of=[-1,0,1,2,3])
    parse_option(config, 'gjobs', converter=int)
    parse_option(config, 'compile', converter=int, one_of=[0,1,2])
    parse_option(config, 'compile_extra', converter=parse_bool)
    parse_option(config, 'glog', converter=parse_bool)
    parse_option(config, 'shared_libraries', converter=parse_bool)
    parse_option(config, 'interface', converter=int, one_of=[0,1,2])
    parse_option(config, 'precision', converter=split_list,
                 subset_of=['dp','qp'])
    parse_option(config, 'process_qp_rescue', converter=parse_bool)
    parse_option(config, 'process_qp_checks', converter=parse_bool)
    parse_option(config, 'expert_mode', converter=parse_bool)
    parse_option(config, 'max_string_length', converter=int)
    parse_option(config, 'compile_libraries', converter=split_list,
                 subset_of=['rambo', 'collier', 'cuttools', 'trred'])
    parse_option(config, 'link_libraries', converter=split_list,
                 subset_of=['rambo', 'collier', 'cuttools', 'trred'])
    parse_option(config, 'scalar_integral_libraries', converter=split_list,
                 subset_of=['qcdloop', 'oneloop'])
    parse_option(config, 'clean', converter=split_list,
                 subset_of=['procs', 'src'])
    parse_option(config, 'debug', converter=int, one_of=range(8))
    parse_option(config, 'print_python_version', converter=parse_bool)
    parse_option(config, 'math_flags', converter=split_list)
    parse_option(config, 'generic_optimisation', converter=split_list)
    parse_option(config, 'born_optimisation', converter=split_list)
    parse_option(config, 'loop_optimisation', converter=split_list)
    parse_option(config, 'link_optimisation', converter=split_list)
    parse_option(config, 'import_env', converter=split_list)
    parse_option(config, 'process_repositories', converter=split_list)
    parse_option(config, 'process_api_version', converter=int)
    parse_option(config, 'template_files', converter=split_list)
    parse_option(config, 'generator_files', converter=split_list)
    parse_option(config, 'force_download', converter=parse_bool)
    parse_option(config, 'process_update', converter=parse_bool)
    parse_option(config, 'pyrunoptions', converter=split_list)

    if config['fortran_tool'] == 'auto':
        if config['fortran_compiler'].startswith('ifort'):
            config['fortran_tool'] = 'ifort'
        else:
            config['fortran_tool'] = 'gfortran'

    config['auto_noautomatic'] = config[config['fortran_tool'] + '_noautomatic']
    config['auto_f77_flags'] = config[config['fortran_tool'] + '_f77_flags']
    config['auto_f90_flags'] = config[config['fortran_tool'] + '_f90_flags']
    config['auto_f_flags'] = config[config['fortran_tool'] + '_f_flags']
    config['auto_debug_flags_4'] = config[config['fortran_tool'] +
                                          '_debug_flags_4']
    parse_option(config, 'noautomatic', interpolate=True, converter=split_list)
    parse_option(config, 'f77_flags', interpolate=True, converter=split_list)
    parse_option(config, 'f90_flags', interpolate=True, converter=split_list)
    parse_option(config, 'f_flags', interpolate=True, converter=split_list)
    parse_option(config, 'common_flags', interpolate=True, converter=split_list)
    parse_option(config, 'link_flags', interpolate=True, converter=split_list)
    parse_option(config, 'debug_flags_1', converter=split_list)
    parse_option(config, 'debug_flags_4', interpolate=True,
                 converter=split_list)

    if config['num_jobs'] <= 0:
        import multiprocessing
        config['num_jobs'] = multiprocessing.cpu_count()

    if config['gjobs'] <= 0:
        import multiprocessing
        config['gjobs'] = multiprocessing.cpu_count()

    if 'dp' not in config['precision']:
        config['precision'] = config['precision'].append('dp')
        print('CONFIG: added \'dp\' to \'precision\'')

    if config['debug'] in (1,3,5,7):
        config['f_flags'].extend(config['debug_flags_1'])
        config['ccflags'].extend(config['debug_flags_1'])
    if config['debug'] in (2,3,6,7):
        config['generic_optimisation'] = ['-O0']
        config['born_optimisation'] = ['-O0']
        config['loop_optimisation'] = ['-O0']
        config['link_optimisation'] = ['-O0']
    if config['debug'] in (4,5,6,7):
        config['f_flags'].extend(config['debug_flags_4'])
    if sys.platform.startswith('linux') and config['cmodel']:
        config['f_flags'].append('-mcmodel=' + config['cmodel'])

    if len(config['release']) > 8:
        exit_error('option \'release\' must have at most 8 characters')

    config['f77_flags'] = config['f77_flags'] + config['f_flags']
    config['f90_flags'] = config['f90_flags'] + config['f_flags']
    config['link_flags'] = config['link_flags'] + config['link_optimisation']

    config['compile_libraries'].extend(['olcommon', 'openloops', 'trred'])
    config['link_libraries'].extend(['olcommon', 'openloops', 'trred'])
    if ('cuttools' in config['compile_libraries']):
        for lib in config['scalar_integral_libraries']:
            if lib not in config['compile_libraries']:
                config['compile_libraries'].append(lib)
    if ('cuttools' in config['link_libraries']):
        for lib in config['scalar_integral_libraries']:
            if lib not in config['link_libraries']:
                config['link_libraries'].append(lib)

    config['generator_dependencies'] = (
       [config['code_generator_prg']]
     + [os.path.join(config['model_dir'], mod + '.m') # model files
        for mod in ['SM']]
     + [os.path.join(config['template_dir'], tmpl + '.mf') # template files
        for tmpl in config['template_files']]
     + [os.path.join(config['generator_dir'], gen + '.m') # generator files
        for gen in config['generator_files']])

    return config
