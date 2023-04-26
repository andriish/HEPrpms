#!/usr/bin/env python

# examples/runtest.py
#
# This file is part of NJet library
# Copyright (C) 2011, 2012, 2013 NJet Collaboration
#
# This software is distributed under the terms of the GNU General Public License (GPL)

import getopt
import itertools
import os
import re
import signal
import sys
import time
from functools import cmp_to_key
from math import pi, sqrt

import testdata

signal.signal(signal.SIGINT, signal.SIG_DFL)

# import njet

try:  # for python3
    from importlib.machinery import SourceFileLoader

    njet = SourceFileLoader('njet', os.path.join(os.path.dirname(__file__), '../blha/njet.py')).load_module()
except ImportError:  # for python2
    import imp

    njet = imp.load_source('njet', os.path.join(os.path.dirname(__file__), '../blha/njet.py'))

OLP = njet.OLP

DEBUG = False
SLCTEST = None
CCTEST = None
NPOINTS = 100
VIEW = 'NJ'
if sys.platform.startswith('linux'):
    LIBNJET = os.path.join(os.path.dirname(__file__), '../.libs/libnjet3.so')
elif sys.platform.startswith('darwin'):
    LIBNJET = os.path.join(os.path.dirname(__file__), '../.libs/libnjet3.dylib')
else:
    print("Warning: unknown system '%s'. Library will probably fail to load." % sys.platform)
    LIBNJET = os.path.join(os.path.dirname(__file__), '../.libs/libnjet3.dll')

factorial = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800]  # n!

ORDER_TPL = """
# fixed
CorrectionType          QCD
BLHA1TwoCouplings       yes

# changable
#Extra NJetMultiPrec 2
#Extra Precision 1e-2
Extra NJetPrintStats yes

# test-specific
%s

# process list
"""


def njet_init(order):
    if os.path.exists(LIBNJET):
        status = OLP.OLP_Start(order, libnjet=LIBNJET)
    else:
        print("Warning: '%s' not found, trying system default." % LIBNJET)
        status = OLP.OLP_Start(order)
    if DEBUG:
        if status:
            print(OLP.contract)
        else:
            print(order)
    return status


def relerr(a, b):
    if abs(a + b) != 0.:
        return abs(2. * (a - b) / (a + b))
    else:
        return abs(a - b)


def out_vals(pref, norm1, val1, norm2, val2, onlyus=False):
    err = 0
    if not onlyus:
        err = relerr(val1, val2)
    if VIEW == 'MC':
        vals = (pref, val1, val2, err)
    else:
        vals = (pref, val1 / norm1, val2 / norm2, err)
    if onlyus:
        print("%6s %25.15e" % vals[:2])
    else:
        print("%6s %25.15e %25.15e %25.15e" % vals)


def pretty_print_results(p, j, params, rval, onlyus=False, mode=None):
    if mode is None:
        mode = params.get('mode', 'PLAIN')
    alphas = params.get('as', 1.)
    ao2pi = 4. * pi * alphas / (8. * pi * pi)

    ep2, ep1, ep0, born = rval[:4]
    tborn = p['born'][j]
    tep2, tep1, tep0 = p['loop'][j]

    norm = ao2pi * born
    tnorm = ao2pi * tborn
    if mode == 'ML5':
        tep2, tep1, tep0 = tnorm * tep2, tnorm * tep1, tnorm * tep0
    elif mode == 'NJ1':
        norm = born
        tnorm = tborn
        tep2, tep1, tep0 = tborn * tep2, tborn * tep1, tborn * tep0
    elif mode == 'LI':
        norm = 1.
        tnorm = 1.
        # tep0 = tep0/(8.*pi*pi)

    out_vals("A0", 1, born, 1, tborn, onlyus)
    out_vals("A1_2", norm, ep2, tnorm, tep2, onlyus)
    out_vals("A1_1", norm, ep1, tnorm, tep1, onlyus)
    out_vals("A1_0", norm, ep0, tnorm, tep0, onlyus)
    if DEBUG and len(rval) == 7:
        out_vals("ACC_2", 1., rval[4], 1., 0, True)
        out_vals("ACC_1", 1., rval[5], 1., 0, True)
        out_vals("ACC_0", 1., rval[6], 1., 0, True)


def pretty_print_time(times):
    diffs = [x - y for (x, y) in zip(times[1:], times[:-1])]
    diffs2 = [x * x for x in diffs]
    n = len(diffs)
    avg = sum(diffs) / n
    if n > 1:
        sig = sqrt((sum(diffs2) - n * avg * avg) / (n - 1))
        print("Time: %.4f (%.2f%%) s" % (avg, 100 * sig / avg))
    else:
        print("Time: %.4f s" % avg)


# get permutation order for limited permutations
def get_perm_order(dsn, dslimit):
    p = list(range(1, factorial[dsn] + 1))
    if len(p) != dslimit:
        tmp = list(itertools.permutations(list(range(dsn))))
        p = [tuple(reversed(j)) > j and i or i + len(p) for i, j in zip(p, tmp)]
    return p


# get copy of a lst with swapped elements s0 and s1
def get_swap2(lst, s0, s1):
    rval = list(lst)
    rval[s0], rval[s1] = rval[s1], rval[s0]
    return rval


# get a list of permutations of mom, from ds0 to ds1 + symmetrisation of sym2
def get_dsmoms(mom, ds0, ds1, perm_order, sym2):
    symfac = 1
    dsmoms = (mom[:ds0] + list(x) + mom[ds1:] for x in itertools.permutations(sorted(mom[ds0:ds1])))
    dsmoms = [m for o, m in sorted(zip(perm_order, dsmoms))]
    if sym2:
        s0, s1 = sym2
        dsmoms.extend([get_swap2(x, s0 - 1, s1 - 1) for x in dsmoms])
        symfac = 2
    return dsmoms, symfac


# get a pretty name for a point
def get_name(j, dsn, dslimit, sym2):
    s = "... point %d ..." % j
    if dsn > 1:
        s += " %d! (%d permutations)" % (dsn, dslimit)
    if sym2:
        s += " x%d" % len(sym2)
    return s


# call OLP to get values, averages over permutations
def get_vals(moms, mcn, **kwargs):
    rvals = []
    avg = 0
    for m in moms:
        avg += 1
        tmpval = OLP.OLP_EvalSubProcess(mcn, m, **kwargs)
        rvals.append(tmpval)
    rval = list(map(sum, list(zip(*rvals))))
    return [x / avg for x in rval]


def run_generic_test(mom, params, data):
    alphas = params.get('as', 1.)
    alpha = params.get('ae', 1.)
    mur = params.get('mur', 1.)
    # mode = params.get('mode', 'PLAIN')
    for p in data:
        mcn = p['mcn']

        dsn = p.get('dsn', (1, 1))
        ds0, ds1 = dsn[0] - 1, dsn[1]
        dsn = ds1 - ds0
        dslimit = p.get('dslimit', factorial[dsn])
        perm_order = get_perm_order(dsn, dslimit)
        sym2 = p.get('sym2', None)

        if 'name' in p:
            name = p['name']
        else:
            name = repr(p['inc'] + p['out'])

        npoints = min(NPOINTS, len(mom))
        print("-------- channel %s -------- (%d points)" % (name, npoints))
        times = [time.time()]
        for j in range(npoints):
            # mj = mom[j]
            dsmoms, symfac = get_dsmoms(mom[j], ds0, ds1, perm_order, sym2)
            allmoms = dsmoms[:(dslimit * symfac)]
            print(get_name(j, dsn, dslimit, sym2))

            if not SLCTEST or SLCTEST == 'both' or not p.get('has_lc', None):
                rval = get_vals(allmoms, mcn, alphas=alphas, alpha=alpha, mur=mur)
                pretty_print_results(p, j, params, rval)
            if SLCTEST and p.get('has_lc', None):
                print("+++ LC+SLC +++")
                rval_lc = get_vals(allmoms, mcn + 1, alphas=alphas, alpha=alpha, mur=mur)
                rval_slc = get_vals(allmoms, mcn + 2, alphas=alphas, alpha=alpha, mur=mur)
                rval_sum = list(map(sum, list(zip(rval_lc, rval_slc))[:4]))
                rval_sum[3] = rval_sum[3] / 2
                pretty_print_results(p, j, params, rval_sum)
                if DEBUG:
                    print("*** LC/(LC+SLC) ***")
                    rval_lc_r = [x / y if y != 0 else 1 for x, y in zip(rval_lc, rval_sum)]
                    pretty_print_results(p, j, params, rval_lc_r, onlyus=True, mode='NJ1')
                    print("*** SLC/(LC+SLC) ***")
                    rval_slc_r = [x / y if y != 0 else 1 for x, y in zip(rval_slc, rval_sum)]
                    pretty_print_results(p, j, params, rval_slc_r, onlyus=True, mode='NJ1')
                    if SLCTEST == 'both':
                        rval_sum_r = [x / y if y != 0 else 1 for x, y in zip(rval_sum, rval)]
                        if all(abs(x - 1) < 1e-13 for x in rval_sum_r):
                            s = 'OK'
                        else:
                            s = 'FAIL'
                        print("*** (LC+SLC)/FULL *** %s" % s)
                        pretty_print_results(p, j, params, rval_sum_r, onlyus=True, mode='NJ1')
            times.append(time.time())
        pretty_print_time(times)


def nis(i, j):
    return i + j * (j - 1) / 2 if i <= j else j + i * (i - 1) / 2


def run_cc_test(mom, params, data):
    alphas = params.get('as', 1.)
    alpha = params.get('ae', 1.)
    mur = params.get('mur', 1.)
    # mode = params.get('mode', 'PLAIN')
    legs = len(mom[0])
    for p in data:
        mcn = p['mcn']
        if 'name' in p:
            name = p['name']
        else:
            name = repr(p['inc'] + p['out'])
        npoints = min(NPOINTS, len(mom))
        print("-------- channel %s -------- (%d points)" % (name, npoints))
        for j in range(npoints):
            print("... point %d ..." % j)
            ccvals = OLP.OLP_EvalSubProcess(mcn + 1, mom[j], alphas=alphas, alpha=alpha, mur=mur,
                                            retlen=legs * (legs - 1) / 2)
            treeval = OLP.OLP_EvalSubProcess(mcn + 2, mom[j], alphas=alphas, alpha=alpha, mur=mur)
            born = treeval[0]
            if relerr(p['born'][j], born) > 1e-10:
                msg = 'FAIL'
            else:
                msg = 'OK'
            if born == 0:
                print("ERROR born = 0")
                born = 1
            print(p['born'][j] / born, msg)
            for i in range(legs):
                xsum = 0
                for k in range(legs):
                    if i == k:
                        sys.stdout.write(" %10.3e" % 0.)
                    else:
                        x = ccvals[nis(i, k)] / born
                        sys.stdout.write(" %10.3e" % x)
                        xsum += x
                sys.stdout.write(" | %17.10e\n" % xsum)


def run_sc_test(mom, params, data):
    alphas = params.get('as', 1.)
    alpha = params.get('ae', 1.)
    mur = params.get('mur', 1.)
    # mode = params.get('mode', 'PLAIN')
    legs = len(mom[0])
    for p in data:
        mcn = p['mcn']
        if 'name' in p:
            name = p['name']
        else:
            name = repr(p['inc'] + p['out'])
        npoints = min(NPOINTS, len(mom))
        print("-------- channel %s -------- (%d points)" % (name, npoints))
        for j in range(npoints):
            print("... point %d ..." % j)
            scvals = OLP.OLP_EvalSubProcess(mcn + 1, mom[j], alphas=alphas, alpha=alpha, mur=mur,
                                            retlen=2 * legs * legs)
            treeval = OLP.OLP_EvalSubProcess(mcn + 2, mom[j], alphas=alphas, alpha=alpha, mur=mur)
            born = treeval[0]
            if relerr(p['born'][j], born) > 1e-10:
                msg = 'FAIL'
            else:
                msg = 'OK'
            if born == 0:
                print("ERROR born = 0")
                born = 1
            print(p['born'][j] / born, msg)
            for i in range(legs):
                for k in range(legs):
                    xr, xi = scvals[2 * (i + k * legs)], scvals[2 * (i + k * legs) + 1]
                    sys.stdout.write(" (%10.3e, %10.3e)" % (xr, xi))
                sys.stdout.write("\n")


def chan_has_lc(p):
    channel = njet.Channel([njet.Process.cross_flavour(i) for i in p['inc']] + p['out'])
    chanmatches = njet.Process.canonical.get(channel.canon_list, None)
    if chanmatches:
        return chanmatches[0].has_lc
    return False


def add_to_order(mcn, order, test):
    new = ["\n"]
    params = test['params']
    if CCTEST:
        params['type'] = CCTEST
    ptype = params.get('type', 'NORMAL')
    new.append("AlphasPower %d" % params.get('aspow', 0))
    new.append("AlphaPower  %d" % params.get('aepow', 0))
    if ptype == 'DS':
        new.append("AmplitudeType LoopDS")
    elif ptype.lower() in ['loophq']:
        new.append("AmplitudeType %s" % ptype)
    else:
        new.append("AmplitudeType Loop")
    new.append(params.get('order', ''))
    for p in test['data']:
        mcn += 1
        p['mcn'] = mcn
        procline = "%s -> %s" % (' '.join(map(str, p['inc'])), ' '.join(map(str, p['out'])))
        new.append(procline)
        if ptype == 'NORMAL':
            p['has_lc'] = chan_has_lc(p)
            if p['has_lc']:
                new.append("Process %d AmplitudeType LoopLC" % (mcn + 1))
                new.append(procline)
                new.append("Process %d AmplitudeType LoopSLC" % (mcn + 2))
                new.append(procline)
                mcn += 2
        elif ptype == 'CC':
            new.append("Process %d AmplitudeType ccTree" % (mcn + 1))
            new.append(procline)
            new.append("Process %d AmplitudeType Tree" % (mcn + 2))
            new.append(procline)
            mcn += 2
        elif ptype == 'SC':
            new.append("Process %d AmplitudeType scTree" % (mcn + 1))
            new.append(procline)
            new.append("Process %d AmplitudeType Tree" % (mcn + 2))
            new.append(procline)
            mcn += 2

    order += '\n'.join(new)
    return mcn, order


def run_batch(curorder, curtests):
    if DEBUG:
        curorder = "NJetReturnAccuracy 2\n" + curorder
    order = ORDER_TPL % curorder
    mcn = 0
    seen = []
    for t in curtests:
        test = t['test']
        if test not in seen:
            mcn, order = add_to_order(mcn, order, test)
            seen.append(test)
    if not njet_init(order):
        print("Skipping batch due to errors")
        return

    for t in curtests:
        proc = t['proc']
        mom = t['mod'].momenta
        test = t['test']
        params = test['params']
        if proc:
            data = [d for d in test['data'] if d['name'] == proc]
        else:
            data = test['data']
        if not data:
            print("Warning: can't find %s" % proc)
            continue
        print("============== tests %s from %s ==============" % (repr(t['testname']), t['mod'].__name__))
        ptype = params.get('type', 'NORMAL')
        if ptype == 'DS':
            run_generic_test(mom, params, data)
        elif ptype == 'CC':
            run_cc_test(mom, params, data)
        elif ptype == 'SC':
            run_sc_test(mom, params, data)
        else:
            run_generic_test(mom, params, data)


def run_tests(mods, tests):
    cmporder_tmp = [order_global(m) for m in mods]

    def cmporder(x, y):
        return cmp(cmporder_tmp.index(x[0]), cmporder_tmp.index(y[0]))

    sortmods = sorted([(order_global(m), m) for m in mods], key=cmp_to_key(cmporder))
    curorder = ''
    curtests = []
    for order, m in sortmods:
        if order != curorder:
            if curorder:
                run_batch(curorder, curtests)
            curorder = order
            curtests = [t for t in tests if t['mod'] == m]
        else:
            curtests.extend([t for t in tests if t['mod'] == m])
    run_batch(curorder, curtests)


def order_global(mod):
    order = ['IRregularisation %s' % mod.scheme]
    if mod.renormalized:
        order.append('Extra NJetRenormalize yes')
    else:
        order.append('Extra NJetRenormalize no')
    order.append('Extra SetParameter qcd(nf) %d' % mod.Nf)
    order.append(mod.extraorder)
    order = '\n'.join(order).rstrip(' \n')
    order = re.sub(r'\n\n+', r'\n', order)
    order = re.sub(r'\s\s+', r' ', order)
    return order


def usage():
    print("""\
Usage: runtests.py [OPTION...]
Run selected tests

  -l, --list                list available tests [default]
  -r, --run=test1,test2     run selected tests
  -n, --npoints=<N>         limit number of test points
  -v, --view=MC,NJ          normalization of the output
  -s, --slc=yes,both        run tests in lc/slc-mode
  -d, --debug               print debug information

  --lib=<path/file.so>      full path to NJet library
  --cc                      print colour-correlated trees
  --sc                      print spin-correlated trees

Other options:
  -h, --help                show this help message
""")


def action_list(param):
    print("========== Available tests ==========")
    for name in sorted(testdata.tests):
        test = getattr(testdata, name)
        print("'%s' %s -- %s" % (name, repr(test.groups), test.info))


def action_run(param):
    testsarr = param.tests.split(',')
    tests = []
    mods = []
    for t in testsarr:
        modname, testname, proc = (t.split(':') + ['', ''])[:3]
        m = getattr(testdata, modname, None)
        if not m:
            print("Warning: unknown test '%s'" % modname)
            continue
        if testname:
            t = getattr(m, testname, None)
            if not t:
                print("Warning: unknown test '%s:%s'" % (modname, testname))
                continue
            testname = [testname]
        else:
            testname = m.groups
        if m not in mods:
            mods.append(m)
        for u in testname:
            tests.append({'mod': m,
                          'test': getattr(m, u),
                          'testname': u,
                          'proc': proc})
    if not mods:
        print("Warning: specifiy tests to run")
        return
    run_tests(mods, tests)


class Params:
    def __init__(self):
        global NPOINTS, VIEW, DEBUG, SLCTEST, CCTEST, LIBNJET
        try:
            opts, args = getopt.getopt(sys.argv[1:], "lr:n:v:s:dh",
                                       ["list", "run=", "npoints=", "view=",
                                        "slc=", "lib=", "debug", "cc", "sc", "help"])
        except getopt.GetoptError as err:
            print(str(err))
            usage()
            sys.exit(2)

        self.action = []
        self.tests = ''

        for op, oparg in opts:
            if op in ("-h", "--help"):
                usage()
                sys.exit()
            elif op in ("-l", "--list"):
                self.action.append('list')
            elif op in ("-v", "--view"):
                VIEW = oparg
            elif op in ("-s", "--slc"):
                SLCTEST = oparg
            elif op in ("-d", "--debug"):
                DEBUG = True
            elif op in "--cc":
                CCTEST = 'CC'
            elif op in "--sc":
                CCTEST = 'SC'
            elif op in "--lib":
                LIBNJET = oparg
            elif op in ("-n", "--npoints"):
                try:
                    NPOINTS = int(oparg)
                    assert NPOINTS > 0
                except ValueError or AssertionError:
                    print("Error: npoints must be a positive integer")
                    sys.exit(2)
            elif op in ("-r", "--run"):
                self.action.append('run')
                self.tests = oparg
            else:
                assert False, "unhandled option"

        if VIEW not in ['MC', 'NJ']:
            print("Error: --view can be 'MC' or 'NJ'")
            sys.exit(2)

        if SLCTEST not in ['yes', 'both', None]:
            print("Error: --slc can be 'yes' or 'both'")
            sys.exit(2)

        if len(self.action) > 1:
            print("Error: 'list' and 'run' are mutually exclusive")
        if len(self.action) != 1:
            usage()
            sys.exit(2)

        self.action = ['action_' + x for x in self.action]


def main():
    param = Params()
    globals()[param.action[0]](param)


if __name__ == '__main__':
    main()
