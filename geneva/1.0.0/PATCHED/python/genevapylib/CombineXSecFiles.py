#-------------------------------------------------------------------------------
# Author(s):
#    Simone Alioli, Frank Tackmann
#
# Copyright:
#    Copyright (C) 2017, 2018 Geneva Collaboration
#
#    This file is part of the Geneva MC framework. Geneva is distributed under
#    the terms of the GNU General Public License version 3 (GPLv3), see the
#    COPYING file that comes with this distribution for details.
#    Please respect the academic usage guidelines in the GUIDELINES file.
#
# Description:
#    Function to combine setup cross sections
#-------------------------------------------------------------------------------

import os
import numpy as np

from . import process
from . import utils

#-------------------------------------------------------------------------------
# Private functions
#-------------------------------------------------------------------------------

def _write(outfile, num_events, weights, weights_squared):
   """Write num_events, weights, weights_squared to outfile."""
   process.log("Writing cross sections to file: " + outfile)
   with utils.open_outfile(outfile) as out:
      out.write(str(num_events) + '\n')
      for w in weights:
         out.write(str(w) + ' ')
      out.write('\n')
      for w2 in weights_squared:
         out.write(str(w2) + ' ')
      out.write('\n')

def _read(infile):
   """Read the number of events, weights, and weights squared from infile."""
   process.log("Reading cross sections from file: " + infile)
   with open(infile) as f:
      num = int(f.readline())
      w = np.fromstring(f.readline(), dtype = np.double, sep = ' ')
      w2 = np.fromstring(f.readline(), dtype = np.double, sep = ' ')
   return num, w, w2

def _combine_MPI(infiles, outfile):
   from mpi4py import MPI

   if process.rank < len(infiles):
      infile = infiles[process.rank]
      try:
         num, w, w2 = _read(infile)
      except Exception:
         process.signal_error("Could not read file: " + infile)
   else:
      num = 0
      w = np.zeros(6)
      w2 = np.zeros(6)

   if process.has_error():
      return

   num_events = process.mpi.reduce(num, op = MPI.SUM)
   weights = process.mpi.reduce(w, op = MPI.SUM)
   weights_squared = process.mpi.reduce(w2, op = MPI.SUM)

   if process.master:
      try:
         _write(outfile, num_events, weights, weights_squared)
      except Exception:
         process.signal_error("Could not write to file: " + outfile)

def _combine_serial(infiles, outfile):
   num_events = 0
   weights = np.zeros(6)
   weights_squared = np.zeros(6)
   for infile in infiles:
      try:
         num, w, w2 = _read(infile)
      except Exception:
         process.signal_error("Could not read file: " + infile)
         return

      num_events += num
      weights += w
      weights_squared += w2

   try:
      _write(outfile, num_events, weights, weights_squared)
   except Exception:
      process.signal_error("Could not write to file: " + outfile)

#-------------------------------------------------------------------------------
# Main public function
#-------------------------------------------------------------------------------

def combine(infiles, outfile, keep, backup_path):
   if process.mpi:
      process.check_size(len(infiles))
      _combine_MPI(infiles, outfile)
   else:
      _combine_serial(infiles, outfile)

   process.exit_if_error("Could not combine cross sections due to errors "
                         + "in one or more files.")

   if process.master and (backup_path or not keep):
      try:
         files = (i for i in set(infiles) if not os.path.samefile(outfile, i))
         if backup_path:
            process.log("Backing up files to: " + backup_path)
            utils.backup_files(files, backup_path, keep, "input file")
            utils.backup_files([outfile], backup_path, True, "output file")
         elif not keep:
            utils.remove_files(files, "input file")
      except Exception:
         process.signal_exit(1)

   return process.return_value()
