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
#    Definition of class Rivet
#-------------------------------------------------------------------------------

import os

from config import geneva_use_zlib
from .utils import format_tag, join_paths, get_filenames

if geneva_use_zlib:
   import gzip
   import io
   ext = ".hepmc.gz"
else:
   ext = ".hepmc"


class Rivet:
   """Handles the commands needed to run a Rivet analysis.

   Returns the commands required to run the analyze-rivet stage in the form.

      [[name1, executable1, [arguments1a, arguments1b, ...], [logfile1a, logfile1b, ...]],
       [name2, executable2, [arguments2a, arguments2b, ...], [logfile1a, logfile1b, ...]],
       ...
      ]

   where
      name       name of the substage
      executable command to be executed
      arguments  the arguments that each command must be run with
      logfile    the log file used for each command when the command list is
                 executed via the geneva <stage> machinery

   This implies that the following commands need to be run
      executable1 arguments1a
      executable1 arguments1b
      ...

      executable2 arguments2a
      executable2 arguments2b
      ...

      ...

   All commands of the same substage (same name) can be run in parallel, while
   the different substages must be run sequentially.
   """
   def getRivetCommand(self, first_seed, num_runs, filestem, analyses, last_seed, outpath, num_weights, rivet_tag, max_time, max_events):
      if last_seed == 0:
         last_seed = first_seed + num_runs//num_weights - 1
      if last_seed < first_seed:
         raise ValueError("Number of runs ({0}) is not enough to handle {1} weight(s).".format(num_runs, num_weights))
      infiles = get_filenames(filestem, ext, first_seed, last_seed)
      if num_weights*len(infiles) > num_runs:
         raise ValueError("Number of runs ({0}) is not enough to handle {1} files"
                          " each with {2} weight(s).".format(num_runs, len(infiles), num_weights))

      name = os.path.basename(filestem) + "_rivet"
      logPath = join_paths(outpath, "log")
      executable = "geneva-rivet-analyze"

      commonOptions = ""
      for a in analyses:
         commonOptions += " --add-analysis " + a

      if outpath:
         commonOptions += " --outpath " + outpath
      if rivet_tag:
         commonOptions += " --rivet-tag " + rivet_tag
      if max_time >= 1:
         commonOptions += " --max-time " + str(max_time)
      if max_events >= 1:
         commonOptions += " --max-events " + str(max_events)

      optionList = []
      logFileList = []
      seed = first_seed
      for filename in infiles:
         for w in range(1, num_weights + 1):
            optionList.append("--infile "+ filename + " --weight " + str(w) + commonOptions)
            logFileList.append(logPath + name + format_tag('w' + str(w)) + format_tag(seed) + ".log")
         seed += 1

      commandList = [[name, executable, optionList, logFileList]]
      return commandList

   def getAllCommands(self, args):
      """
      Get the list of commands
      """
      if args.stage == 'analyze-rivet':
         return self.getRivetCommand(args.start_seed, args.num_runs, args.filestem, args.analyses,
                                     args.last_seed, args.outpath, args.num_weights, args.rivet_tag, args.max_time, args.max_events)
      else:
         raise ValueError("Unknown Rivet stage: " + args.stage)
