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
#    Definition of class FixedOrder
#-------------------------------------------------------------------------------

from . import OptionHandler as opt
from .utils import format_tag, join_paths, get_filenames


class FixedOrder:
   """Generate the commands to run a fixed-order calculation.

   Returns a list of all commands required to run a given stage in the form

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

   #=======================================
   #              SETUP STAGE
   #=======================================
   def getSetupCommand(self, option_file, first_seed, num_runs, points, iterations, extra_options):
      opthandler = opt.OptionHandler(option_file, extra_options)
      nameStem = opthandler.getSetupStem() + "_setup"
      setupPath = opthandler.getSetupPath()
      logPath = join_paths(setupPath, "log")
      gridStem = setupPath + opthandler.getGridFileStem()
      assert(opthandler.needsIntegrationGrids())

      last_seed = first_seed + num_runs - 1

      # Get number of points per run from points and num_runs.
      pointsPerGrid = max(points // num_runs, 1)

      # Create the grids for the main calculation.
      nameGrids = nameStem + "_grids"
      executable = "geneva-main"
      executable_combine_grids = "geneva-setup-combine-grids"

      commonOptions = "--optionsFile " + option_file + " " + extra_options \
                      + " --writeReport false" \
                      + " --Analyzer None" \
                      + " --writeEvents false" \
                      + " --num 0" \
                      + " --EventGenerator Adaptive" \
                      + " --Mint::refineGrid true" \
                      + " --Mint::iterationsGrid 1" \
                      + " --Mint::maxPointsPerIterGrid " + str(pointsPerGrid) \
                      + " --FixedOrder::ScaleVariations none" \

      commandList = []
      for i in range(1, iterations + 1):
         # Create the grids for each iteration.
         name = nameGrids + "_iter" + str(i)
         optionList = []
         logFileList = []
         for seed in range(first_seed, last_seed + 1):
            optionList.append(commonOptions + " --seed " + str(seed))
            logFileList.append(logPath + name + format_tag(seed) + ".log")
         commandList.append([name, executable, optionList, logFileList])

         # Combine the grids for each iteration.
         name = nameGrids + "_combine" + "_iter" + str(i)
         optionList = [gridStem + " --plot-iteration " + str(i)
                       + " --first " + str(first_seed) + " --last " + str(last_seed)]
         logFileList = [logPath + name + ".log"]
         commandList.append([name, executable_combine_grids, optionList, logFileList])

      return commandList

   #=======================================
   #              CALCULATE STAGE
   #=======================================
   def getCalculateCommand(self, option_file, first_seed, num_runs, extra_options):
      opthandler = opt.OptionHandler(option_file, extra_options)
      generatePath = opthandler.getGeneratePath()
      logPath = join_paths(generatePath, "log")

      last_seed = first_seed + num_runs - 1

      name = opthandler.getGenerateStem() + "_calculate"
      executable = "geneva-main"

      commonOptions = "--optionsFile " + option_file + " " + extra_options \
                      + " --writeEvents false" \
                      + " --Mint::refineGrid false" \

      commandList = []
      optionList = []
      logFileList = []
      for seed in range(first_seed, last_seed + 1):
         optionList.append(commonOptions + " --seed " + str(seed))
         logFileList.append(logPath + name + format_tag(seed) + ".log")
      commandList.append([name, executable, optionList, logFileList])

      # Combine the analyzer files if required.
      name += "_combine_plots"
      executable = "geneva-combine-plots"
      if opthandler.useUnshoweredAnalyzer():
         reportStem = generatePath + opthandler.getUnshoweredReportStem()
         option = " ".join(get_filenames(reportStem, ".xml", first_seed, last_seed))
         option += " --outfile " + reportStem + ".xml"
         option += " --backup-path " + join_paths(generatePath, "backup-xml")
         optionList = [option]
         logFileList = [logPath + name + ".log"]
         commandList.append([name, executable, optionList, logFileList])

      # Combine the fixed-order analyzer files if required.
      if opthandler.useFixedOrderAnalyzer():
         name += "_fixed"
         reportStem = generatePath + opthandler.getFixedOrderReportStem()
         option = " ".join(get_filenames(reportStem, ".xml", first_seed, last_seed))
         option += " --outfile " + reportStem + ".xml"
         option += " --backup-path " + join_paths(generatePath, "backup-xml")
         optionList = [option]
         logFileList = [logPath + name + ".log"]
         commandList.append([name, executable, optionList, logFileList])

      return commandList

   def getAllCommands(self, args):
      """
      Get the list of commands
      """
      if args.stage == 'setup':
         return self.getSetupCommand(args.option_file, args.start_seed, args.num_runs,
                                     args.points, args.iterations, args.extra_options)
      elif args.stage == 'generate':
         return self.getCalculateCommand(args.option_file, args.start_seed, args.num_runs, args.extra_options)
      else:
         calcname=opt.OptionHandler(args.option_file, "").getValue("calc")
         raise ValueError("Calculation "+ calcname +" cannot handle stage "+args.stage+" !")
