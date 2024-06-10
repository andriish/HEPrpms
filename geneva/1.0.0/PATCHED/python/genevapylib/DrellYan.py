#-------------------------------------------------------------------------------
# Author(s):
#    Christian Bauer, Simone Alioli, Frank Tackmann
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
#    Definition of class DrellYan
#-------------------------------------------------------------------------------

from . import OptionHandler as opt
from . import Lhef as lhef
from .utils import format_tag, format_path, join_paths, get_filenames


class DrellYan:
   """Generate the commands to run the resummed Drell-Yan calculation.

   Returns a list of all commands required to run a given stage for Drell-Yan
   production (the CalcSCETppV012 calculation) in the form

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
      pointsXSSetup = max(pointsPerGrid // 50, 1)
      pointsXSSetupGrids = max(pointsXSSetup // 5, 1)

      commandList = []

      # Create the splitting function grids. Done on a single core.
      name = nameStem + "_splitting"
      executable = "geneva-main"

      commonOptions = "--optionsFile " + option_file + " " + extra_options \
                      + " --writeReport false" \
                      + " --Analyzer None" \
                      + " --writeEvents false" \
                      + " --num 0" \
                      + " --EventGenerator Simple" \
                      + " --CalcSCETppV012::ImprovedXSFix false" \

      optionList = [commonOptions]
      logFileList = [logPath + name + ".log"]
      commandList.append([name, executable, optionList, logFileList])

      # Create the grid for the setup calculation for the alpha factors.
      nameAlphaGrids = nameStem + "_profile_grid"
      alphaGridStem = gridStem + format_tag("mult_5")
      executable = "geneva-setup-xsec"
      executable_combine_grids = "geneva-setup-combine-grids"

      commonOptions = "--optionsFile " + option_file + " " + extra_options \
                      + " --writeReport false" \
                      + " --Analyzer None" \
                      + " --writeEvents false" \
                      + " --num 0" \
                      + " --EventGenerator Adaptive" \
                      + " --EventGenerator::OnlySignatureSize 5" \
                      + " --IntegrationChannels::separation Multiplicity" \
                      + " --Mint::refineGrid true" \
                      + " --Mint::iterationsGrid 1" \
                      + " --Mint::maxPointsPerIterGrid " + str(pointsXSSetupGrids) \
                      + " --CalcSCETppV012::scale murun" \
                      + " --FixedOrder::ScaleVariations none" \
                      + " --CalcSCETppV012::run1 none" \
                      + " --CalcSCETppV012::DebugAdditive::IncludeFixedOrder false" \
                      + " --CalcSCETppV012::DebugAdditive::IncludeResum true" \
                      + " --CalcSCETppV012::DebugAdditive::IncludeResummExpanded false" \
                      + " --CalcSCETppV012::ImprovedXSFix::needsSetup true" \

      for i in range(1, iterations + 1):
         # Create the grids for each iteration.
         name = nameAlphaGrids + "_iter" + str(i)
         optionList = []
         logFileList = []
         for seed in range(first_seed, last_seed + 1):
            optionList.append(commonOptions + " --seed " + str(seed))
            logFileList.append(logPath + name + format_tag(seed) + ".log")
         commandList.append([name, executable, optionList, logFileList])

         # Combine the grids for each iteration.
         name = nameAlphaGrids + "_combine" + "_iter" + str(i)
         optionList = [alphaGridStem + " --first " + str(first_seed)
                       + " --last " + str(last_seed)]
         logFileList = [logPath + name + ".log"]
         commandList.append([name, executable_combine_grids, optionList, logFileList])

      # Create the setup files for the alpha factors.
      name = nameStem + "_profile"
      executable = "geneva-setup-xsec"

      commonOptions = "--optionsFile " + option_file + " " + extra_options \
                      + " --writeReport false" \
                      + " --Analyzer None" \
                      + " --writeEvents false" \
                      + " --num " + str(pointsXSSetup) \
                      + " --EventGenerator Adaptive" \
                      + " --EventGenerator::OnlySignatureSize 5" \
                      + " --IntegrationChannels::separation Multiplicity" \
                      + " --Mint::refineGrid false" \
                      + " --CalcSCETppV012::scale murun" \
                      + " --FixedOrder::ScaleVariations none" \
                      + " --CalcSCETppV012::run1 none" \
                      + " --CalcSCETppV012::DebugAdditive::IncludeFixedOrder false" \
                      + " --CalcSCETppV012::DebugAdditive::IncludeResum true" \
                      + " --CalcSCETppV012::DebugAdditive::IncludeResummExpanded false" \
                      + " --CalcSCETppV012::ImprovedXSFix::needsSetup true" \

      optionList = []
      logFileList = []
      for seed in range(first_seed, last_seed + 1):
         optionList.append(commonOptions + " --seed " + str(seed))
         logFileList.append(logPath + name + format_tag(seed) + ".log")
      commandList.append([name, executable, optionList, logFileList])

      # Combine the setup files for the alpha factors and remove intermediate grids.
      name += "_combine"
      executable = "geneva-setup-combine-xsec"

      xsecStem = setupPath + opthandler.getXSFileStem()
      option = " ".join(get_filenames(xsecStem, ".dat", first_seed, last_seed))
      option += " --outfile " + xsecStem + ".dat"
      option += " --remove " + alphaGridStem + ".dat"
      optionList = [option]
      logFileList = [logPath + name + ".log"]
      commandList.append([name, executable, optionList, logFileList])

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
                      + " --CalcSCETppV012::scale murun" \
                      + " --FixedOrder::ScaleVariations none" \

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
   #              GENERATE STAGE
   #=======================================
   def getGenerateCommand(self, option_file, first_seed, num_runs, extra_options):
      opthandler = opt.OptionHandler(option_file, extra_options)
      generatePath = opthandler.getGeneratePath()
      logPath = join_paths(generatePath, "log")

      last_seed = first_seed + num_runs - 1

      name = opthandler.getGenerateStem() + "_generate"
      executable = "geneva-main"

      commonOptions = "--optionsFile " + option_file + " " + extra_options \
                      + " --writeEvents true" \
                      + " --LHEF::normalizeWeights false" \
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

   #=======================================
   #              REWEIGHT STAGE
   #=======================================
   def getReweightCommand(self, option_file, first_seed, num_runs, sigma_below):
      opthandler = opt.OptionHandler(option_file, "")
      generatePath = opthandler.getGeneratePath()
      generateStem = opthandler.getGenerateStem()
      lheStem = generatePath + generateStem
      logPath = join_paths(generatePath, "log")

      last_seed = first_seed + num_runs - 1

      name = generateStem + "_reweight"
      executable = "geneva-lhef-reweight"

      option = " ".join(get_filenames(lheStem, lhef.ext, first_seed, last_seed))
      option += " --target {0[0]} {0[1]} {0[2]}".format(sigma_below)
      option += " --multiplicity 4 --num-weights 11 --up-weight 10 --down-weight 11"
      optionList = [option]
      logFileList = [logPath + name + ".log"]
      commandList = [[name, executable, optionList, logFileList]]
      return commandList

   #=======================================
   #              ANALYZE LHEF STAGE
   #=======================================
   def getAnalyzeLhefCommand(self, option_file, first_seed, num_runs, outpath,
                             max_time, max_events, no_enforce_lhef_options):
      opthandler = opt.OptionHandler(option_file, "")
      generateStem = opthandler.getGenerateStem()
      lheStem = opthandler.getGeneratePath() + generateStem
      logPath = join_paths(outpath, "log")

      last_seed = first_seed + num_runs - 1

      name = generateStem + "_analyze"
      executable = "geneva-lhef-analyze"

      # Create options common to all lhe files.
      commonOptions = "--option-file " + option_file
      if outpath:
         commonOptions += " --outpath " + outpath
      if max_time >= 1:
         commonOptions += " --max-time " + str(max_time)
      if max_events >= 1:
         commonOptions += " --max-events " + str(max_events)
      if no_enforce_lhef_options:
         commonOptions += " --enforce-lhef-options off"

      commandList = []
      optionList = []
      logFileList = []

      seed = first_seed
      for filename in get_filenames(lheStem, lhef.ext, first_seed, last_seed):
         optionList.append(commonOptions + " --infile " + filename)
         logFileList.append(logPath + name + format_tag(seed) + ".log")
         seed += 1
      commandList.append([name, executable, optionList, logFileList])

      # Combine the analyzer files.
      name += "_combine_plots"
      executable = "geneva-combine-plots"

      reportStem = format_path(outpath) + opthandler.getUnshoweredReportStem()
      option = " ".join(get_filenames(reportStem, ".xml", first_seed, last_seed))
      option += " --outfile " + reportStem + ".xml"
      option += " --backup-path " + join_paths(outpath, "backup-xml")
      optionList = [option]
      logFileList = [logPath + name + ".log"]
      commandList.append([name, executable, optionList, logFileList])
      return commandList

   #=======================================
   #              SHOWER STAGE
   #=======================================
   def getShowerCommand(self, option_file, pythia_options, first_seed, num_runs,
                        outpath, shower_tag, max_time, max_events,
                        no_enforce_lhef_options, no_hepmc):
      opthandler = opt.OptionHandler(option_file, "")
      generateStem = opthandler.getGenerateStem()
      lheStem = opthandler.getGeneratePath() + generateStem
      logPath = join_paths(outpath, "log")

      last_seed = first_seed + num_runs - 1

      # The shower tag is logically treated as an addition to the generateStem.
      generateStem += format_tag(shower_tag)

      # Shower the events
      name = generateStem + "_shower"
      executable = "geneva-shower-pythia8"

      # forward the options
      commonOptions = "--option-file " + option_file + " --pythia-options " + pythia_options
      if outpath:
         commonOptions += " --outpath " + outpath
      if shower_tag:
         commonOptions += " --shower-tag " + shower_tag
      if max_time >= 1:
         commonOptions += " --max-time " + str(max_time)
      if max_events >= 1:
         commonOptions += " --max-events " + str(max_events)
      if no_enforce_lhef_options:
         commonOptions += " --enforce-lhef-options off"
      if no_hepmc:
         commonOptions += " --hepmc off"

      commandList = []
      optionList = []
      logFileList = []

      # Get the list of LHEF files using the seed in the input card
      seed = first_seed
      for filename in get_filenames(lheStem, lhef.ext, first_seed, last_seed):
         optionList.append(commonOptions + " --infile " + filename
                           + " --seed " + str(seed))
         logFileList.append(logPath + name + format_tag(seed) + ".log")
         seed += 1
      commandList.append([name, executable, optionList, logFileList])

      # Combine the analyzer files if required.
      if opthandler.useAnalyzer():
         name += "_combine_plots"
         executable = "geneva-combine-plots"

         reportStem = format_path(outpath) + generateStem + opthandler.getAnalyzeTag()
         option = " ".join(get_filenames(reportStem, ".xml", first_seed, last_seed))
         option += " --outfile " + reportStem + ".xml"
         option += " --backup-path " + join_paths(outpath, "backup-xml")
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
         return self.getGenerateCommand(args.option_file, args.start_seed, args.num_runs, args.extra_options)
      elif args.stage == 'reweight':
         return self.getReweightCommand(args.option_file, args.start_seed, args.num_runs, args.sigma_below)
      elif args.stage == 'analyze-lhef':
         return self.getAnalyzeLhefCommand(args.option_file, args.start_seed, args.num_runs,
                                           args.outpath, args.max_time, args.max_events,
                                           args.no_enforce_lhef_options)
      elif args.stage == 'shower':
         return self.getShowerCommand(args.option_file, args.pythia_options, args.start_seed, args.num_runs,
                                      args.outpath, args.shower_tag, args.max_time, args.max_events,
                                      args.no_enforce_lhef_options, args.no_hepmc)
      else:
         calcname=opt.OptionHandler(args.option_file, "").getValue("calc")
         raise ValueError("Calculation "+ calcname +" cannot handle stage "+args.stage+" !")
