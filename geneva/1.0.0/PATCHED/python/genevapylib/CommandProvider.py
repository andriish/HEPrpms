#-------------------------------------------------------------------------------
# Author(s):
#    Christian Bauer, Simone Alioli
#
# Copyright:
#    Copyright (C) 2016 LBNL, CERN
#
#    This file is part of the Geneva MC framework. Geneva is distributed under
#    the terms of the GNU General Public License version 3 (GPLv3), see the
#    COPYING file that comes with this distribution for details.
#    Please respect the academic usage guidelines in the GUIDELINES file.
#
# Description:
#    Definition of getCommandProvider
#-------------------------------------------------------------------------------

def getCommandProvider(args):
   if args.stage == "analyze-rivet":
      from . import Rivet
      return Rivet.Rivet()
   else:
      #Pick the right process from input card
      from .OptionHandler import OptionHandler
      try:
         if "--help" in args.extra_options:
            raise ValueError("--help cannot be passed as extra option.")
         extra_options = args.extra_options
      except AttributeError:
         extra_options = ""
      calc = OptionHandler(args.option_file, extra_options).getValue("calc")
      if calc == "CalcSCETppV012":
         from .DrellYan import DrellYan
         return DrellYan()
      elif "CalcNLOppV" in calc:
         from .FixedOrder import FixedOrder
         return FixedOrder()
      else:
         raise ValueError("Cannot yet handle calculation "+calc+" !")
