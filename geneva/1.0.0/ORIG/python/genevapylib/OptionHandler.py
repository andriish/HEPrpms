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
#    Definition of class OptionHandler
#-------------------------------------------------------------------------------

import cpp_extensions as cpp
from utils import format_tag, format_path


class OptionHandler:
   """Provides an interface to the underlying C++ options of Geneva as relevant
      to the Python driver.

      Calls the C++ extensions to combine the global default options with the
      given option file (either .yml or .xml) and extra options (in the form of
      a valid geneva command line).

      The resulting options are interpreted to provide needed information or
      queries for the Python driver.

      In addition, it also provides access to the raw option values via the
      getValue function, but this direct access should be avoided.
   """
   def __init__(self, filename, extra_options):
      if filename.endswith('.xml') or filename.endswith('.yml'):
         self.optionMap = cpp.get_options("--optionsFile " + filename + " " + extra_options)
      else:
         raise ValueError("Option file " + filename + " not yet supported. Supported extensions are .xml and .yml")
      if not self.optionMap:
         raise RuntimeError("Unable to fill the OptionMap dictionary.")

   def getValue(self, name):
      return self.optionMap[name]

   def getSetupPath(self):
      return format_path(self.getValue("filePath::setup"))

   def getSetupStem(self):
      return self.getValue("fileStem") + format_tag(self.getValue("setupTag"))

   def needsIntegrationGrids(self):
      return self.getValue("EventGenerator").startswith("Adaptive")

   def getGridFileStem(self):
      if self.needsIntegrationGrids():
         stem = self.getValue("Mint::fileStem")
         if not stem:
            stem = self.getSetupStem()
            stem += format_tag(self.getValue("Mint::fileTag"))
            stem += format_tag("grid")
         return stem
      else:
         raise ValueError("Cannot yet handle EventGenerator: "
                          + self.getValue("EventGenerator"))

   def getXSFileStem(self):
      stem = self.getValue("CalcSCETppV012::ImprovedXSFix::fileStem")
      if not stem:
         stem = self.getSetupStem()
         stem += format_tag(self.getValue("CalcSCETppV012::ImprovedXSFix::fileTag"))
         stem += format_tag("profile")
      return stem

   def getGeneratePath(self):
      return format_path(self.getValue("filePath::generate"))

   def getGenerateStem(self):
      return self.getValue("fileStem") + format_tag(self.getValue("generateTag"))

   def getAnalyzeTag(self):
      return format_tag(self.getValue("Analyzer::fileTag"))

   def useAnalyzer(self):
      if self.getValue("Analyzer") == "None":
         return False
      report = self.getValue("writeReport").lower()
      return report == "true" or report == "on" or report == "t"

   def useUnshoweredAnalyzer(self):
      if not self.useAnalyzer():
         return False
      analyzer = self.getValue("Analyzer::Unshowered").lower()
      return analyzer == "on" or analyzer == "true" or analyzer == "t"

   def getUnshoweredReportStem(self):
      return self.getGenerateStem() + self.getAnalyzeTag() + format_tag("lhef")

   def useFixedOrderAnalyzer(self):
      if self.getValue("Analyzer") == "None":
         return False
      fo_analyzer = self.getValue("Analyzer::FixedOrder").lower()
      return fo_analyzer == "on" or fo_analyzer == "true" or fo_analyzer == "t"

   def getFixedOrderReportStem(self):
      stem = self.getGenerateStem() + self.getAnalyzeTag()
      if self.getValue("FixedOrder") == "LO":
         stem += format_tag("fixedLO")
      elif self.getValue("FixedOrder") == "NLO":
         stem += format_tag("fixedNLO")
      return stem
