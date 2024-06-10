#-------------------------------------------------------------------------------
# Author(s):
#    Calvin Berggren, Simone Alioli, Frank Tackmann
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
#    Functions for combining analyzer files
#-------------------------------------------------------------------------------

# uncomment to turn on debugger
#import pdb
#pdb.set_trace()

import math as m
import copy
import glob
import shutil
import os
import sys

## TODO: Try to use 30x faster cElementTree module (requires solving problem with CDATA)
import xml.etree.ElementTree as ET
import xml.dom.minidom

import process
import utils


#-------------------------------------------------------------------------------
# Private functions
# TODO: Update to proper _naming_conventions
#-------------------------------------------------------------------------------

# special CDATA handling taken from http://stackoverflow.com/questions/174890/how-to-output-cdata-using-elementtree
# requires ElementTree not cElementTree so the class can be extended
def CDATA(text=None):
   element = ET.Element('![CDATA[')
   element.text = text
   return element

ET._original_serialize_xml = ET._serialize_xml
def _serialize_xml(write, elem, encoding, qnames, namespaces):
   if elem.tag == '![CDATA[':
      write("\n<![CDATA[%s]]>\n" % elem.text)
   else:
      ET._original_serialize_xml(write, elem, encoding, qnames, namespaces)
ET._serialize_xml = ET._serialize['xml'] = _serialize_xml


def addxmlwhitespace(elem):
   """Return an ElementTree which has whitespace added."""
   rough_str = ET.tostring(elem)
   reparsed = xml.dom.minidom.parseString(rough_str)
   whitespace_str = reparsed.toprettyxml(indent="   ")
   return ET.fromstring(whitespace_str)


mmasymbols = ["name", "value", "error", "weightSq", "weight", "binEntries", "entries", "variations", "bins", "binWidth", "binning"]
mmasymbolsresults = ["linear", "log"]

def getcodefromfile(analyzerdata):
   """Load the analyzer from a given string. Returns a string which is a processed version of the input string."""

   code = analyzerdata

   code = code.replace("(* This is the Analyzer output of Geneva *)","")
   code = code.replace("{","[")
   code = code.replace("}","]")
   code = code.replace("*^","e")
   code = code.replace("-nan","\"nan\"")
   code = code.replace("\"nan\"","nan") # in case there are nan already in quotes
   code = code.replace("nan","\"nan\"")
   code = code.replace("inf","\"inf\"")
   code = code.replace("+\"inf\"","\"+inf\"")
   code = code.replace("-\"inf\"","\"-inf\"")

   for sym in mmasymbols:
      code = code.replace(", " + sym, ', "' + sym + '"')
   for sym in mmasymbolsresults:
      code = code.replace("= " + sym, '= "' + sym + '"')

   return code

def formatoutput(k, result, var):
   """Takes a particular rate or hist element from the dictionary as input and returns a string in Mathematica syntax"""

   formattedresult = ""
   formattedresult += var + "["
   for i, val in enumerate(k):
      if i != 0:
         formattedresult += ", "
      if type(val).__name__ == "str" and not val in mmasymbols:
         formattedresult += '"' + str(val) + '"'
      else:
         formattedresult += str(val)
   formattedresult += "] = "
   if type(result).__name__ == "str" and not result in mmasymbolsresults:
      formattedresult += '"' + result + '"'
   elif type(result).__name__ == "float":
      formattedresult += ("%.15e" % result).replace("e", "*^")
   elif type(result).__name__ == "list":
      formattedresult += "{"
      for j, el in enumerate(result):
         if j != 0:
            formattedresult += ", "
         if type(el).__name__ == "str":
            formattedresult += '"' + el + '"'
         elif type(el).__name__ == "float":
            formattedresult += ("%.15e" % el).replace("e", "*^")
         else:
            formattedresult += str(el)
      formattedresult += "}"
   else:
      formattedresult += str(result)
   formattedresult += ";\n"

   return formattedresult

def isNumber(x):
   return type(x).__name__ == "int" or type(x).__name__ == "float"

def Add(total, value):
   # for errors of unknown type
   if not isNumber(total) and total != "nan":
      return "othererror"
   if not isNumber(value) and value != "nan":
      return "othererror"

   # for nan errors
   if total == "nan" or value == "nan":
      return "nan"

   return total + value

def AddQuadrature(total, value):
   # for errors of unknown type
   if not isNumber(total) and total != "nan":
      return "othererror"
   if not isNumber(value) and value != "nan":
      return "othererror"

   # for nan errors
   if total == "nan" or value == "nan":
      return "nan"

   return m.sqrt(total*total + value*value)


def GetValue(weight, nentries):
   # for errors of unknown type
   if not isNumber(weight) and weight != "nan":
      return "othererror"
   if not isNumber(nentries) and nentries != "nan":
      return "othererror"

   # for nan errors
   if weight == "nan" or nentries == "nan":
      return "nan"

   return weight/max(1,nentries)

def GetError(weight, weightSq, nentries):
   # for errors of unknown type
   if not isNumber(weight) and weight != "nan":
      return "othererror"
   if not isNumber(weightSq) and weightSq != "nan":
      return "othererror"
   if not isNumber(nentries) and nentries != "nan":
      return "othererror"

   # for nan errors
   if weight == "nan" or weightSq == "nan" or nentries == "nan":
      return "nan"

   return m.sqrt( m.fabs(weightSq*1./max(1,nentries) - (weight*1./max(1,nentries))**2)/max(1,nentries) )

def _combine(infiles, stem, outfile, do_sum):
   """Combine the given infiles.

   Uses stem as the report title for the result. Writes the result to outfile.
   If do_sum is True, the results are logically added instead of averaged."""
   ratefinal = {}
   histfinal = {}
   seed = {}
   NumEvents = {}
   RuntimeInSec = {}
   MergeHistory = {}

   for c, analyzerfile in enumerate(infiles):
      process.log("Processing file: " + analyzerfile)
      tree = ET.parse(analyzerfile)
      root = tree.getroot()

      # Note: the filestem here refers to the previous title
      filestem = root.findall("./Summary/Title")[0].text

      rate = {}
      hist = {}
      # load analyzer files
      code = root.findall("./AnalyzerOutput")[0].text
      code = getcodefromfile(code)
      exec(code)
      # update metadata in the xml
      if c == 0:
         treefinal = copy.deepcopy(tree)
         rootfinal = treefinal.getroot()
         AnalyzerOutputfinal = rootfinal.findall("./AnalyzerOutput")[0]
         rootfinal.remove(AnalyzerOutputfinal) # clear output from memory
         rootfinal.findall("./Summary/Title")[0].text = stem
         if len(infiles) != 1:
            rootfinal.findall("./seed")[0].text = "multiple"
         NumEventsfinal = rootfinal.findall("./Summary/NumEvents")[0]
         RuntimeInSecfinal = rootfinal.findall("./Summary/RuntimeInSec")[0]
         MergeHistoryfinal = rootfinal.findall("./MergeHistory")
         if len(MergeHistoryfinal) != 0:
            rootfinal.remove(MergeHistoryfinal[0])
         MergeHistoryfinal = ET.Element("MergeHistory")

         # sections with CDATA require special handling
         # the specific sections which contain CDATA are hard-coded here
         FilesDifffinal = rootfinal.findall("./FilesDiff")
         if len(FilesDifffinal) != 0:
            FilesDiffcdata = CDATA("\n" + FilesDifffinal[0].text.strip() + "\n")
            FilesDifffinal[0].text = ""
            FilesDifffinal[0].append(FilesDiffcdata)
         svnDifffinal = rootfinal.findall("./svnDiff")
         if len(svnDifffinal) != 0:
            svnDiffcdata = CDATA("\n" + svnDifffinal[0].text.strip() + "\n")
            svnDifffinal[0].text = ""
            svnDifffinal[0].append(svnDiffcdata)
      else:
         NumEventsfinal.text = str( int(NumEventsfinal.text) + int(root.findall("./Summary/NumEvents")[0].text) )
         RuntimeInSecfinal.text = str( int(RuntimeInSecfinal.text) + int(root.findall("./Summary/RuntimeInSec")[0].text) )

      # add current file's info to the merge history
      mh = root.findall("./MergeHistory")
      if len(mh) == 0:
         item = ET.Element("item")
         itemseed = ET.SubElement(item, "seed")
         itemseed.text = root.findall("./seed")[0].text
         itemNumEvents = ET.SubElement(item, "NumEvents")
         itemNumEvents.text = root.findall("./Summary/NumEvents")[0].text
         itemRuntimeInSec = ET.SubElement(item, "RuntimeInSec")
         itemRuntimeInSec.text = root.findall("./Summary/RuntimeInSec")[0].text
         item = addxmlwhitespace(item) # add white space to the item element that has been built
         MergeHistoryfinal.append(item)
      else:
         for historyitem in mh[0]:
            MergeHistoryfinal.append(historyitem)

      # get the names of the rate observables
      if c == 0:
         ratefinal[stem, "names"] = rate[filestem, "names"]

      # Loop over all rate observables and merge the objects
      # Need to cover the central scale and all the scale variations
      for obs in ratefinal[stem, "names"]:
         nvariations = rate[filestem, obs, "variations"]
         if c == 0:
            ratefinal[stem, obs, "variations"] = nvariations
         nvariationsfinal = ratefinal[stem, obs, "variations"]
         isEmpty = (nvariations == 0 and nvariations != nvariationsfinal and rate[filestem, obs, "central", "binEntries"] == 0) # the rate in the current file is empty and has no variations
         needsReinitialization = (nvariationsfinal == 0 and c != 0 and nvariations != nvariationsfinal and ratefinal[stem, obs, "central", "binEntries"] == 0) # if previous files were empty for this rate but we have found one that is not, then this rate will not be initialized yet

         if needsReinitialization:
            ratefinal[stem, obs, "variations"] = nvariations
            nvariationsfinal = ratefinal[stem, obs, "variations"]

         scalenames = ["vary" + str(x) for x in range(1, nvariationsfinal+1)]
         scalenames.insert(0, "central")
         for scale in scalenames:
            # merge rate observable
            if c == 0 or (needsReinitialization and scale != "central"):
               ratefinal[stem, obs, scale, "name"] = rate[filestem, obs, scale, "name"]
               ratefinal[stem, obs, scale, "weight"] = 0
               ratefinal[stem, obs, scale, "weightSq"] = 0
               ratefinal[stem, obs, scale, "value"] = 0
               ratefinal[stem, obs, scale, "error"] = 0
               ratefinal[stem, obs, scale, "binEntries"] = 0
               if c == 0:
                  ratefinal[stem, obs, scale, "entries"] = 0
               else:
                  ratefinal[stem, obs, scale, "entries"] = ratefinal[stem, obs, "central", "entries"] - rate[filestem, obs, "central", "entries"]

            if not isEmpty:
               if not do_sum:
                  ratefinal[stem, obs, scale, "weight"] = Add(ratefinal[stem, obs, scale, "weight"], rate[filestem, obs, scale, "weight"])
                  ratefinal[stem, obs, scale, "weightSq"] = Add(ratefinal[stem, obs, scale, "weightSq"], rate[filestem, obs, scale, "weightSq"])
               else:
                  ratefinal[stem, obs, scale, "value"] = Add(ratefinal[stem, obs, scale, "value"], rate[filestem, obs, scale, "value"])
                  ratefinal[stem, obs, scale, "error"] = AddQuadrature(ratefinal[stem, obs, scale, "error"], rate[filestem, obs, scale, "error"])

               ratefinal[stem, obs, scale, "binEntries"] = Add(ratefinal[stem, obs, scale, "binEntries"], rate[filestem, obs, scale, "binEntries"])
               ratefinal[stem, obs, scale, "entries"] = Add(ratefinal[stem, obs, scale, "entries"], rate[filestem, obs, scale, "entries"])
            else:
               ratefinal[stem, obs, scale, "entries"] = Add(ratefinal[stem, obs, scale, "entries"], rate[filestem, obs, "central", "entries"])

            if c == len(infiles) - 1:
               if do_sum and not isEmpty:
                     ratefinal[stem, obs, scale, "weight"] = ratefinal[stem, obs, scale, "value"] * ratefinal[stem, obs, scale, "entries"]
                     ratefinal[stem, obs, scale, "weightSq"] =  ratefinal[stem, obs, scale, "error"]**2 * ratefinal[stem, obs, scale, "entries"]**2 + ratefinal[stem, obs, scale, "entries"] * ratefinal[stem, obs, scale, "value"]**2
               else:
                 ratefinal[stem, obs, scale, "value"] = GetValue(ratefinal[stem, obs, scale, "weight"], ratefinal[stem, obs, scale, "entries"])
                 ratefinal[stem, obs, scale, "error"] = GetError(ratefinal[stem, obs, scale, "weight"], ratefinal[stem, obs, scale, "weightSq"], ratefinal[stem, obs, scale, "entries"])

      # get the names of the hist observables
      if not (filestem, "names") in hist:
         continue
      if c == 0:
         histfinal[stem, "names"] = hist[filestem, "names"]

      # Loop over all hist observables and merge the objects
      for obs in histfinal[stem, "names"]:
         nvariations = hist[filestem, obs, "variations"]
         if c == 0:
            histfinal[stem, obs, "variations"] = nvariations
         nvariationsfinal = histfinal[stem, obs, "variations"]
         isEmpty = (nvariations == 0 and nvariations != nvariationsfinal and sum(hist[filestem, obs, "central", "binEntries"]) == 0) # the hist in the current file is empty and has no variations
         needsReinitialization = (nvariationsfinal == 0 and c != 0 and nvariations != nvariationsfinal and sum(histfinal[stem, obs, "central", "binEntries"]) == 0) # if previous files were empty for this hist but we have found one that is not, then this hist will not be initialized yet

         if needsReinitialization:
            histfinal[stem, obs, "variations"] = nvariations
            nvariationsfinal = histfinal[stem, obs, "variations"]

         scalenames = ["vary" + str(x) for x in range(1, nvariationsfinal+1)]
         scalenames.insert(0, "central")
         for scale in scalenames:
            # merge hist observable
            if c == 0 or (needsReinitialization and scale != "central"):
               histfinal[stem, obs, scale, "name"] = hist[filestem, obs, scale, "name"]
               histfinal[stem, obs, scale, "bins"] = hist[filestem, obs, scale, "bins"]
               histfinal[stem, obs, scale, "binWidth"] = hist[filestem, obs, scale, "binWidth"]
               histfinal[stem, obs, scale, "binning"] = hist[filestem, obs, scale, "binning"]
               nbins = len(hist[filestem, obs, scale, "weight"])
               histfinal[stem, obs, scale, "weight"] = [0]*nbins
               histfinal[stem, obs, scale, "weightSq"] = [0]*nbins
               histfinal[stem, obs, scale, "value"] = [0]*nbins
               histfinal[stem, obs, scale, "error"] = [0]*nbins
               histfinal[stem, obs, scale, "binEntries"] = [0]*nbins
               if c == 0:
                  histfinal[stem, obs, scale, "entries"] = 0
               else:
                  histfinal[stem, obs, scale, "entries"] = histfinal[stem, obs, "central", "entries"] - hist[filestem, obs, "central", "entries"]

            if not isEmpty:
               if not do_sum:
                  histfinal[stem, obs, scale, "weight"] = [Add(a,b) for a,b in zip(histfinal[stem, obs, scale, "weight"], hist[filestem, obs, scale, "weight"])]
                  histfinal[stem, obs, scale, "weightSq"] = [Add(a,b) for a,b in zip(histfinal[stem, obs, scale, "weightSq"], hist[filestem, obs, scale, "weightSq"])]
               else:
                  histfinal[stem, obs, scale, "value"] = [Add(a,b) for a,b in zip(histfinal[stem, obs, scale, "value"], hist[filestem, obs, scale, "value"])]
                  histfinal[stem, obs, scale, "error"] = [AddQuadrature(a,b) for a,b in zip(histfinal[stem, obs, scale, "error"], hist[filestem, obs, scale, "error"])]

               histfinal[stem, obs, scale, "binEntries"] = [Add(a,b) for a,b in zip(histfinal[stem, obs, scale, "binEntries"], hist[filestem, obs, scale, "binEntries"])]
               histfinal[stem, obs, scale, "entries"] = Add(histfinal[stem, obs, scale, "entries"], hist[filestem, obs, scale, "entries"])
            else:
               histfinal[stem, obs, scale, "entries"] = Add(histfinal[stem, obs, scale, "entries"], hist[filestem, obs, "central", "entries"])

            if c == len(infiles) - 1:
               if do_sum and not isEmpty:
                  histfinal[stem, obs, scale, "weight"] =  [ a * histfinal[stem, obs, scale, "entries"] for a in histfinal[stem, obs, scale, "value"]]
                  histfinal[stem, obs, scale, "weightSq"] = [Add(a,b) for a,b in zip([(c*histfinal[stem, obs, scale, "entries"])**2 for c in histfinal[stem, obs, scale, "error"]], [(d**2 * histfinal[stem, obs, scale, "entries"]) for d in histfinal[stem, obs, scale, "value"]])]
               else:
                  histfinal[stem, obs, scale, "value"] = [GetValue(a,histfinal[stem, obs, scale, "entries"]) for a in histfinal[stem, obs, scale, "weight"]]
                  histfinal[stem, obs, scale, "error"] = [GetError(a,b,histfinal[stem, obs, scale, "entries"]) for a,b in zip(histfinal[stem, obs, scale, "weight"], histfinal[stem, obs, scale, "weightSq"])]


   # TODO: also sort mergehistory
   rootfinal.append(MergeHistoryfinal)

   ### write out the final result to a file in original syntax

   analyzeroutput = ""

   # TODO: remove sort ? (needed to facilitate comparisons)
   for k, result in sorted(ratefinal.iteritems()):
      analyzeroutput += formatoutput(k, result, "rate")

   analyzeroutput += "\n"

   for k, result in sorted(histfinal.iteritems()):
      analyzeroutput += formatoutput(k, result, "hist")

   AnalyzerOutputfinal = ET.SubElement(rootfinal, "AnalyzerOutput")
   analyzeroutputcdata = CDATA(analyzeroutput)
   AnalyzerOutputfinal.append(analyzeroutputcdata)

   process.log("Writing file: " + outfile)
   treefinal.write(outfile, xml_declaration=True)

def _call_combine(infiles, stem, outfile, do_sum):
   try:
      _combine(infiles, stem, outfile, do_sum)
   except (KeyboardInterrupt, Exception):
      process.signal_error("In one of the files: " + ", ".join(infiles))
   return process.return_value()

#-------------------------------------------------------------------------------
# Main public function
#-------------------------------------------------------------------------------

def combine(infiles, outfile, keep, backup_path, jobs, do_sum):
   stem = None
   while process.master:
      # Skip files that don't exist.
      infiles = list(filter(utils.check_file_exists, infiles))

      if not infiles:
         process.signal_error("No input files, that seemed a bit too easy ...", False)
         break

      if do_sum and len(infiles) > 2:
         process.signal_error("Cannot sum more than two report files.", False)
         break

      stem = os.path.splitext(os.path.basename(outfile))[0]
      if not stem:
         process.signal_error("Invalid output file (empty file stem): "
                              + outfile, False)
         break

      try:
         utils.ensure_dir_exists(os.path.dirname(outfile))
      except OSError:
         process.signal_exit(1)
      break

   process.exit_if_error()

   if process.mpi:
      rank = process.rank
      batch_files = process.mpi.bcast(infiles)
      stem = process.mpi.bcast(stem)
   else:
      process.open_pool(jobs)
      batch_files = infiles

   size = process.size
   offset = 0

   while True:
      num = len(batch_files)
      # Single core or last batch, write to the correct outfile and break out.
      if size == 1 or num <= 3:
         if process.master:
            _call_combine(batch_files, stem, outfile, do_sum)
         break

      if num <= 2*size:
         batch_size = 2
      else:
         batch_size = int(m.ceil(num/(1.0*size)))
      batches = [batch_files[i:i+batch_size] for i in xrange(0, num, batch_size)]

      # Perform run on each batch, and write result to temporary file.
      if process.mpi:
         if rank < len(batches):
            _call_combine(batches[rank], stem,
                          outfile + ".batch" + str(offset + rank), do_sum)
      else:
         procStates = []
         for i in range(len(batches)):
            p = process.pool.apply_async(
               _call_combine,
               (batches[i], stem, outfile + ".batch" + str(offset + i), do_sum))
            procStates.append(p)
         for p in procStates:
            if p.get():
               process.pool.terminate()
               process.signal_error(1)

      if process.has_error():
         break

      # Use created batch files as new input files.
      batch_files = [outfile + ".batch" + str(offset + i) for i in range(len(batches))]
      offset = offset + len(batches)

   # Clean up and remove temporary files.
   if process.master:
      process.close_pool()
      utils.remove_files(glob.glob(outfile + ".batch*"), "temporary file")

   process.exit_if_error("Could not combine plots due to errors in one or more files.")

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

   return 0
