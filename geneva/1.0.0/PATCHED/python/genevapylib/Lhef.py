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
#    Functions to compute xsec from lhef file
#-------------------------------------------------------------------------------

import math as m

from .config import geneva_use_zlib
from . import process

if geneva_use_zlib:
   import gzip
   import io
   ext = ".lhe.gz"
else:
   ext = ".lhe"


def _open_file(filename):
   f = open(filename,'rb')
   if (f.read(2) == '\x1f\x8b') and geneva_use_zlib:
      f.seek(0)
      return io.BufferedReader(gzip.GzipFile(fileobj=f))
   else:
      f.seek(0)
      return f

class XSInfo:
   def __init__(self):
      self.sumWeights=0.0
      self.sumWeightsSq=0.0
      self.value=0.0
      self.error=0.0
      self.numEvents=0
      self.numEventsInFile=0
      self.sigmaBelow = 0.0
      self.sigmaBelowUp = 0.0
      self.sigmaBelowDown = 0.0
      self.sigmaAbove = 0.0
      self.sigmaAboveUp = 0.0
      self.sigmaAboveDown = 0.0

def getXSInfoMPI(files, mult):
   """
   Computes the required cross sections below and above the resolution cutoff
   from the list of files using MPI
   """

   from mpi4py import MPI

   #Get the number of loops one needs to perform
   numLoops = int(len(files) / process.size) + (len(files) % process.size > 0)

   #Determine the cross-section information from the LHE files
   if process.master:
      sigmaBelow = 0.
      sigmaAbove = 0.
      sigmaBelowUp = 0.
      sigmaAboveUp = 0.
      sigmaBelowDown = 0.
      sigmaAboveDown = 0.
      neventsRequested = 0
      neventsFound = 0
      nevents = 0
      sumWeights = 0.
      sumWeightsSquared = 0.

   add = 0
   for i in range(numLoops):
      tmpSigma0 = 0.
      tmpSigmaAbove = 0.
      tmpSigma0Up = 0.
      tmpSigmaAboveUp = 0.
      tmpSigma0Down = 0.
      tmpSigmaAboveDown = 0.
      tmpNeventsRequested = 0
      tmpNeventsFound = 0
      tmpNevents = 0
      tmpSumWeights = 0.
      tmpSumWeightsSquared = 0.
      if process.rank + add < len(files):
         try:
            lhefile = files[process.rank + add]
            process.log("Calculating cross section from file: " + lhefile)
            with _open_file(lhefile) as f:
               npart = 0
               while True:
                  line = f.readline()
                  if line == '':
                     break
                  elif '<event>' in line:
                     tmpNevents += 1
                     line = f.readline()
                     npart=int(line.split()[0])
                     #TODO: Make this script independent from the calculation performed
                     assert npart >= mult
                     weight=float(line.split()[2])
                     if npart == mult:
                        tmpSigma0+=weight
                     else:
                        tmpSigmaAbove+=weight
                  elif '<weights>' in line:
                     #TODO: Make this script independent from the calculation performed
                     assert npart >= mult
                     weightUp=float(line.split()[10])
                     weightDown=float(line.split()[11])
                     if npart == mult:
                        tmpSigma0Up+=weightUp
                        tmpSigma0Down+=weightDown
                     else:
                        tmpSigmaAboveUp+=weightUp
                        tmpSigmaAboveDown+=weightDown
                  elif '#   number of processed events :' in line:
                     tmpNeventsRequested += int((line.split(':')[1]).strip())
                  elif '#   number of events in file :' in line:
                     tmpNeventsFound += int((line.split(':')[1]).strip())
                  elif '#   sum of processed weights :' in line:
                     tmpSumWeights += float((line.split(':')[1]).strip())
                  elif '#   sum of processed weights squared :' in line:
                     tmpSumWeightsSquared += float((line.split(':')[1]).strip())
                  elif '#   weights normalized to total cross section :' in line:
                     if (line.split(':')[1]).strip() == "true":
                        raise RuntimeError("The file contains event weights normalized to the total cross section.")

            if (tmpNevents != tmpNeventsFound):
               raise RuntimeError("The file is supposed to contain {0} events,\n"
                                  "which does not match the number of read events: {1}".format(tmpNeventsFound, tmpNevents))

         except Exception:
            process.signal_error("In file: " + lhefile)

      process.exit_if_error("Unable to combine cross sections due to errors in one or more files.")

      # use MPI to get the totals
      tmpSigma0tot = process.mpi.reduce(tmpSigma0, op = MPI.SUM)
      tmpSigmaAbovetot = process.mpi.reduce(tmpSigmaAbove, op = MPI.SUM)
      tmpSigma0Uptot = process.mpi.reduce(tmpSigma0Up, op = MPI.SUM)
      tmpSigmaAboveUptot = process.mpi.reduce(tmpSigmaAboveUp, op = MPI.SUM)
      tmpSigma0Downtot = process.mpi.reduce(tmpSigma0Down, op = MPI.SUM)
      tmpSigmaAboveDowntot = process.mpi.reduce(tmpSigmaAboveDown, op = MPI.SUM)
      tmpNeventsRequestedtot = process.mpi.reduce(tmpNeventsRequested, op = MPI.SUM)
      tmpNeventsFoundtot = process.mpi.reduce(tmpNeventsFound, op = MPI.SUM)
      tmpNeventstot = process.mpi.reduce(tmpNevents, op = MPI.SUM)
      tmpSumWeights = process.mpi.reduce(tmpSumWeights, op = MPI.SUM)
      tmpSumWeightsSquared = process.mpi.reduce(tmpSumWeightsSquared, op = MPI.SUM)

      if process.master:
         sigmaBelow += tmpSigma0tot
         sigmaAbove += tmpSigmaAbovetot
         sigmaBelowUp += tmpSigma0Uptot
         sigmaAboveUp += tmpSigmaAboveUptot
         sigmaBelowDown += tmpSigma0Downtot
         sigmaAboveDown += tmpSigmaAboveDowntot
         neventsRequested += tmpNeventsRequestedtot
         neventsFound += tmpNeventsFoundtot
         nevents += tmpNeventstot
         sumWeights += tmpSumWeights
         sumWeightsSquared += tmpSumWeightsSquared

      add += process.size

   if process.master:
      xsInfo = XSInfo()
      xsInfo.sumWeights = sumWeights
      xsInfo.sumWeightsSq = sumWeightsSquared
      xsInfo.value = sumWeights/neventsRequested
      xsInfo.error = m.sqrt((sumWeightsSquared/neventsRequested - (sumWeights/neventsRequested)**2)/(neventsRequested-1))
      xsInfo.numEvents =  neventsRequested
      xsInfo.numEventsInFile = neventsFound
      xsInfo.sigmaBelow = sigmaBelow/neventsRequested
      xsInfo.sigmaBelowUp = sigmaBelowUp/neventsRequested
      xsInfo.sigmaBelowDown = sigmaBelowDown/neventsRequested
      xsInfo.sigmaAbove = sigmaAbove/neventsRequested
      xsInfo.sigmaAboveUp = sigmaAboveUp/neventsRequested
      xsInfo.sigmaAboveDown = sigmaAboveDown/neventsRequested
   else:
      xsInfo = None

   return xsInfo


def getXSInfoSingleCore(files, mult):
   """
   Computes the required cross sections below and above the resolution cutoff
   from the list of files on a single core
   """
   sigmaBelow = 0.
   sigmaAbove = 0.
   sigmaBelowUp = 0.
   sigmaAboveUp = 0.
   sigmaBelowDown = 0.
   sigmaAboveDown = 0.
   neventsRequested = 0
   neventsFound = 0
   totSumWeights = 0.
   totSumWeightsSq = 0.
   numevts = 0
   for lhefile in files:
      try:
         process.log("Calculating cross section from file: " + lhefile)
         with _open_file(lhefile) as f:
            npart = 0
            while True:
               line = f.readline()
               if line == '':
                  break
               elif '<event>' in line:
                  numevts += 1
                  line = f.readline()
                  npart = int(line.split()[0])
                  assert npart >= mult
                  weight=float(line.split()[2])
                  if npart == mult:
                     sigmaBelow+=weight
                  else:
                     sigmaAbove+=weight
               elif '<weights>' in line:
                  #TODO: Make this script independent from the calculation performed
                  assert npart >= mult
                  weightUp = float(line.split()[10])
                  weightDown = float(line.split()[11])
                  if npart == mult:
                     sigmaBelowUp += weightUp
                     sigmaBelowDown += weightDown
                  else:
                     sigmaAboveUp += weightUp
                     sigmaAboveDown += weightDown
               elif '#   number of processed events :' in line:
                  neventsRequested += int((line.split(':')[1]).strip())
               elif '#   number of events in file :' in line:
                  neventsFound += int((line.split(':')[1]).strip())
               elif '#   sum of processed weights :' in line:
                  totSumWeights += float((line.split(':')[1]).strip())
               elif '#   sum of processed weights squared :' in line:
                  totSumWeightsSq += float((line.split(':')[1]).strip())
               elif '#   weights normalized to total cross section :' in line:
                  if (line.split(':')[1]).strip() == "true":
                     raise RuntimeError("The file contains event weights normalized to the total cross section.")

         if (numevts != neventsFound):
            raise RuntimeError("The file is supposed to contain {0} events, "
                               "which does not match the number of read events: {1}".format(neventsFound, numevts))

      except Exception:
         process.signal_error("In file: " + lhefile)

   process.exit_if_error("Unable to combine cross sections due to errors in one or more files.")

   xsInfo = XSInfo()
   xsInfo.sumWeights = totSumWeights
   xsInfo.sumWeightsSq = totSumWeightsSq
   xsInfo.value = totSumWeights/neventsRequested
   xsInfo.error = m.sqrt((totSumWeightsSq/neventsRequested - (totSumWeights/neventsRequested)**2)/(neventsRequested-1))
   xsInfo.numEvents =  neventsRequested
   xsInfo.numEventsInFile = neventsFound
   xsInfo.sigmaBelow = sigmaBelow/neventsRequested
   xsInfo.sigmaBelowUp = sigmaBelowUp/neventsRequested
   xsInfo.sigmaBelowDown = sigmaBelowDown/neventsRequested
   xsInfo.sigmaAbove = sigmaAbove/neventsRequested
   xsInfo.sigmaAboveUp = sigmaAboveUp/neventsRequested
   xsInfo.sigmaAboveDown = sigmaAboveDown/neventsRequested
   return xsInfo
