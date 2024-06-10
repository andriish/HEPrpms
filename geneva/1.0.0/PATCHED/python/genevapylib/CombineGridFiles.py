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
#    Functions to combine and plot grids
#-------------------------------------------------------------------------------

import os
import glob
from collections import OrderedDict
import numpy as np

from . import process
from . import utils

#-------------------------------------------------------------------------------
# Private functions
#-------------------------------------------------------------------------------

def _plot_grids(outstem, iter, dim, numBins, values, numevts, xsec):
   #  order grids by dimension and then lexicographically
   allgrids=[g[0] for g in sorted(zip(list(dim.keys()), list(dim.values())), key=lambda g: g[1])]
   # calculates cumulants
   cumValues = {}
   for g in allgrids:
      for d in range(dim[g]):
         cumValues[g,d,0] = 0.
         for bin in range(0,numBins[g]):
            if values[g,d,bin,0] != 0:
               cumValues[g,d,bin+1] = cumValues[g,d,bin] + abs(values[g,d,bin,1]/values[g,d,bin,0])
            else:
               cumValues[g,d,bin+1] = cumValues[g,d,bin]
         if cumValues[g,d,numBins[g]] != 0:
            for bin in range(0,numBins[g]+1):
               cumValues[g,d,bin] /= cumValues[g,d,numBins[g]]

   # calculates new grids
   Grid = {}
   for g in allgrids:
      for d in range(dim[g]):
         Grid[g,iter,d]=[]
         Grid[g,iter,d].append(0.)
         if cumValues[g,d,numBins[g]] != 0:
            for bin in range(1,numBins[g]):
               r=float(bin)/float(numBins[g])
               j=1
               while(r>cumValues[g,d,j]):
                  j+=1
               assert j>0
               assert j<=numBins[g]
               assert cumValues[g,d,j] != cumValues[g,d,j-1]
               Grid[g,iter,d].append((r-cumValues[g,d,j-1])/(cumValues[g,d,j] - cumValues[g,d,j-1])*(values[g,d,j,2]-values[g,d,j-1,2])+values[g,d,j-1,2])
            Grid[g,iter,d].append(1.)
         else:
            for i in range(1,numBins[g]):
              Grid[g,iter,d].append(0.)
   NumPoints = {}
   CrossSection = {}
   combinedPlot = outstem + ".pyplot"
   process.log("Storing information to plot grids to file: " + combinedPlot)
   if os.path.isfile(combinedPlot):
      # if file already exists, read in values of previous iterations
      assert iter > 1
      with open(combinedPlot,"r") as input:
         for line in input:
            if line.startswith("    Grid[") or line.startswith("    NumPoints[") or line.startswith("    CrossSection["):
               exec(line.lstrip(' ').rstrip('\n').rstrip('\r'))
   else:
      assert iter == 1
      for g in allgrids:
         for d in range(dim[g]):
            Grid[g,0,d]=[values[g,d,b,2] for b in range(numBins[g])]

   totalXS = {}
   totalPoints ={}
   for it in range(1,iter+1):
      totalXS[it]=0.
      totalPoints[it]=0
      for g in allgrids:
         if(it == iter):
            NumPoints[g,it]=numevts[g]
            CrossSection[g,it]=xsec[g]
         totalPoints[it]+=NumPoints[g,it]
         totalXS[it]+=CrossSection[g,it]
   pdfName = combinedPlot[0:combinedPlot.rfind(".")] + ".pdf"
   with open(combinedPlot,"w") as target:
      target.write("#! /usr/bin/env python\n")
      target.write("from matplotlib.backends.backend_pdf import PdfPages\n")
      target.write("from matplotlib import pyplot as plt\n")
      target.write("plt.rc('text', usetex=True)\n")
      target.write("Grid={}\n")
      target.write("NumPoints={}\n")
      target.write("CrossSection={}\n")
      target.write("ax={}\n")
      target.write("with PdfPages(\'" + os.path.basename(outstem) + ".pdf') as pdf:\n")
      for g in allgrids:
         target.write("    fig = plt.figure(figsize=("+str(10*dim[g])+","+str(10*iter)+"), dpi=100)\n")
         for d in range(dim[g]):
               target.write("    Grid[\""+g+"\",0,"+str(d)+"]="+str([Grid[g,0,d][b] for b in range(numBins[g])])+"\n")
               target.write("    Grid[\""+g+"\",0,"+str(d)+"].append(1.0)\n")
         for it in range(1,iter+1):
            target.write("    NumPoints[\""+g+"\","+str(it)+"]="+str(NumPoints[g,it])+"\n")
            target.write("    CrossSection[\""+g+"\","+str(it)+"]="+str(CrossSection[g,it])+"\n")
            for d in range(dim[g]):
               target.write("    Grid[\""+g+"\","+str(it)+","+str(d)+"]="+str([Grid[g,it,d][b] for b in range(numBins[g])])+"\n")
               target.write("    Grid[\""+g+"\","+str(it)+","+str(d)+"].append(1.0)\n")
               target.write("    ax[\""+g+"\","+str(it)+","+str(d)+"]=fig.add_subplot("+str(iter)+","+str(dim[g])+","+str((it-1)*dim[g]+d+1)+")\n")
               if (d==0):
                  target.write("    ax[\""+g+"\","+str(it)+","+str(d)+"].set_title(\'Iteration="+str(it)+" $N$="+str(NumPoints[g,it])+" ("+"{0:.4g}".format(100.*float(NumPoints[g,it])/totalPoints[it])+" \% of total "+str(totalPoints[it])+")     $\sigma$="+"{0:.5g}".format(CrossSection[g,it])+" [pb] ("+"{0:.4g}".format(100.*CrossSection[g,it]/totalXS[it])+" \% of total "+ "{0:.5g}".format(totalXS[it])+" [pb] )\', loc='left',  fontsize=25 )\n")
               target.write("    ax[\""+g+"\","+str(it)+","+str(d)+"].set_xticks(Grid[\""+g+"\","+str(it)+","+str(d)+"])\n")
               target.write("    ax[\""+g+"\","+str(it)+","+str(d)+"].set_yticks(Grid[\""+g+"\","+str(it-1)+","+str(d)+"])\n")
               target.write("    ax[\""+g+"\","+str(it)+","+str(d)+"].plot(Grid[\""+g+"\","+str(it)+","+str(d)+"],Grid[\""+g+"\","+str(it-1)+","+str(d)+"])\n")
               target.write("    ax[\""+g+"\","+str(it)+","+str(d)+"].grid(color = 'gray', linestyle = '-',linewidth=0.1)\n")
               target.write("    plt.tick_params(axis='x', which='major', bottom='off', top='off', labelbottom='off')\n")
               target.write("    plt.tick_params(axis='y', which='major', left='off', right='off', labelleft='off')\n")
         target.write("    fig.tight_layout(pad=3)\n")
         target.write("    fig.subplots_adjust(left=0.01, bottom=0.01, right=0.99, top=0.9, wspace=0.02, hspace=0.1)\n")
         target.write("    fig.suptitle(\'Channel "+g.replace("_"," ")+"\', fontsize=30)\n")
         target.write("    pdf.savefig()\n")
         target.write("    plt.close()\n");

def _write(outfile, numBins, dim, weightPrev, weightNow, weightSqPrev, weightSqNow,
              numEventsPrev, numEvents, values, numFiles, numSigs, Signaturelines, XSlines):
   process.log("Writing grid to file: " + outfile)
   with open(outfile, 'w') as out:
      out.write(str(numBins) + "\n")
      out.write(str(dim) + "\n")
      out.write(str(weightPrev) + "\n")
      out.write(str(weightNow) + "\n")
      out.write(str(weightSqPrev) + "\n")
      out.write(str(weightSqNow) + "\n")
      out.write(str(numEventsPrev) + "\n")
      out.write(str(numEvents) + "\n")
      for d in range(dim):
         for bin in range(numBins):
            out.write(str(values[d,bin,0]) + " " + str(values[d,bin,1]) + " " + str(values[d,bin,2]) + "\n")
      out.write('###\n')
      crossSection = 0.
      for sig in range(numSigs):
         out.write(Signaturelines[sig])
         out.write(str(XSlines[sig] / numFiles) + "\n")
         crossSection+=float(XSlines[sig])/numFiles

   return crossSection

def _combine_MPI(infiles, outfile):
   from mpi4py import MPI
   infile = infiles[process.rank]
   dim = 0
   numBins = 0
   values = {}
   numEvents = 0
   crossSection = 0.
   numEventsPrev = 0
   weightPrev = 0.
   weightSqPrev= 0.
   process.log("Reading grid from file: " + infile)
   try:
      with open(infile) as f:
         numBins = int(f.readline())
         dim = int(f.readline())
         weightPrev = float(f.readline())
         tmpWeightNow = float(f.readline())
         weightSqPrev = float(f.readline())
         tmpWeightSqNow = float(f.readline())
         numEventsPrev = int(f.readline())
         tmpNumEventsNow = int(f.readline())
         tmpValuesEntries = np.empty(dim*numBins,dtype=int)
         tmpValuesWeights = np.empty(dim*numBins,dtype=float)
         tmpValuesPos = np.empty(dim*numBins,dtype=float)
         for d in range(dim):
            for bin in range(numBins):
               line = f.readline().split()
               tmpValuesEntries[d*numBins+bin]=int(line[0])
               tmpValuesWeights[d*numBins+bin]=float(line[1])
               tmpValuesPos[d*numBins+bin]=float(line[2])
         ## Process the information for each Signature in the end
         tmp=f.readline()
         assert tmp=="###\n"
         numSigs = 0
         Signaturelines=[]
         tmpXSlines=[]
         while True:
            sigLine = f.readline()
            if sigLine == '':
               break
            valLine = f.readline()
            Signaturelines.append(sigLine)
            tmpXSlines.append(float(valLine))
            numSigs += 1
   except Exception:
      process.signal_error("In file: " + infile)

   if process.has_error():
      return None, None, None, None, None

   maxNumBins = process.mpi.reduce(numBins, op = MPI.MAX)
   minNumBins = process.mpi.reduce(numBins, op = MPI.MIN)
   maxDims = process.mpi.reduce(dim, op = MPI.MAX)
   minDims = process.mpi.reduce(dim, op = MPI.MIN)
   maxWeightPrev = process.mpi.reduce(weightPrev, op = MPI.MAX)
   minWeightPrev = process.mpi.reduce(weightPrev, op = MPI.MIN)
   maxWeightSqPrev = process.mpi.reduce(weightSqPrev, op = MPI.MAX)
   minWeightSqPrev = process.mpi.reduce(weightSqPrev, op = MPI.MIN)
   maxNumEventsPrev = process.mpi.reduce(numEventsPrev, op = MPI.MAX)
   minNumEventsPrev = process.mpi.reduce(numEventsPrev, op = MPI.MIN)

   if process.master:
      try:
         assert maxNumBins == minNumBins
         assert maxDims == minDims
         assert maxWeightPrev == minWeightPrev
         assert maxWeightSqPrev == minWeightSqPrev
         assert maxNumEventsPrev == minNumEventsPrev
      except Exception:
         process.signal_error("Inconsistent input files.")

   if process.has_error():
      return None, None, None, None, None

   weightNow = process.mpi.reduce(tmpWeightNow, op = MPI.SUM)
   weightSqNow = process.mpi.reduce(tmpWeightSqNow, op = MPI.SUM)
   numEvents = process.mpi.reduce(tmpNumEventsNow, op = MPI.SUM)
   valuesEntries = process.mpi.reduce(tmpValuesEntries, op = MPI.SUM)
   valuesWeights = np.empty(dim*numBins)
   process.mpi.Reduce([tmpValuesWeights, MPI.DOUBLE], [valuesWeights, MPI.DOUBLE], op = MPI.SUM)
   XSlines=[]
   for sig in range(numSigs):
      xs = process.mpi.reduce(tmpXSlines[sig], op = MPI.SUM)
      XSlines.append(xs)

   if process.master:
      for d in range(dim):
         for bin in range(numBins):
            values[d,bin,0]=valuesEntries[d*numBins+bin]
            values[d,bin,1]=valuesWeights[d*numBins+bin]
            values[d,bin,2]=tmpValuesPos[d*numBins+bin]
         values[d,numBins,2]=1.

      try:
         crossSection = _write(outfile, numBins, dim, weightPrev, weightNow, weightSqPrev, weightSqNow,
                                  numEventsPrev, numEvents, values, len(infiles), numSigs, Signaturelines, XSlines)
      except Exception:
         process.signal_error("Could not write to file: " + outfile)

   return dim, numBins, values, numEvents, crossSection

def _combine_serial(infiles, outfile):
   ## Set up variables we need
   weightPrev = 0.
   weightNow = 0.
   weightSqPrev = 0.
   weightSqNow = 0.
   numEventsPrev = 0
   Signaturelines = []
   XSlines = []
   numSigs = 0
   dim = 0
   numBins = 0
   values = {}
   numEvents = 0
   crossSection = 0.
   ## Loop over all files and process the information
   for idx, infile in enumerate(infiles):
      process.log("Reading grid from file: " + infile)
      try:
         with open(infile) as f:
            ## Get the information of number of bins and dimension.
            ## This should be the same for all files
            if idx == 0:
               numBins = int(f.readline())
               dim = int(f.readline())
            else:
               tmp = int(f.readline())
               assert numBins == tmp
               tmp = int(f.readline())
               assert dim == tmp
            ## Add together the weight, weightSq and numEvents
            if idx == 0:
               weightPrev = float(f.readline())
            else:
               tmp = float(f.readline())
               assert weightPrev == tmp
            weightNow += float(f.readline())
            if idx == 0:
               weightSqPrev = float(f.readline())
            else:
               tmp = float(f.readline())
               assert weightSqPrev == tmp
            weightSqNow += float(f.readline())
            if idx == 0:
               numEventsPrev = int(f.readline())
            else:
               tmp = float(f.readline())
               assert numEventsPrev == tmp
            numEvents += int(f.readline())
            ## Loop over the grids for each dimension
            for d in range(dim):
               for bin in range(numBins):
                  line = f.readline().split()
                  if idx == 0:
                     values[d,bin,0]=int(line[0])
                     values[d,bin,1]=float(line[1])
                     values[d,bin,2]=float(line[2])
                  else:
                     values[d,bin,0] += int(line[0])
                     values[d,bin,1] += float(line[1])
                     assert values[d,bin,2] == float(line[2])
               values[d,numBins,2]=1.
            ## Process the information for each Signature in the end
            tmp=f.readline()
            assert tmp=="###\n"
            numSigs = 0
            while True:
               sigLine = f.readline()
               if sigLine == '':
                  break
               valLine = f.readline()
               if idx == 0:
                  Signaturelines.append(sigLine)
                  XSlines.append(float(valLine))
               else:
                  assert Signaturelines[numSigs] == sigLine
                  XSlines[numSigs] += float(valLine)
               numSigs += 1
      except Exception:
         process.signal_error("In file: " + infile)
         return None, None, None, None, None

   try:
      crossSection = _write(outfile, numBins, dim, weightPrev, weightNow, weightSqPrev, weightSqNow,
                               numEventsPrev, numEvents, values, len(infiles), numSigs, Signaturelines, XSlines)
   except Exception:
      process.signal_error("Could not write to file: " + outfile)

   return dim, numBins, values, numEvents, crossSection

#-------------------------------------------------------------------------------
# Public functions
#-------------------------------------------------------------------------------

def get_grid_names(stem, first, last):
   """Extract the names and files for all grids

   Looks at the names of all files ending with the first label and removes from
   them the leading stem and trailing _first.dat. It then uses the constructed
   grid names and returns the names of the grid files for each grid with labels
   in the range [first, last].
   """
   filenames = sorted(glob.glob(stem + '*' + utils.format_tag(first) + ".dat"))
   if len(filenames) < 1:
      raise IOError("Could not find grid files with filestem: " + stem)

   begin = len(stem)
   end = -len(utils.format_tag(first) + ".dat")
   gridfiles = OrderedDict()
   for filename in filenames:
      name = filename[begin:end]
      gridfiles[name] = utils.get_filenames(stem + name, ".dat", first, last)
   return gridfiles

def combine(outstem, gridFiles, keep, backup_path, plot_iteration):
   if process.master:
      try:
         utils.ensure_dir_exists(os.path.dirname(outstem))
      except OSError:
         process.signal_exit(1)

   process.exit_if_error()

   dim = {}
   numBins = {}
   values = {}
   numEvents = {}
   crossSection = {}
   outfiles = []
   for name, infiles in list(gridFiles.items()):
      outfile = outstem + name + ".dat"
      if process.master:
         process.log("Combining grid: " + name)
         outfiles.append(outfile)

      if process.mpi:
         if process.master and len(infiles) != process.size:
            process.signal_error(
               ("Using {0} MPI processes, but need to use exactly {1}.\n"
               + "Please relaunch with 'mpirun -n {1}' (or equivalent).").format(process.size, len(infiles)),
               False)
         process.exit_if_error()
         tmpDim, tmpNumBins, tmpValues, tmpNumEvents, tmpXS = _combine_MPI(infiles, outfile)
      else:
         tmpDim, tmpNumBins, tmpValues, tmpNumEvents, tmpXS = _combine_serial(infiles, outfile)

      process.exit_if_error("Could not combine grids due to errors in one or "
                            + "more files.")

      if process.master:
         dim[name]=tmpDim
         numBins[name]=tmpNumBins
         numEvents[name]=tmpNumEvents
         crossSection[name]=tmpXS
         for d in range(dim[name]):
            for b in range(numBins[name]):
               for i in range(3):
                  values[name,d,b,i]=tmpValues[d,b,i]
            values[name,d,numBins[name],2]=tmpValues[d,numBins[name],2]

   # Write the new Plot file
   if process.master and plot_iteration:
      try:
         _plot_grids(outstem, plot_iteration, dim, numBins, values, numEvents, crossSection)
      except Exception:
         process.log_warning("Could not generate grid plots.")

   if process.master and (backup_path or not keep):
      try:
         # Collect all input files removing all duplicates and all overlaps with
         # output files.
         files = list(OrderedDict.fromkeys(
            i
            for infiles in list(gridFiles.values())
            for i in infiles
            if not any(os.path.samefile(o, i) for o in outfiles)))
         if backup_path:
            process.log("Backing up files to: " + backup_path)
            utils.backup_files(files, backup_path, keep, "input file")
            utils.backup_files(outfiles, backup_path, True, "output file")
         elif not keep:
            utils.remove_files(files, "input file")
      except Exception:
         process.signal_exit(1)

   return process.return_value()
