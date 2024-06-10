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
#    Functions for handling NERSC slurm queues
#-------------------------------------------------------------------------------

import sys
import math


def checkHostName(machine):
   """Get the name of the host we are running (only works for Cori-Haswell, Cori-KNL and Edison)"""
   allowedMachines = ["edison","cori-haswell","cori-knl"]
   if not machine in allowedMachines:
      print "Machine "+machine+" not supported. Can only run on the following systems: "+str(allowedMachines)
      sys.exit(1)

def checkQueue(machine,queue):
   """Get the name of the queue we are running
      (only works for queues debug, regular, premium)
   """
   if machine in ["edison", "cori-haswell"," cori-knl"]:
      allowedQueues = ["regular", "premium", "debug"]
      if not queue in allowedQueues:
         print "Queue "+queue+" not supported. Can only run on the following queues: "+str(allowedQueues)
         sys.exit(1)

def getTimeString(machine,queue,max_time):
   """Get the timestring to put on sbatch
      (checks that the amount of time does not exceed the allowed time)
   """
   #Get the time information
   if (queue == "debug") and (max_time > 30):
      print "Can only run up to 30 minutes on the debug queue."
      sys.exit(1)
   else:
      if (machine == "edison") and (max_time > 2160):
         print "On Edison can only run up to 36 hours on the regular or premium queues."
         sys.exit(1)
      elif (machine[:4] == "cori") and (max_time > 2880):
         print "On Cori can only run up to 48 hours on the regular or premium queues."
         sys.exit(1)
   hours = int(max_time / 60)
   minutes = max_time % 60
   timestring = str(hours).zfill(2) + ":" + str(minutes).zfill(2) + ":00"
   return timestring

def getNumNodes(machine, numCores):
   """Get the number of nodes required for the run"""
   numCoresPerNode = 0
   if machine == "cori-haswell":
      numCoresPerNode = 32
   elif machine == "cori-knl":
      numCoresPerNode = 68
   elif machine == "edison":
      numCoresPerNode = 24
   else:
      print "This script only works for Edison and Cori-Haswell or Cori-KNL."
      sys.exit(1)
   numNodes = int(math.ceil(float(numCores) / numCoresPerNode))
   return numNodes

def writeSlurmFile(slurmfile, machine, queue, license, numNodes, timestring, command):
   with open(slurmfile, 'w') as f:
      f.write("#!/bin/bash -l\n")
      f.write("#SBATCH -q "+queue+"\n")
      if machine == "cori-haswell":
         f.write("#SBATCH -C haswell\n")
      if machine == "cori-knl":
         f.write("#SBATCH -C knl\n")
      f.write("#SBATCH -N " + str(numNodes) + "\n")
      f.write("#SBATCH -t " + timestring + "\n")
      f.write("#SBATCH -J " + slurmfile.replace('submit','',1).replace('.sl','',1) + "\n")
      if license != None:
         f.write("#SBATCH -L "+",".join(license)+ "\n")
      f.write("#OpenMP settings:\n")
      f.write("export OMP_NUM_THREADS=1\n")
      f.write("export OMP_PLACES=threads\n")
      f.write("export OMP_PROC_BIND=spread\n")
      f.write("cd \"$SLURM_SUBMIT_DIR\"\n")
      f.write(" ".join(str(x) for x in command) + "\n")
      f.write("rm "+slurmfile+"\n")
