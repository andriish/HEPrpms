#-------------------------------------------------------------------------------
# Author(s):
#    Simone Alioli, Christian Bauer
#
# Copyright:
#    Copyright (C) 2014 LBNL
#
#    This file is part of the Geneva MC framework. Geneva is distributed under
#    the terms of the GNU General Public License version 3 (GPLv3), see the
#    COPYING file that comes with this distribution for details.
#    Please respect the academic usage guidelines in the GUIDELINES file.
#
# Description:
#    Function to reweight lhef event files
#-------------------------------------------------------------------------------

import os
import shutil

from . import process
from config import geneva_use_zlib

if geneva_use_zlib:
   import gzip


def _open_files(infile, outfile):
   f = open(infile,'rb')
   if (f.read(2) == '\x1f\x8b') and geneva_use_zlib:
      f.seek(0)
      return gzip.GzipFile(fileobj=f), gzip.GzipFile(outfile, "w")
   else:
      f.seek(0)
      return f, open(outfile, "w")

def reweight(infile, mult, nweights, posmuupfix, posmudownfix,
             rwgt_fac_central, rwgt_fac_up, rwgt_fac_down, xsec_fac, extraLog, keep):
   outfile = infile + "-rwgt"
   try:
      process.log("Reweighting file: " + infile)
      inf = None
      outf = None
      inf, outf = _open_files(infile, outfile)
      while True:
         line = inf.readline()
         if line == '':
                  break
         elif '<event>' in line:
            outf.write(line)
            line = inf.readline()
            linesplit = line.split()
            npart = int(linesplit[0])
            assert npart==mult or npart==mult+1 or npart==mult+2, "Wrong multiplicity (%r) in %r" %(npart, infile)
            if npart == mult:
               rwgfact=rwgt_fac_central
            else:
               rwgfact=xsec_fac
            wgt = float(linesplit[2]) * rwgfact
            stg = '{0:.16e}'.format(wgt)
            (head,sep,tail) = stg.partition('e')
            linesplit[2] = head.rstrip('0')+sep+tail
            outf.write(str.join(" ",linesplit)+'\n')
            while True:
               line = inf.readline()
               if '<weights>' in line:
                  linesplit=line.split()
                  assert len(linesplit) >= nweights+2, "Too many (%r) weights in %r" %(len(linesplit)-2, infile)
                  if npart == mult:
                     for pos in range(1,nweights+1):
                        fac = float()
                        if (pos == posmuupfix) :
                           fac = rwgt_fac_up
                        elif (pos == posmudownfix) :
                           fac = rwgt_fac_down
                        else:
                           fac = rwgt_fac_central
                        wgt=float(linesplit[pos])*fac
                        stg='{0:.16e}'.format(wgt)
                        (head,sep,tail) = stg.partition('e')
                        linesplit[pos] = head.rstrip('0')+sep+tail
                     outf.write(str.join(" ",linesplit)+'\n')
                  else:
                     for pos in range(1,nweights+1):
                        fac = xsec_fac
                        wgt = float(linesplit[pos])*fac
                        stg = '{0:.16e}'.format(wgt)
                        (head,sep,tail) = stg.partition('e')
                        linesplit[pos] = head.rstrip('0')+sep+tail
                     outf.write(str.join(" ",linesplit)+'\n')
               else:
                  outf.write(line)
               if '</event>' in line or line == '':
                  break
         elif '## Begin Reweighting Information' in line:
               raise RuntimeError("This file has already been reweighted.")
         elif '## End Run Information' in line:
               outf.write(line)
               for line in extraLog:
                     outf.write(line)
         else:
            outf.write(line)

   except Exception:
      process.signal_error("In file: " + infile)
      try:
         os.remove(outfile)
      except OSError:
         pass
      return

   finally:
      if inf:
         inf.close()
      if outf:
         outf.close()

   if keep:
      origfile = infile + "-orig"
      try:
         shutil.move(infile, origfile)
         process.log("Kept original file as: " + origfile)
      except Exception:
         process.log_warning("Could not rename original file. Stored the"
                             + "reweighted file as: " + outfile)
         return

   try:
      shutil.move(outfile, infile)
   except Exception:
      process.log_warning("Could not overwrite original file. Stored "
                          + "reweighted file as: " + outfile)
   return
