

#include "appl_grid/appl_pdf.h" 

#include "mcfmQQ_pdf.h"


// fortran callable wrapper
extern "C" void fmcfmBB_pdf__(const double* fA, const double* fB, double* H) { 
  static mcfmBB_pdf pdf;
  pdf.evaluate(fA, fB, H);
}
extern "C" void fmcfmCC_pdf__(const double* fA, const double* fB, double* H) { 
  static mcfmCC_pdf pdf;
  pdf.evaluate(fA, fB, H);
}
extern "C" void fmcfmTT_pdf__(const double* fA, const double* fB, double* H) { 
  static mcfmTT_pdf pdf;
  pdf.evaluate(fA, fB, H);
}

