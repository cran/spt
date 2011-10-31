/*
 *  Copyright (C) 2009-2010  B. Wang
 *  Unlimited use and distribution (see LICENCE).
 */

#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void stri(double *xy, double *result);
//void reemnorm(double *xy, double *result);
void SPTChild2(double *xy, double *result);
void SPTChild3(double *xy, double *result);
void PTChild(double *xy, double *result);
void SptDimFP(double *angles, double *dim);

static const R_FortranMethodDef FortEntries[] = {
  //  {"maxedge", (DL_FUNC) &maxedge, 2},
  {"stri", (DL_FUNC) &stri, 2},
  {"SPTChild2", (DL_FUNC) &SPTChild2, 2},
  {"SPTChild3", (DL_FUNC) &SPTChild3, 2},
  {"PTChild", (DL_FUNC) &PTChild, 2},
  {"SptDimFP", (DL_FUNC) &SptDimFP, 2},

  {NULL, NULL, 0}
};


void R_init_spt(DllInfo *dll)
{
  //    R_registerRoutines(dll, NULL, NULL, callMethods, NULL);
  R_registerRoutines(dll, NULL, NULL, FortEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
