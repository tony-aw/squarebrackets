#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_make_NA ( SEXP x ) {
  
  if(TYPEOF(x) == LGLSXP) {
    return(ScalarLogical(NA_LOGICAL));
  }
  if(TYPEOF(x) == INTSXP) {
    return(ScalarInteger(NA_INTEGER));
  }
  if(TYPEOF(x) == REALSXP) {
    return(ScalarReal(NA_REAL));
  }
  if(TYPEOF(x) == CPLXSXP) {
    Rcomplex out;
    out.r = NA_REAL;
    out.i = NA_REAL;
    return(ScalarComplex(out));
  }
  if(TYPEOF(x) == STRSXP) {
    return(ScalarString(NA_STRING));
  }
  
  return R_NilValue;
}