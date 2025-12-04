#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_is_missing_idx ( SEXP x) {

  
  R_xlen_t n = Rf_xlength(x);
  int xtype = TYPEOF(x);
  if(n > 1) {
    return Rf_ScalarLogical(0);
  }
  if(xtype == NILSXP) {
    return Rf_ScalarLogical(1);
  }
  if(n == 0) {
    return Rf_ScalarLogical(0);
  }
  if(xtype == INTSXP) {
    if(INTEGER(x)[0] == 0) {
      return Rf_ScalarLogical(1);
    }
  }
  if(xtype == REALSXP) {
    if(REAL(x)[0] == 0) {
      return Rf_ScalarLogical(1);
    }
  }
  
  return Rf_ScalarLogical(0);


}