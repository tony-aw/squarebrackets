#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_convert_bi_32 ( SEXP x, SEXP size ) {
  
  R_xlen_t n = xlength(x);
  
  SEXP res = PROTECT(allocVector(INTSXP, n));
  int *pres = INTEGER(res);
  
  int by = 0;
  if(Rf_length(size) > 1) {
    by = 1;
  }
  const int *pv = INTEGER_RO(size);
  
  if(TYPEOF(x) == INTSXP) {
    const int *px = INTEGER_RO(x);
    for(int j = 0; j != n; ++j) {
      if(px[j] < 0) {
        pres[j] = pv[j * by] + px[j] + 1;
      }
      else {
        pres[j] = px[j];
      }
    }
  }
  else if(TYPEOF(x) == REALSXP) {
    const double *px = REAL_RO(x);
    for(int j = 0; j != n; ++j) {
      if(px[j] < 0) {
        pres[j] = pv[j * by] + px[j] + 1;
      }
      else {
        pres[j] = px[j];
      }
    }
  }
  
  
  UNPROTECT(1);
  return(res);  


}