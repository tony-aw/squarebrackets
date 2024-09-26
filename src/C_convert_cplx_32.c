#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_convert_cplx_32 ( SEXP x, SEXP val ) {


  R_xlen_t n = xlength(x);
  
  SEXP res = PROTECT(allocVector(INTSXP, n));
  int *pres;
  pres = INTEGER(res);
  
  const double *px;
  px = REAL(x);
  
  if(Rf_length(val) == 1) {
    
    int v;
    v = asInteger(val);
    
    for(int i = 0; i != n; ++i) {
      if(px[i] < 0) {
        pres[i] = v + px[i] + 1;
      }
      else {
        pres[i] = px[i];
      }
    }
    
  }
  else if(Rf_length(val) == n) {
    const int *pv;
    pv = INTEGER(val);
    
    for(int i = 0; i != n; ++i) {
      if(px[i] < 0) {
        pres[i] = pv[i] + px[i] + 1;
      }
      else {
        pres[i] = px[i];
      }
    }
  }
  else {
    error("unsupported type");
  }
  
  UNPROTECT(1);
  return(res);
  


}