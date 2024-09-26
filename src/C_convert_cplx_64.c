#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_convert_cplx_64 ( SEXP x, SEXP val ) {



  
  R_xlen_t n = xlength(x);
  
  SEXP res = PROTECT(allocVector(REALSXP, n));
  double *pres;
  pres = REAL(res);
  
  const double *px;
  px = REAL(x);
  
  if(Rf_length(val) == 1) {
    
    double v;
    v = asReal(val);
    
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
    const double *pv;
    pv = REAL(val);
    
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