#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_convert_cplx_64 ( SEXP x, SEXP val ) {


R_xlen_t n = xlength(x);

SEXP res = PROTECT(allocVector(REALSXP, n));
double *pres = REAL(res);

const Rcomplex *px = COMPLEX_RO(x);

if(Rf_length(val) == 1) {
  
  double v;
  v = asReal(val);
  
  for(int j = 0; j != n; ++j) {
    if(px[j].i < 0) {
      pres[j] = v + px[j].i + 1;
    }
    else {
      pres[j] = px[j].i;
    }
  }
  
}
else if(Rf_length(val) == n) {
  const double *pv = REAL_RO(val);
  
  for(int j = 0; j != n; ++j) {
    if(px[j].i < 0) {
      pres[j] = pv[j] + px[j].i + 1;
    }
    else {
      pres[j] = px[j].i;
    }
  }
}
else {
  error("recycling not allowed");
}

UNPROTECT(1);
return(res);  


}