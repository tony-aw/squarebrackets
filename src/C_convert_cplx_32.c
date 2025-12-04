#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_convert_cplx_32 ( SEXP x, SEXP val ) {

R_xlen_t n = xlength(x);

SEXP res = PROTECT(allocVector(INTSXP, n));
int *pres = INTEGER(res);

const Rcomplex *px = COMPLEX_RO(x);

if(Rf_length(val) == 1) {
  
  int v;
  v = asInteger(val);
  
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
  const int *pv = INTEGER_RO(val);
  
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