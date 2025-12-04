#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_match_range( SEXP o, SEXP m ) {


  const int *restrict po = INTEGER(o);
  const int *restrict pm = INTEGER(m);
  
  int n = Rf_length(o);
  int countNA = 0;
  
  for(int i = 0; i < n; ++i) {
    if(pm[i] < 0) {
      countNA ++;
    }
  }

  n = n - countNA;

  SEXP res = PROTECT(Rf_allocVector(INTSXP, n));
  int *pres;
  pres = INTEGER(res);
  
  
  if(n < 100) {
    for(int i = 0; i < n; ++i) {
      pres[i] = po[i];
    }
    UNPROTECT(1);
    return res;
  }
  
  int nRemain = n % 10;
  int nDiv = n - nRemain;
  
  for(int i = 0; i < nDiv; i+=10) {
    pres[i] = po[i];
    pres[i+1] = po[i+1];
    pres[i+2] = po[i+2];
    pres[i+3] = po[i+3];
    pres[i+4] = po[i+4];
    pres[i+5] = po[i+5];
    pres[i+6] = po[i+6];
    pres[i+7] = po[i+7];
    pres[i+8] = po[i+8];
    pres[i+9] = po[i+9];
  }
  
  if(nRemain > 0) {
    for(int i = nDiv; i < n; ++i) {
      pres[i] = po[i];
    }
  }
  
  
  UNPROTECT(1);
  return res;

}