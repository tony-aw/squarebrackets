#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_any_badmargin ( SEXP x, SEXP val ) {
 R_xlen_t n = xlength(x);
 
switch(TYPEOF(val)) {
  case INTSXP:
    {
      const int *px = INTEGER(x);
      int v;
      v = asInteger(val);
      for(R_xlen_t i = 0; i != n; ++i) { 
        if(px[i] < 0 || px[i] > v) return ScalarLogical(1);
      }
      return ScalarLogical(0);
      break;
    }

  case REALSXP: 
    {
      const double *px = REAL(x);
      double v;
      v = asReal(val);
      for(R_xlen_t i = 0; i != n; ++i) { 
        if(px[i] < 0 || px[i] > v) return ScalarLogical(1);
      }
      return ScalarLogical(0);
      break;
    }

  default: error("unsupported type");
 }

 return(R_NilValue);
}