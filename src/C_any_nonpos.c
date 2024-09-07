#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_any_nonpos ( SEXP x ) {

R_xlen_t n = xlength(x);
switch(TYPEOF(x)) {
  case INTSXP:
    {
      const int *px = INTEGER(x);
      if(ALTREP(x)) {
        if(px[0] < 1) return Rf_ScalarLogical(1);
        if(px[n-1] < 1) return Rf_ScalarLogical(1);
        return Rf_ScalarLogical(0);
        break;
      }

      for(R_xlen_t i = 0; i != n; ++i) { 
        if(px[i] < 1) return ScalarLogical(1);
      }
      return ScalarLogical(0);
      break;
    }
  
  
  case REALSXP: 
    {
      const double *px = REAL(x);
      if(ALTREP(x)) {
        if(px[0] < 1) return Rf_ScalarLogical(1);
        if(px[n-1] < 1) return Rf_ScalarLogical(1);
        return Rf_ScalarLogical(0);
        break;
      }

      for(R_xlen_t i = 0; i != n; ++i) { 
        if(px[i] < 1) return ScalarLogical(1);
      }
      return ScalarLogical(0);
      break;
    }
  default: error("unsupported type");
}
return(R_NilValue);


}