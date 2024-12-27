#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_any_badmargin ( SEXP x, SEXP val ) {
 
R_xlen_t n = Rf_xlength(x);

switch(TYPEOF(x)) {
  case INTSXP:
    {
      
      int v;
      v = Rf_asInteger(val);
      const int *px = INTEGER_RO(x);
      for(R_xlen_t i = 0; i != n; ++i) { 
        if(px[i] < 0 || px[i] > v) return Rf_ScalarLogical(1);
      }
      return Rf_ScalarLogical(0);
      break;
    }
  
  case REALSXP: 
    {
      
      double v;
      v = Rf_asReal(val);
      const double *px = REAL_RO(x);
      for(R_xlen_t i = 0; i != n; ++i) { 
        if(px[i] < 0 || px[i] > v) return Rf_ScalarLogical(1);
      }
      return Rf_ScalarLogical(0);
      break;
    }
  
  default: error("unsupported type");
}


return(R_NilValue);

}
