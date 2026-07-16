#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_any_zerolen ( SEXP x ) {
  
  const R_xlen_t n = Rf_xlength(x);
  
  if(n == 0) {
    return(ScalarLogical(1));
  }
  
  for(R_xlen_t i = 0; i < n; ++i) {
    if(Rf_xlength(VECTOR_ELT(x, i)) == 0) {
      return ScalarLogical(1);
    }
  }
  return ScalarLogical(0);
}
