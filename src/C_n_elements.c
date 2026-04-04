#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_n_elements ( SEXP x ) {
  
  R_xlen_t n = Rf_xlength(x);
  return ScalarReal(n);
}