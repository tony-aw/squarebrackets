#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_all_dim_zero ( SEXP x ) {

  R_xlen_t n = xlength(x);
  const int *px = INTEGER_RO(x);
  for(R_xlen_t i = 0; i != n; ++i) { 
    if(px[i] != 0) return ScalarLogical(0);
  }
  return ScalarLogical(1);

}