#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_any_address ( SEXP x, SEXP v ) {


R_xlen_t n = Rf_xlength(x);
const SEXP *px = STRING_PTR_RO(x);
const SEXP pv = STRING_ELT(v, 0);

for(R_xlen_t i = 0; i < n; ++i) {
  if(R_compute_identical(px[i], pv, 0)) {
    return ScalarLogical(1);
  }
}

return ScalarLogical(0);


}