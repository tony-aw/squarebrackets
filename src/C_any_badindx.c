#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

// Inspired by collapse::anyv, though this function works differently

SEXP C_any_badindx ( SEXP x, SEXP val ) {
  R_xlen_t n = xlength(x);
  const int *px = INTEGER(x);
  int v;
  v = asInteger(val);
  for(int i = 0; i != n; ++i) { 
    if(px[i] < 1 || px[i] > v) return ScalarLogical(1);
  }
  return ScalarLogical(0);
}