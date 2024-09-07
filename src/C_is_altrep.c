#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_is_altrep( SEXP x ) {
     return(Rf_ScalarLogical(ALTREP(x)));
}