#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>


SEXP C_altrep_attr ( SEXP x ) {
  
  return ATTRIB(ALTREP_CLASS(x));
  
}