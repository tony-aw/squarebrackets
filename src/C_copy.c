#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_copy(SEXP x)
{
  return(duplicate(x));
}