#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_serial ( SEXP x ) {

double type = TYPEOF(x);

double maximum = pow(2, 31) - 1;
double len = Rf_xlength(x);
double oversized = len - maximum;

double *pres;
SEXP res = PROTECT(Rf_allocVector(REALSXP, 5));
pres = REAL(res);

pres[0] = type;
pres[1] = len;
pres[2] = oversized;
pres[3] = 0;
pres[4] = 202326021992;

UNPROTECT(1);

return(res);

}
