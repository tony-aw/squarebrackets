

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_sub2ind_2d(
  const SEXP ind1, const SEXP ind2, const SEXP dimcumprod
) {

int ni = Rf_length(ind1);
int nj = Rf_length(ind2);

R_xlen_t counter = 0;
double temp = 0.0;

int *pi; 
 pi = INTEGER(ind1);
int *pj; 
 pj = INTEGER(ind2);
double pdim1 = REAL(dimcumprod)[0]; 



double *pout;
SEXP out = PROTECT(allocVector(REALSXP, ni * nj));
pout = REAL(out);
  
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      temp = pi[i] + pdim1 * (pj[j] - 1);
      pout[counter] = temp;
      counter++;
    
	 }
	 }

UNPROTECT(1);

return out;

}



