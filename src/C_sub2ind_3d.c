

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_sub2ind_3d(
  const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP dimcumprod
) {

int ni = Rf_length(ind1);
int nj = Rf_length(ind2);
int nk = Rf_length(ind3);

R_xlen_t counter = 0;
double temp = 0.0;

int *restrict pi; 
 pi = INTEGER(ind1);
int *restrict pj; 
 pj = INTEGER(ind2);
int *restrict pk; 
 pk = INTEGER(ind3);

double *restrict pdim;
pdim = REAL(dimcumprod);


double *restrict pout;
SEXP out = PROTECT(allocVector(REALSXP, ni * nj * nk));
pout = REAL(out);
  
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      temp = pi[i] + pdim[0] * (pj[j] - 1) + pdim[1] * (pk[k] - 1);
      pout[counter] = temp;
      counter++;
    
	 }
	 }
	 }

UNPROTECT(1);

return out;

}



