

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_sub2ind_4d(
  const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP dimcumprod
) {

int ni = Rf_length(ind1);
int nj = Rf_length(ind2);
int nk = Rf_length(ind3);
int nl = Rf_length(ind4);

R_xlen_t counter = 0;
int temp = 0;

int *pi; 
 pi = INTEGER(ind1);
int *pj; 
 pj = INTEGER(ind2);
int *pk; 
 pk = INTEGER(ind3);
int *pl; 
 pl = INTEGER(ind4);
int pdim1 = INTEGER(dimcumprod)[0]; 

int pdim2 = INTEGER(dimcumprod)[1]; 

int pdim3 = INTEGER(dimcumprod)[2]; 



int *pout;
SEXP out = PROTECT(allocVector(INTSXP, ni * nj * nk * nl));
pout = INTEGER(out);
  
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      temp = pi[i] + pdim1 * (pj[j] - 1) + pdim2 * (pk[k] - 1) + pdim3 * (pl[l] - 1);
      pout[counter] = temp;
      counter++;
    
	 }
	 }
	 }
	 }

UNPROTECT(1);

return out;

}


