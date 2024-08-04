

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_sub2ind_6d(
  SEXP ind1, SEXP ind2, SEXP ind3, SEXP ind4, SEXP ind5, SEXP ind6, const SEXP dimcumprod
) {

int ni = Rf_length(ind1);
int nj = Rf_length(ind2);
int nk = Rf_length(ind3);
int nl = Rf_length(ind4);
int nm = Rf_length(ind5);
int nn = Rf_length(ind6);

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
int *pm; 
 pm = INTEGER(ind5);
int *pn; 
 pn = INTEGER(ind6);

int *pdim;
pdim = INTEGER(dimcumprod);

int *pout;
SEXP out = PROTECT(allocVector(INTSXP, ni * nj * nk * nl * nm * nn));
pout = INTEGER(out);
  
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
      temp = pi[i] + pdim[0] * (pj[j] - 1) + pdim[1] * (pk[k] - 1) + pdim[2] * (pl[l] - 1) + pdim[3] * (pm[m] - 1) + pdim[4] * (pn[n] - 1);
      pout[counter] = temp;
      counter++;
    
	 }
	 }
	 }
	 }
	 }
	 }

UNPROTECT(1);

return out;

}



