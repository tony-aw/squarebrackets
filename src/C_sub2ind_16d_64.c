
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_sub2ind_16d_64(
  SEXP sub, SEXP dimcumprod
) {

SEXP ind1 = VECTOR_ELT(sub, 0);
SEXP ind2 = VECTOR_ELT(sub, 1);
SEXP ind3 = VECTOR_ELT(sub, 2);
SEXP ind4 = VECTOR_ELT(sub, 3);
SEXP ind5 = VECTOR_ELT(sub, 4);
SEXP ind6 = VECTOR_ELT(sub, 5);
SEXP ind7 = VECTOR_ELT(sub, 6);
SEXP ind8 = VECTOR_ELT(sub, 7);
SEXP ind9 = VECTOR_ELT(sub, 8);
SEXP ind10 = VECTOR_ELT(sub, 9);
SEXP ind11 = VECTOR_ELT(sub, 10);
SEXP ind12 = VECTOR_ELT(sub, 11);
SEXP ind13 = VECTOR_ELT(sub, 12);
SEXP ind14 = VECTOR_ELT(sub, 13);
SEXP ind15 = VECTOR_ELT(sub, 14);
SEXP ind16 = VECTOR_ELT(sub, 15);

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
int len7 = Rf_length(ind7);
int len8 = Rf_length(ind8);
int len9 = Rf_length(ind9);
int len10 = Rf_length(ind10);
int len11 = Rf_length(ind11);
int len12 = Rf_length(ind12);
int len13 = Rf_length(ind13);
int len14 = Rf_length(ind14);
int len15 = Rf_length(ind15);
int len16 = Rf_length(ind16);

const int *pind1;
pind1 = INTEGER_RO(ind1);

const int *pind2;
pind2 = INTEGER_RO(ind2);

const int *pind3;
pind3 = INTEGER_RO(ind3);

const int *pind4;
pind4 = INTEGER_RO(ind4);

const int *pind5;
pind5 = INTEGER_RO(ind5);

const int *pind6;
pind6 = INTEGER_RO(ind6);

const int *pind7;
pind7 = INTEGER_RO(ind7);

const int *pind8;
pind8 = INTEGER_RO(ind8);

const int *pind9;
pind9 = INTEGER_RO(ind9);

const int *pind10;
pind10 = INTEGER_RO(ind10);

const int *pind11;
pind11 = INTEGER_RO(ind11);

const int *pind12;
pind12 = INTEGER_RO(ind12);

const int *pind13;
pind13 = INTEGER_RO(ind13);

const int *pind14;
pind14 = INTEGER_RO(ind14);

const int *pind15;
pind15 = INTEGER_RO(ind15);

const int *pind16;
pind16 = INTEGER_RO(ind16);


R_xlen_t counter = 0;
double temp = 0;

const double *pdim;
pdim = REAL(dimcumprod);

double *pout;
SEXP out = PROTECT(Rf_allocVector(REALSXP, len1 * len2 * len3 * len4 * len5 * len6 * len7 * len8 * len9 * len10 * len11 * len12 * len13 * len14 * len15 * len16));
pout = REAL(out);
  
	 for(int iter16 = 0; iter16 < len16; ++iter16) {

	 for(int iter15 = 0; iter15 < len15; ++iter15) {

	 for(int iter14 = 0; iter14 < len14; ++iter14) {

	 for(int iter13 = 0; iter13 < len13; ++iter13) {

	 for(int iter12 = 0; iter12 < len12; ++iter12) {

	 for(int iter11 = 0; iter11 < len11; ++iter11) {

	 for(int iter10 = 0; iter10 < len10; ++iter10) {

	 for(int iter9 = 0; iter9 < len9; ++iter9) {

	 for(int iter8 = 0; iter8 < len8; ++iter8) {

	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      
      temp = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1) + pdim[6] * (pind8[iter8] - 1) + pdim[7] * (pind9[iter9] - 1) + pdim[8] * (pind10[iter10] - 1) + pdim[9] * (pind11[iter11] - 1) + pdim[10] * (pind12[iter12] - 1) + pdim[11] * (pind13[iter13] - 1) + pdim[12] * (pind14[iter14] - 1) + pdim[13] * (pind15[iter15] - 1) + pdim[14] * (pind16[iter16] - 1);
      pout[counter] = temp;
      counter++;
      
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }

UNPROTECT(1);
return out;

}



