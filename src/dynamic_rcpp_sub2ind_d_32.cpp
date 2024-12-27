

#include <Rcpp.h>

using namespace Rcpp;






//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_sub2ind_2d_32)]]
SEXP C_sub2ind_2d_32(
  const SEXP ind1, const SEXP ind2, SEXP dimcumprod
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
const int *pind1;
pind1 = INTEGER_RO(ind1);

const int *pind2;
pind2 = INTEGER_RO(ind2);


R_xlen_t counter = 0;
int temp = 0;

int *pdim;
pdim = INTEGER(dimcumprod);

int *pout;
SEXP out = PROTECT(Rf_allocVector(INTSXP, len1 * len2));
pout = INTEGER(out);
  
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      
      temp = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      pout[counter] = temp;
      counter++;
      
	 }
	 }

UNPROTECT(1);
return out;

}







//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_sub2ind_3d_32)]]
SEXP C_sub2ind_3d_32(
  const SEXP ind1, const SEXP ind2, const SEXP ind3, SEXP dimcumprod
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
const int *pind1;
pind1 = INTEGER_RO(ind1);

const int *pind2;
pind2 = INTEGER_RO(ind2);

const int *pind3;
pind3 = INTEGER_RO(ind3);


R_xlen_t counter = 0;
int temp = 0;

int *pdim;
pdim = INTEGER(dimcumprod);

int *pout;
SEXP out = PROTECT(Rf_allocVector(INTSXP, len1 * len2 * len3));
pout = INTEGER(out);
  
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      
      temp = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      pout[counter] = temp;
      counter++;
      
	 }
	 }
	 }

UNPROTECT(1);
return out;

}







//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_sub2ind_4d_32)]]
SEXP C_sub2ind_4d_32(
  const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, SEXP dimcumprod
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
const int *pind1;
pind1 = INTEGER_RO(ind1);

const int *pind2;
pind2 = INTEGER_RO(ind2);

const int *pind3;
pind3 = INTEGER_RO(ind3);

const int *pind4;
pind4 = INTEGER_RO(ind4);


R_xlen_t counter = 0;
int temp = 0;

int *pdim;
pdim = INTEGER(dimcumprod);

int *pout;
SEXP out = PROTECT(Rf_allocVector(INTSXP, len1 * len2 * len3 * len4));
pout = INTEGER(out);
  
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      
      temp = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      pout[counter] = temp;
      counter++;
      
	 }
	 }
	 }
	 }

UNPROTECT(1);
return out;

}







//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_sub2ind_5d_32)]]
SEXP C_sub2ind_5d_32(
  const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, SEXP dimcumprod
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
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


R_xlen_t counter = 0;
int temp = 0;

int *pdim;
pdim = INTEGER(dimcumprod);

int *pout;
SEXP out = PROTECT(Rf_allocVector(INTSXP, len1 * len2 * len3 * len4 * len5));
pout = INTEGER(out);
  
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      
      temp = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      pout[counter] = temp;
      counter++;
      
	 }
	 }
	 }
	 }
	 }

UNPROTECT(1);
return out;

}







//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_sub2ind_6d_32)]]
SEXP C_sub2ind_6d_32(
  const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, SEXP dimcumprod
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
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


R_xlen_t counter = 0;
int temp = 0;

int *pdim;
pdim = INTEGER(dimcumprod);

int *pout;
SEXP out = PROTECT(Rf_allocVector(INTSXP, len1 * len2 * len3 * len4 * len5 * len6));
pout = INTEGER(out);
  
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      
      temp = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
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







//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_sub2ind_7d_32)]]
SEXP C_sub2ind_7d_32(
  const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, SEXP dimcumprod
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
int len7 = Rf_length(ind7);
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


R_xlen_t counter = 0;
int temp = 0;

int *pdim;
pdim = INTEGER(dimcumprod);

int *pout;
SEXP out = PROTECT(Rf_allocVector(INTSXP, len1 * len2 * len3 * len4 * len5 * len6 * len7));
pout = INTEGER(out);
  
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      
      temp = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      pout[counter] = temp;
      counter++;
      
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







//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_sub2ind_8d_32)]]
SEXP C_sub2ind_8d_32(
  const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, const SEXP ind8, SEXP dimcumprod
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
int len7 = Rf_length(ind7);
int len8 = Rf_length(ind8);
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


R_xlen_t counter = 0;
int temp = 0;

int *pdim;
pdim = INTEGER(dimcumprod);

int *pout;
SEXP out = PROTECT(Rf_allocVector(INTSXP, len1 * len2 * len3 * len4 * len5 * len6 * len7 * len8));
pout = INTEGER(out);
  
	 for(int iter8 = 0; iter8 < len8; ++iter8) {

	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      
      temp = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1) + pdim[6] * (pind8[iter8] - 1);
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

UNPROTECT(1);
return out;

}







//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_2d_8d_32)]]
SEXP rcpp_sub2ind_2d_8d_32(
  List sub, SEXP dimcumprod
) {
  int n = sub.length();
  
  SEXP ind1 = sub[0];
  SEXP ind2 = sub[1];
  SEXP ind3;
  SEXP ind4;
  SEXP ind5;
  SEXP ind6;
  SEXP ind7;
  SEXP ind8;
  SEXP out;

  if(n > 2) {
    ind3 = sub[2];
    if(n > 3) {
      ind4 = sub[3];
      if(n > 4) {
        ind5 = sub[4];
        if(n > 5) {
          ind6 = sub[5];
          if(n > 6) {
            ind7 = sub[6];
            if(n > 7) {
              ind8 = sub[7];
            }
          }
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      out = C_sub2ind_2d_32(
        ind1, ind2,
        dimcumprod
      );
      break;
    case 3:
      out = C_sub2ind_3d_32(
        ind1, ind2, ind3,
        dimcumprod
      );
      break;
    case 4:
      out = C_sub2ind_4d_32(
        ind1, ind2, ind3, ind4,
        dimcumprod
      );
      break;
    case 5:
      out = C_sub2ind_5d_32(
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod
      );
      break;
    case 6:
      out = C_sub2ind_6d_32(
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod
      );
      break;
    case 7:
      out = C_sub2ind_7d_32(
        ind1, ind2, ind3, ind4, ind5, ind6, ind7,
        dimcumprod
      );
      break;
    case 8:
      out = C_sub2ind_8d_32(
        ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8,
        dimcumprod
      );
      break;
  }
  
  return out;
}




