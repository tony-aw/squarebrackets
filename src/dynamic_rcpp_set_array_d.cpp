

#include <Rcpp.h>

using namespace Rcpp;







//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_Logical)]]
void rcpp_set_array_2d_Logical(
  LogicalVector x, const SEXP ind1, const SEXP ind2, SEXP dimcumprod, LogicalVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2)) {
R_xlen_t counter = 0;
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_Logical)]]
void rcpp_set_array_3d_Logical(
  LogicalVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, SEXP dimcumprod, LogicalVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3)) {
R_xlen_t counter = 0;
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_Logical)]]
void rcpp_set_array_4d_Logical(
  LogicalVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, SEXP dimcumprod, LogicalVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4)) {
R_xlen_t counter = 0;
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_Logical)]]
void rcpp_set_array_5d_Logical(
  LogicalVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, SEXP dimcumprod, LogicalVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5)) {
R_xlen_t counter = 0;
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_Logical)]]
void rcpp_set_array_6d_Logical(
  LogicalVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, SEXP dimcumprod, LogicalVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6)) {
R_xlen_t counter = 0;
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_7d_Logical)]]
void rcpp_set_array_7d_Logical(
  LogicalVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, SEXP dimcumprod, LogicalVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
int len7 = Rf_length(ind7);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);

const int *pind7;
pind7 = INTEGER(ind7);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6 * len7)) {
R_xlen_t counter = 0;
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_Integer)]]
void rcpp_set_array_2d_Integer(
  IntegerVector x, const SEXP ind1, const SEXP ind2, SEXP dimcumprod, IntegerVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2)) {
R_xlen_t counter = 0;
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_Integer)]]
void rcpp_set_array_3d_Integer(
  IntegerVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, SEXP dimcumprod, IntegerVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3)) {
R_xlen_t counter = 0;
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_Integer)]]
void rcpp_set_array_4d_Integer(
  IntegerVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, SEXP dimcumprod, IntegerVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4)) {
R_xlen_t counter = 0;
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_Integer)]]
void rcpp_set_array_5d_Integer(
  IntegerVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, SEXP dimcumprod, IntegerVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5)) {
R_xlen_t counter = 0;
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_Integer)]]
void rcpp_set_array_6d_Integer(
  IntegerVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, SEXP dimcumprod, IntegerVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6)) {
R_xlen_t counter = 0;
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_7d_Integer)]]
void rcpp_set_array_7d_Integer(
  IntegerVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, SEXP dimcumprod, IntegerVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
int len7 = Rf_length(ind7);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);

const int *pind7;
pind7 = INTEGER(ind7);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6 * len7)) {
R_xlen_t counter = 0;
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_Numeric)]]
void rcpp_set_array_2d_Numeric(
  NumericVector x, const SEXP ind1, const SEXP ind2, SEXP dimcumprod, NumericVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2)) {
R_xlen_t counter = 0;
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_Numeric)]]
void rcpp_set_array_3d_Numeric(
  NumericVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, SEXP dimcumprod, NumericVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3)) {
R_xlen_t counter = 0;
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_Numeric)]]
void rcpp_set_array_4d_Numeric(
  NumericVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, SEXP dimcumprod, NumericVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4)) {
R_xlen_t counter = 0;
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_Numeric)]]
void rcpp_set_array_5d_Numeric(
  NumericVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, SEXP dimcumprod, NumericVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5)) {
R_xlen_t counter = 0;
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_Numeric)]]
void rcpp_set_array_6d_Numeric(
  NumericVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, SEXP dimcumprod, NumericVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6)) {
R_xlen_t counter = 0;
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_7d_Numeric)]]
void rcpp_set_array_7d_Numeric(
  NumericVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, SEXP dimcumprod, NumericVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
int len7 = Rf_length(ind7);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);

const int *pind7;
pind7 = INTEGER(ind7);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6 * len7)) {
R_xlen_t counter = 0;
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_Character)]]
void rcpp_set_array_2d_Character(
  CharacterVector x, const SEXP ind1, const SEXP ind2, SEXP dimcumprod, CharacterVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2)) {
R_xlen_t counter = 0;
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_Character)]]
void rcpp_set_array_3d_Character(
  CharacterVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, SEXP dimcumprod, CharacterVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3)) {
R_xlen_t counter = 0;
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_Character)]]
void rcpp_set_array_4d_Character(
  CharacterVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, SEXP dimcumprod, CharacterVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4)) {
R_xlen_t counter = 0;
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_Character)]]
void rcpp_set_array_5d_Character(
  CharacterVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, SEXP dimcumprod, CharacterVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5)) {
R_xlen_t counter = 0;
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_Character)]]
void rcpp_set_array_6d_Character(
  CharacterVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, SEXP dimcumprod, CharacterVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6)) {
R_xlen_t counter = 0;
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_7d_Character)]]
void rcpp_set_array_7d_Character(
  CharacterVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, SEXP dimcumprod, CharacterVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
int len7 = Rf_length(ind7);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);

const int *pind7;
pind7 = INTEGER(ind7);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6 * len7)) {
R_xlen_t counter = 0;
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_Complex)]]
void rcpp_set_array_2d_Complex(
  ComplexVector x, const SEXP ind1, const SEXP ind2, SEXP dimcumprod, ComplexVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2)) {
R_xlen_t counter = 0;
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_Complex)]]
void rcpp_set_array_3d_Complex(
  ComplexVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, SEXP dimcumprod, ComplexVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3)) {
R_xlen_t counter = 0;
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_Complex)]]
void rcpp_set_array_4d_Complex(
  ComplexVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, SEXP dimcumprod, ComplexVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4)) {
R_xlen_t counter = 0;
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_Complex)]]
void rcpp_set_array_5d_Complex(
  ComplexVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, SEXP dimcumprod, ComplexVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5)) {
R_xlen_t counter = 0;
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_Complex)]]
void rcpp_set_array_6d_Complex(
  ComplexVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, SEXP dimcumprod, ComplexVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6)) {
R_xlen_t counter = 0;
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_7d_Complex)]]
void rcpp_set_array_7d_Complex(
  ComplexVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, SEXP dimcumprod, ComplexVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
int len7 = Rf_length(ind7);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);

const int *pind7;
pind7 = INTEGER(ind7);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6 * len7)) {
R_xlen_t counter = 0;
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_Raw)]]
void rcpp_set_array_2d_Raw(
  RawVector x, const SEXP ind1, const SEXP ind2, SEXP dimcumprod, RawVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2)) {
R_xlen_t counter = 0;
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_Raw)]]
void rcpp_set_array_3d_Raw(
  RawVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, SEXP dimcumprod, RawVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3)) {
R_xlen_t counter = 0;
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_Raw)]]
void rcpp_set_array_4d_Raw(
  RawVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, SEXP dimcumprod, RawVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4)) {
R_xlen_t counter = 0;
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_Raw)]]
void rcpp_set_array_5d_Raw(
  RawVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, SEXP dimcumprod, RawVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5)) {
R_xlen_t counter = 0;
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_Raw)]]
void rcpp_set_array_6d_Raw(
  RawVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, SEXP dimcumprod, RawVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6)) {
R_xlen_t counter = 0;
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_7d_Raw)]]
void rcpp_set_array_7d_Raw(
  RawVector x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, SEXP dimcumprod, RawVector rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
int len7 = Rf_length(ind7);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);

const int *pind7;
pind7 = INTEGER(ind7);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6 * len7)) {
R_xlen_t counter = 0;
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_Logical)]]
void rcpp_set_array_2d_6d_Logical(
  LogicalVector x, List out, NumericVector dimcumprod, LogicalVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;
  IntegerVector ind7;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
          if(n > 6) {
            ind7 = out[6];
          }
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_Logical(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_Logical(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_Logical(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_Logical(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_Logical(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
    case 7:
      rcpp_set_array_7d_Logical(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6, ind7,
        dimcumprod,
        rp
      );
      break;
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_Integer)]]
void rcpp_set_array_2d_6d_Integer(
  IntegerVector x, List out, NumericVector dimcumprod, IntegerVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;
  IntegerVector ind7;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
          if(n > 6) {
            ind7 = out[6];
          }
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_Integer(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_Integer(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_Integer(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_Integer(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_Integer(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
    case 7:
      rcpp_set_array_7d_Integer(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6, ind7,
        dimcumprod,
        rp
      );
      break;
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_Numeric)]]
void rcpp_set_array_2d_6d_Numeric(
  NumericVector x, List out, NumericVector dimcumprod, NumericVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;
  IntegerVector ind7;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
          if(n > 6) {
            ind7 = out[6];
          }
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_Numeric(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_Numeric(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_Numeric(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_Numeric(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_Numeric(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
    case 7:
      rcpp_set_array_7d_Numeric(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6, ind7,
        dimcumprod,
        rp
      );
      break;
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_Character)]]
void rcpp_set_array_2d_6d_Character(
  CharacterVector x, List out, NumericVector dimcumprod, CharacterVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;
  IntegerVector ind7;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
          if(n > 6) {
            ind7 = out[6];
          }
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_Character(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_Character(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_Character(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_Character(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_Character(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
    case 7:
      rcpp_set_array_7d_Character(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6, ind7,
        dimcumprod,
        rp
      );
      break;
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_Complex)]]
void rcpp_set_array_2d_6d_Complex(
  ComplexVector x, List out, NumericVector dimcumprod, ComplexVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;
  IntegerVector ind7;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
          if(n > 6) {
            ind7 = out[6];
          }
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_Complex(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_Complex(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_Complex(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_Complex(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_Complex(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
    case 7:
      rcpp_set_array_7d_Complex(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6, ind7,
        dimcumprod,
        rp
      );
      break;
  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_Raw)]]
void rcpp_set_array_2d_6d_Raw(
  RawVector x, List out, NumericVector dimcumprod, RawVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;
  IntegerVector ind7;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
          if(n > 6) {
            ind7 = out[6];
          }
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_Raw(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_Raw(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_Raw(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_Raw(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_Raw(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
    case 7:
      rcpp_set_array_7d_Raw(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6, ind7,
        dimcumprod,
        rp
      );
      break;
  }
}


