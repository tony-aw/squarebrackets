

#include <Rcpp.h>

using namespace Rcpp;










//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_2d)]]
IntegerVector rcpp_sub2ind_2d(
  IntegerVector ind1, IntegerVector ind2, IntegerVector dimcumprod
) {

int ni = ind1.length();
int nj = ind2.length();
R_xlen_t counter = 0;
IntegerVector flatind(ni * nj);
  
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
  
      flatind[counter] = ind1[i] + dimcumprod[0] * (ind2[j] - 1);
      counter++;
    
	 }
	 }

return flatind;

}







//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_3d)]]
IntegerVector rcpp_sub2ind_3d(
  IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector dimcumprod
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
R_xlen_t counter = 0;
IntegerVector flatind(ni * nj * nk);
  
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
  
      flatind[counter] = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
      counter++;
    
	 }
	 }
	 }

return flatind;

}







//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_4d)]]
IntegerVector rcpp_sub2ind_4d(
  IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector dimcumprod
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
R_xlen_t counter = 0;
IntegerVector flatind(ni * nj * nk * nl);
  
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
  
      flatind[counter] = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1);
      counter++;
    
	 }
	 }
	 }
	 }

return flatind;

}







//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_5d)]]
IntegerVector rcpp_sub2ind_5d(
  IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, IntegerVector dimcumprod
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
R_xlen_t counter = 0;
IntegerVector flatind(ni * nj * nk * nl * nm);
  
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
  
      flatind[counter] = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1);
      counter++;
    
	 }
	 }
	 }
	 }
	 }

return flatind;

}







//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_6d)]]
IntegerVector rcpp_sub2ind_6d(
  IntegerVector ind1, IntegerVector ind2, IntegerVector ind3, IntegerVector ind4, IntegerVector ind5, IntegerVector ind6, IntegerVector dimcumprod
) {

int ni = ind1.length();
int nj = ind2.length();
int nk = ind3.length();
int nl = ind4.length();
int nm = ind5.length();
int nn = ind6.length();
R_xlen_t counter = 0;
IntegerVector flatind(ni * nj * nk * nl * nm * nn);
  
	 for(int n = 0; n < nn; ++n) {
	 for(int m = 0; m < nm; ++m) {
	 for(int l = 0; l < nl; ++l) {
	 for(int k = 0; k < nk; ++k) {
	 for(int j = 0; j < nj; ++j) {
	 for(int i = 0; i < ni; ++i) {
  
      flatind[counter] = ind1[i] + dimcumprod[0] * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1) + dimcumprod[2] * (ind4[l] - 1) + dimcumprod[3] * (ind5[m] - 1) + dimcumprod[4] * (ind6[n] - 1);
      counter++;
    
	 }
	 }
	 }
	 }
	 }
	 }

return flatind;

}



