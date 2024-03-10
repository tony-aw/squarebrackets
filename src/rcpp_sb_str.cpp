#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sb_str)]]
std::string rcpp_sb_str(std::string x, IntegerVector ind) {
  std::string out = "";
  int n = ind.size();
  for(int i = 0; i < n; ++i) {
    if(ind[i] < 0) stop("`ind` must be a vector of strictly positive integers");
    out += x.at(ind[i]);
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sb_str_rp)]]
std::string rcpp_sb_str_rp(std::string x, IntegerVector ind, std::string rp_x, IntegerVector rp_ind) {
  R_xlen_t n = ind.length();
  for(R_xlen_t i = 0; i < n; ++i) {
    if(ind[i] < 0 || rp_ind[i] < 0) stop("both indices must be strictly positive integers");
  }  

  for(R_xlen_t i = 0; i < n; ++i) {
    x.at(ind[i]) = rp_x.at(rp_ind[i]);
  }
  return x;
}

