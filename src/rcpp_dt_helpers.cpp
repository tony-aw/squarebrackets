#include <Rcpp.h>

using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_dt_needcoe)]]
bool rcpp_dt_needcoe(
  List x, IntegerVector col, List rp
) {
  
  int n = Rf_length(col);
  
  RObject tempx;
  RObject temprp;
  CharacterVector classx(1);
  CharacterVector classrp(1);
  
  for(int i = 0; i < n; ++i) {
    if(TYPEOF(x[col[i] - 1]) != TYPEOF(rp[i])) {
      return true;
    }
    
    tempx = x[col[i] - 1];
    temprp = rp[i];
    if(tempx.hasAttribute("class") != temprp.hasAttribute("class")) {
      return true;
    }
    if(tempx.hasAttribute("class") && temprp.hasAttribute("class")) {
      classx = tempx.attr("class");
      classrp = temprp.attr("class");
      if(classx[0] != classrp[0]) {
        return true;
      }
    }
  }
  
  return false;
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_dt_find_name)]]
R_xlen_t rcpp_dt_find_name(
  SEXP x, SEXP v, int dir
) {
  const SEXP *px = STRING_PTR_RO(x);
  const SEXP *pv = STRING_PTR_RO(v);
  R_xlen_t n = Rf_xlength(x);
  
  if(dir == 1) {
    for(int i = 0; i < n; ++i) {
      if(R_compute_identical(px[i], pv[0], 0)) {
        return(i + 1);
      }
    }
    return 0;
  }
  else if(dir == -1) {
    for(int i = (n - 1); i >= 0; --i) {
      if(R_compute_identical(px[i], pv[0], 0)) {
        return(i + 1);
      }
    }
    return 0;
  }
  
  return 0;
  
}
