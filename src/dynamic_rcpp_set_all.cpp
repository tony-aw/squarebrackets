

#include <Rcpp.h>

using namespace Rcpp;



template<int RTYPE> void rcpp_set_all_template(Vector<RTYPE> x, Vector<RTYPE> rp) {
  R_xlen_t n = x.length();

  if(rp.length() == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[i] = rp[i];
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      x[i] = rp[0];
    }
  }
  else stop("recycling not allowed");

}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_all_atomic)]]
void rcpp_set_all_atomic(
  SEXP x, const SEXP rp
) {


switch(TYPEOF(x)){

  case LGLSXP:
  {
    rcpp_set_all_template<LGLSXP>(as<LogicalVector>(x), as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_all_template<INTSXP>(as<IntegerVector>(x), as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_all_template<REALSXP>(as<NumericVector>(x), as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_all_template<CPLXSXP>(as<ComplexVector>(x), as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_all_template<STRSXP>(as<CharacterVector>(x), as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_all_template<RAWSXP>(as<RawVector>(x), as<RawVector>(rp));
    break;
  }

  default: stop("unsupported type given");
}
}

