

#include <Rcpp.h>

#include "squarebrackets.h"

using namespace Rcpp;




template<int RTYPE> void rcpp_set_array_d_template(
  Vector<RTYPE> x, SEXP sub, SEXP dimcumprod, Vector<RTYPE> rp
) {


R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

if(rp.length() == n) {
  R_xlen_t counter = 0;
  MACRO_DIM_DOCALL(MACRO_SETARRAY0);
}
else if(rp.length() == 1) {
  MACRO_DIM_DOCALL(MACRO_SETARRAY1);
}
else stop("recycling not allowed");

}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_d_atomic)]]
void rcpp_set_array_DTYPEd_atomic(
  SEXP x, SEXP sub, SEXP dimcumprod, const SEXP rp
) {
  
  
  switch(TYPEOF(x)){
    
  case LGLSXP:
  {
    rcpp_set_array_d_template<LGLSXP>(as<LogicalVector>(x), sub, dimcumprod, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_array_d_template<INTSXP>(as<IntegerVector>(x), sub, dimcumprod, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_array_d_template<REALSXP>(as<NumericVector>(x), sub, dimcumprod, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_array_d_template<CPLXSXP>(as<ComplexVector>(x), sub, dimcumprod, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_array_d_template<STRSXP>(as<CharacterVector>(x), sub, dimcumprod, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_array_d_template<RAWSXP>(as<RawVector>(x), sub, dimcumprod, as<RawVector>(rp));
    break;
  }

  }
}


