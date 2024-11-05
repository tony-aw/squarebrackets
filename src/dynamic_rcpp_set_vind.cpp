

#include <Rcpp.h>

using namespace Rcpp;



template<int RTYPE> void rcpp_set_vind_32_template(
  Vector<RTYPE> x, const SEXP ind, const Vector<RTYPE> rp
) {
  R_xlen_t n = Rf_xlength(ind);
  
  const int *pind = INTEGER(ind);
  
  if(rp.length() == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[pind[i]] = rp[i];
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[pind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_32_atomic)]]
void rcpp_set_vind_32_atomic(
  SEXP x, const SEXP ind, const SEXP rp
) {


switch(TYPEOF(x)){

  case LGLSXP:
  {
    rcpp_set_vind_32_template<LGLSXP>(as<LogicalVector>(x), ind, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_vind_32_template<INTSXP>(as<IntegerVector>(x), ind, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_vind_32_template<REALSXP>(as<NumericVector>(x), ind, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_vind_32_template<CPLXSXP>(as<ComplexVector>(x), ind, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_vind_32_template<STRSXP>(as<CharacterVector>(x), ind, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_vind_32_template<RAWSXP>(as<RawVector>(x), ind, as<RawVector>(rp));
    break;
  }

  default: stop("unsupported type given");
}
}


template<int RTYPE> void rcpp_set_vind_64_template(
  Vector<RTYPE> x, const SEXP ind, const Vector<RTYPE> rp
) {
  R_xlen_t n = Rf_xlength(ind);
  
  const double *pind = REAL(ind);
  
  if(rp.length() == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[pind[i]] = rp[i];
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[pind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_64_atomic)]]
void rcpp_set_vind_64_atomic(
  SEXP x, const SEXP ind, const SEXP rp
) {


switch(TYPEOF(x)){

  case LGLSXP:
  {
    rcpp_set_vind_64_template<LGLSXP>(as<LogicalVector>(x), ind, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_vind_64_template<INTSXP>(as<IntegerVector>(x), ind, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_vind_64_template<REALSXP>(as<NumericVector>(x), ind, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_vind_64_template<CPLXSXP>(as<ComplexVector>(x), ind, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_vind_64_template<STRSXP>(as<CharacterVector>(x), ind, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_vind_64_template<RAWSXP>(as<RawVector>(x), ind, as<RawVector>(rp));
    break;
  }

  default: stop("unsupported type given");
}
}

