

#include <Rcpp.h>

#include "squarebrackets.h"

using namespace Rcpp;







inline void rcpp_set_all_Logical(
  SEXP x, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(x);
  
   int *px = LOGICAL(x);
  const int *prp = LOGICAL_RO(rp);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, i, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_all_Integer(
  SEXP x, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(x);
  
   int *px = INTEGER(x);
  const int *prp = INTEGER_RO(rp);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, i, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_all_Numeric(
  SEXP x, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(x);
  
   double *px = REAL(x);
  const double *prp = REAL_RO(rp);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, i, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_all_Complex(
  SEXP x, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(x);
  
   Rcomplex *px = COMPLEX(x);
  const Rcomplex *prp = COMPLEX_RO(rp);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, i, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_all_Raw(
  SEXP x, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(x);
  
   Rbyte *px = RAW(x);
  const Rbyte *prp = RAW_RO(rp);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, i, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_all_Character(
  SEXP x, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(x);
  
  const SEXP *prp = STRING_PTR_RO(rp);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      SET_STRING_ELT(x, i, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      SET_STRING_ELT(x, i, prp[0]);
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
         rcpp_set_all_Logical(x, rp);
        break;
      }
      case INTSXP:
      {
         rcpp_set_all_Integer(x, rp);
        break;
      }
      case REALSXP:
      {
         rcpp_set_all_Numeric(x, rp);
        break;
      }
      case CPLXSXP:
      {
         rcpp_set_all_Complex(x, rp);
        break;
      }
      case RAWSXP:
      {
         rcpp_set_all_Raw(x, rp);
        break;
      }
      case STRSXP:
      {
         rcpp_set_all_Character(x, rp);
        break;
      }
      default: stop("unsupported type given");
    }
    
  

}



