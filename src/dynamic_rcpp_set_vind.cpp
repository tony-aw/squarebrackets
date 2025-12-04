

#include <Rcpp.h>

#include "squarebrackets.h"

using namespace Rcpp;







inline void rcpp_set_vind_32_Logical(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
   int *px = LOGICAL(x);
  const int *prp = LOGICAL_RO(rp);
  const int *pind = INTEGER_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, pind[i] - 1, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, pind[i] - 1, prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_vind_32_Integer(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
   int *px = INTEGER(x);
  const int *prp = INTEGER_RO(rp);
  const int *pind = INTEGER_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, pind[i] - 1, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, pind[i] - 1, prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_vind_32_Numeric(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
   double *px = REAL(x);
  const double *prp = REAL_RO(rp);
  const int *pind = INTEGER_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, pind[i] - 1, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, pind[i] - 1, prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_vind_32_Complex(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
   Rcomplex *px = COMPLEX(x);
  const Rcomplex *prp = COMPLEX_RO(rp);
  const int *pind = INTEGER_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, pind[i] - 1, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, pind[i] - 1, prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_vind_32_Raw(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
   Rbyte *px = RAW(x);
  const Rbyte *prp = RAW_RO(rp);
  const int *pind = INTEGER_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, pind[i] - 1, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, pind[i] - 1, prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_vind_32_Character(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
  // SEXP *px = STRING_PTR(x);
  const SEXP *prp = STRING_PTR_RO(rp);
  const int *pind = INTEGER_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      SET_STRING_ELT(x, pind[i] - 1, prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      SET_STRING_ELT(x, pind[i] - 1, prp[0]);
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
         rcpp_set_vind_32_Logical(x, ind, rp);
        break;
      }
      case INTSXP:
      {
         rcpp_set_vind_32_Integer(x, ind, rp);
        break;
      }
      case REALSXP:
      {
         rcpp_set_vind_32_Numeric(x, ind, rp);
        break;
      }
      case CPLXSXP:
      {
         rcpp_set_vind_32_Complex(x, ind, rp);
        break;
      }
      case RAWSXP:
      {
         rcpp_set_vind_32_Raw(x, ind, rp);
        break;
      }
      case STRSXP:
      {
         rcpp_set_vind_32_Character(x, ind, rp);
        break;
      }
      default: stop("unsupported type given");
    }
    
  

}







inline void rcpp_set_vind_64_Logical(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
   int *px = LOGICAL(x);
  const int *prp = LOGICAL_RO(rp);
  const double *pind = REAL_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, (R_xlen_t)(pind[i] - 1), prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, (R_xlen_t)(pind[i] - 1), prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_vind_64_Integer(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
   int *px = INTEGER(x);
  const int *prp = INTEGER_RO(rp);
  const double *pind = REAL_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, (R_xlen_t)(pind[i] - 1), prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, (R_xlen_t)(pind[i] - 1), prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_vind_64_Numeric(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
   double *px = REAL(x);
  const double *prp = REAL_RO(rp);
  const double *pind = REAL_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, (R_xlen_t)(pind[i] - 1), prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, (R_xlen_t)(pind[i] - 1), prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_vind_64_Complex(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
   Rcomplex *px = COMPLEX(x);
  const Rcomplex *prp = COMPLEX_RO(rp);
  const double *pind = REAL_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, (R_xlen_t)(pind[i] - 1), prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, (R_xlen_t)(pind[i] - 1), prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_vind_64_Raw(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
   Rbyte *px = RAW(x);
  const Rbyte *prp = RAW_RO(rp);
  const double *pind = REAL_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      MACRO_SET_ATOMIC(px, (R_xlen_t)(pind[i] - 1), prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      MACRO_SET_ATOMIC(px, (R_xlen_t)(pind[i] - 1), prp[0]);
    }
  }
  else stop("recycling not allowed");
  
}





inline void rcpp_set_vind_64_Character(
  SEXP x, const SEXP ind, const SEXP rp
) {
  
  R_xlen_t n = Rf_xlength(ind);
  
  // SEXP *px = STRING_PTR(x);
  const SEXP *prp = STRING_PTR_RO(rp);
  const double *pind = REAL_RO(ind);

  if(Rf_xlength(rp) == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      SET_STRING_ELT(x, (R_xlen_t)(pind[i] - 1), prp[i]);
    }
  }
  else if(Rf_xlength(rp) == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      SET_STRING_ELT(x, (R_xlen_t)(pind[i] - 1), prp[0]);
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
         rcpp_set_vind_64_Logical(x, ind, rp);
        break;
      }
      case INTSXP:
      {
         rcpp_set_vind_64_Integer(x, ind, rp);
        break;
      }
      case REALSXP:
      {
         rcpp_set_vind_64_Numeric(x, ind, rp);
        break;
      }
      case CPLXSXP:
      {
         rcpp_set_vind_64_Complex(x, ind, rp);
        break;
      }
      case RAWSXP:
      {
         rcpp_set_vind_64_Raw(x, ind, rp);
        break;
      }
      case STRSXP:
      {
         rcpp_set_vind_64_Character(x, ind, rp);
        break;
      }
      default: stop("unsupported type given");
    }
    
  

}



