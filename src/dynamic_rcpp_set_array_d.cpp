

#include <Rcpp.h>

#include "squarebrackets.h"

using namespace Rcpp;







inline void rcpp_set_array_d_Logical(
  SEXP x, SEXP sub, SEXP dimcumprod, SEXP rp
) {


R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

 int *px = LOGICAL(x);
const int *prp = LOGICAL_RO(rp);

if(Rf_xlength(rp) == n) {
  R_xlen_t counter = 0;
  MACRO_DIM_DOCALL(MACRO_SET_ATOMIC(px, flatind - 1, prp[counter]); counter++);
}
else if(Rf_xlength(rp) == 1) {
  
  MACRO_DIM_DOCALL(MACRO_SET_ATOMIC(px, flatind - 1, prp[0]));
}
else stop("recycling not allowed");

}





inline void rcpp_set_array_d_Integer(
  SEXP x, SEXP sub, SEXP dimcumprod, SEXP rp
) {


R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

 int *px = INTEGER(x);
const int *prp = INTEGER_RO(rp);

if(Rf_xlength(rp) == n) {
  R_xlen_t counter = 0;
  MACRO_DIM_DOCALL(MACRO_SET_ATOMIC(px, flatind - 1, prp[counter]); counter++);
}
else if(Rf_xlength(rp) == 1) {
  
  MACRO_DIM_DOCALL(MACRO_SET_ATOMIC(px, flatind - 1, prp[0]));
}
else stop("recycling not allowed");

}





inline void rcpp_set_array_d_Numeric(
  SEXP x, SEXP sub, SEXP dimcumprod, SEXP rp
) {


R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

 double *px = REAL(x);
const double *prp = REAL_RO(rp);

if(Rf_xlength(rp) == n) {
  R_xlen_t counter = 0;
  MACRO_DIM_DOCALL(MACRO_SET_ATOMIC(px, flatind - 1, prp[counter]); counter++);
}
else if(Rf_xlength(rp) == 1) {
  
  MACRO_DIM_DOCALL(MACRO_SET_ATOMIC(px, flatind - 1, prp[0]));
}
else stop("recycling not allowed");

}





inline void rcpp_set_array_d_Complex(
  SEXP x, SEXP sub, SEXP dimcumprod, SEXP rp
) {


R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

 Rcomplex *px = COMPLEX(x);
const Rcomplex *prp = COMPLEX_RO(rp);

if(Rf_xlength(rp) == n) {
  R_xlen_t counter = 0;
  MACRO_DIM_DOCALL(MACRO_SET_ATOMIC(px, flatind - 1, prp[counter]); counter++);
}
else if(Rf_xlength(rp) == 1) {
  
  MACRO_DIM_DOCALL(MACRO_SET_ATOMIC(px, flatind - 1, prp[0]));
}
else stop("recycling not allowed");

}





inline void rcpp_set_array_d_Raw(
  SEXP x, SEXP sub, SEXP dimcumprod, SEXP rp
) {


R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

 Rbyte *px = RAW(x);
const Rbyte *prp = RAW_RO(rp);

if(Rf_xlength(rp) == n) {
  R_xlen_t counter = 0;
  MACRO_DIM_DOCALL(MACRO_SET_ATOMIC(px, flatind - 1, prp[counter]); counter++);
}
else if(Rf_xlength(rp) == 1) {
  
  MACRO_DIM_DOCALL(MACRO_SET_ATOMIC(px, flatind - 1, prp[0]));
}
else stop("recycling not allowed");

}





inline void rcpp_set_array_d_Character(
  SEXP x, SEXP sub, SEXP dimcumprod, SEXP rp
) {


R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

// SEXP *px = STRING_PTR(x);
const SEXP *prp = STRING_PTR_RO(rp);

if(Rf_xlength(rp) == n) {
  R_xlen_t counter = 0;
  MACRO_DIM_DOCALL(SET_STRING_ELT(x, flatind - 1, prp[counter]); counter++);
}
else if(Rf_xlength(rp) == 1) {
  
  MACRO_DIM_DOCALL(SET_STRING_ELT(x, flatind - 1, prp[0]));
}
else stop("recycling not allowed");

}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_d_atomic)]]
void rcpp_set_array_d_atomic(
  SEXP x, SEXP sub, SEXP dimcumprod, SEXP rp
) {

    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
         rcpp_set_array_d_Logical(x, sub, dimcumprod, rp);
        break;
      }
      case INTSXP:
      {
         rcpp_set_array_d_Integer(x, sub, dimcumprod, rp);
        break;
      }
      case REALSXP:
      {
         rcpp_set_array_d_Numeric(x, sub, dimcumprod, rp);
        break;
      }
      case CPLXSXP:
      {
         rcpp_set_array_d_Complex(x, sub, dimcumprod, rp);
        break;
      }
      case RAWSXP:
      {
         rcpp_set_array_d_Raw(x, sub, dimcumprod, rp);
        break;
      }
      case STRSXP:
      {
         rcpp_set_array_d_Character(x, sub, dimcumprod, rp);
        break;
      }
      default: stop("unsupported type given");
    }
    
  

}



