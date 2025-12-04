

#include <Rcpp.h>

#include "squarebrackets.h"

using namespace Rcpp;



inline int rcpp_count_stringmatches(SEXP y, SEXP v) {
  int n = Rf_length(v);
  const SEXP *pv = STRING_PTR_RO(v);
  int count = 0;
  for(int i = 0; i < n; ++i) {
    if((int)R_compute_identical(y, pv[i], 0)) {
      count++;
    }
  }
  return count;
}

#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \
  POINTER[INDEX] = REPLACEMENT; \
} while(0)




//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_countv)]]
R_xlen_t rcpp_countv(
    SEXP y, SEXP v, LogicalVector na, LogicalVector invert
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(y) == 0) {
      return count;
    }
    
    MACRO_SLICEV_DO(count++);
  
    return count;
  }

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_whichv_32)]]
IntegerVector rcpp_whichv_32(
    SEXP y, SEXP v, LogicalVector na, LogicalVector invert
  ) {
    R_xlen_t count = 0;
   
    R_xlen_t amount = rcpp_countv(y, v, na, invert);
    int *pout;
    SEXP out = PROTECT(Rf_allocVector(INTSXP, amount));
    pout = INTEGER(out);
    
    if(amount == 0) {
      UNPROTECT(1);
      return out;
    }
    
    
    MACRO_SLICEV_DO(pout[count] = i + 1; count++);
  
    UNPROTECT(1);
    return out;
  }
  


SEXP rcpp_slicev_x_Logical(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    const int *px = LOGICAL_RO(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert);
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, size));
     int *pout = LOGICAL(out);
    
    if(size == 0) {
      UNPROTECT(1);
      return out;
    }
    
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(pout, count, px[i]); count++);
    
    UNPROTECT(1);
    return out;
  }
  




SEXP rcpp_slicev_x_Integer(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    const int *px = INTEGER_RO(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert);
    SEXP out = PROTECT(Rf_allocVector(INTSXP, size));
     int *pout = INTEGER(out);
    
    if(size == 0) {
      UNPROTECT(1);
      return out;
    }
    
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(pout, count, px[i]); count++);
    
    UNPROTECT(1);
    return out;
  }
  




SEXP rcpp_slicev_x_Numeric(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    const double *px = REAL_RO(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert);
    SEXP out = PROTECT(Rf_allocVector(REALSXP, size));
     double *pout = REAL(out);
    
    if(size == 0) {
      UNPROTECT(1);
      return out;
    }
    
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(pout, count, px[i]); count++);
    
    UNPROTECT(1);
    return out;
  }
  




SEXP rcpp_slicev_x_Complex(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    const Rcomplex *px = COMPLEX_RO(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert);
    SEXP out = PROTECT(Rf_allocVector(CPLXSXP, size));
     Rcomplex *pout = COMPLEX(out);
    
    if(size == 0) {
      UNPROTECT(1);
      return out;
    }
    
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(pout, count, px[i]); count++);
    
    UNPROTECT(1);
    return out;
  }
  




SEXP rcpp_slicev_x_Raw(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    const Rbyte *px = RAW_RO(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert);
    SEXP out = PROTECT(Rf_allocVector(RAWSXP, size));
     Rbyte *pout = RAW(out);
    
    if(size == 0) {
      UNPROTECT(1);
      return out;
    }
    
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(pout, count, px[i]); count++);
    
    UNPROTECT(1);
    return out;
  }
  




SEXP rcpp_slicev_x_Character(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    const SEXP *px = STRING_PTR_RO(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert);
    SEXP out = PROTECT(Rf_allocVector(STRSXP, size));
    // SEXP *pout = STRING_PTR(out);
    
    if(size == 0) {
      UNPROTECT(1);
      return out;
    }
    
    MACRO_SLICEV_DO(SET_STRING_ELT(out, count, px[i]); count++);
    
    UNPROTECT(1);
    return out;
  }
  

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slicev_x_atomic)]]
SEXP rcpp_slicev_x_atomic(
  SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert
) {

    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
        return rcpp_slicev_x_Logical(x, y, v, na, invert);
        break;
      }
      case INTSXP:
      {
        return rcpp_slicev_x_Integer(x, y, v, na, invert);
        break;
      }
      case REALSXP:
      {
        return rcpp_slicev_x_Numeric(x, y, v, na, invert);
        break;
      }
      case CPLXSXP:
      {
        return rcpp_slicev_x_Complex(x, y, v, na, invert);
        break;
      }
      case RAWSXP:
      {
        return rcpp_slicev_x_Raw(x, y, v, na, invert);
        break;
      }
      case STRSXP:
      {
        return rcpp_slicev_x_Character(x, y, v, na, invert);
        break;
      }
      default: stop("unsupported type given");
    }
    
  
  return R_NilValue;
}


void rcpp_slicev_set_Logical(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
  }
  
   int *px = LOGICAL(x);
  const int *prp = LOGICAL_RO(rp);

  if(Rf_xlength(rp) == 1 && Rf_xlength(y) > 0) {
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && Rf_xlength(y) > 0) {
    R_xlen_t count = 0;
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[count]); count++);
  }
  // don't use error for replacement length:
  // that is handled in 'R';
  // placing it here ruins replacement through transformation, which may be of len zero
  
}
  
  



void rcpp_slicev_set_Integer(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
  }
  
   int *px = INTEGER(x);
  const int *prp = INTEGER_RO(rp);

  if(Rf_xlength(rp) == 1 && Rf_xlength(y) > 0) {
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && Rf_xlength(y) > 0) {
    R_xlen_t count = 0;
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[count]); count++);
  }
  // don't use error for replacement length:
  // that is handled in 'R';
  // placing it here ruins replacement through transformation, which may be of len zero
  
}
  
  



void rcpp_slicev_set_Numeric(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
  }
  
   double *px = REAL(x);
  const double *prp = REAL_RO(rp);

  if(Rf_xlength(rp) == 1 && Rf_xlength(y) > 0) {
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && Rf_xlength(y) > 0) {
    R_xlen_t count = 0;
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[count]); count++);
  }
  // don't use error for replacement length:
  // that is handled in 'R';
  // placing it here ruins replacement through transformation, which may be of len zero
  
}
  
  



void rcpp_slicev_set_Complex(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
  }
  
   Rcomplex *px = COMPLEX(x);
  const Rcomplex *prp = COMPLEX_RO(rp);

  if(Rf_xlength(rp) == 1 && Rf_xlength(y) > 0) {
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && Rf_xlength(y) > 0) {
    R_xlen_t count = 0;
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[count]); count++);
  }
  // don't use error for replacement length:
  // that is handled in 'R';
  // placing it here ruins replacement through transformation, which may be of len zero
  
}
  
  



void rcpp_slicev_set_Raw(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
  }
  
   Rbyte *px = RAW(x);
  const Rbyte *prp = RAW_RO(rp);

  if(Rf_xlength(rp) == 1 && Rf_xlength(y) > 0) {
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && Rf_xlength(y) > 0) {
    R_xlen_t count = 0;
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[count]); count++);
  }
  // don't use error for replacement length:
  // that is handled in 'R';
  // placing it here ruins replacement through transformation, which may be of len zero
  
}
  
  



void rcpp_slicev_set_Character(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
  }
  
  // SEXP *px = STRING_PTR(x);
  const SEXP *prp = STRING_PTR_RO(rp);

  if(Rf_xlength(rp) == 1 && Rf_xlength(y) > 0) {
    MACRO_SLICEV_DO(SET_STRING_ELT(x, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && Rf_xlength(y) > 0) {
    R_xlen_t count = 0;
    MACRO_SLICEV_DO(SET_STRING_ELT(x, i, prp[count]); count++);
  }
  // don't use error for replacement length:
  // that is handled in 'R';
  // placing it here ruins replacement through transformation, which may be of len zero
  
}
  
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slicev_set_atomic)]]
void rcpp_slicev_set_atomic(
  SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, SEXP rp
) {

    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
         rcpp_slicev_set_Logical(x, y, v, na, invert, rp);
        break;
      }
      case INTSXP:
      {
         rcpp_slicev_set_Integer(x, y, v, na, invert, rp);
        break;
      }
      case REALSXP:
      {
         rcpp_slicev_set_Numeric(x, y, v, na, invert, rp);
        break;
      }
      case CPLXSXP:
      {
         rcpp_slicev_set_Complex(x, y, v, na, invert, rp);
        break;
      }
      case RAWSXP:
      {
         rcpp_slicev_set_Raw(x, y, v, na, invert, rp);
        break;
      }
      case STRSXP:
      {
         rcpp_slicev_set_Character(x, y, v, na, invert, rp);
        break;
      }
      default: stop("unsupported type given");
    }
    
  
}

