

#include <Rcpp.h>

#include "squarebrackets.h"

using namespace Rcpp;






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_stride_get_Rxlent)]]
  R_xlen_t rcpp_stride_get_Rxlent(
    List stride, int arg
  ) {
    RObject extraction = stride[arg];
    if(Rf_xlength(extraction) > 1) {
      stop("attempting to extract non-scalar R_xlen_t");
    }
    if (is<IntegerVector>(extraction)) {
      return as<IntegerVector>(extraction)[0];
    } 
    else if (is<NumericVector>(extraction)) {
      return as<NumericVector>(extraction)[0];
    } 
    else {
      stop("attempting to extract non-numeric R_xlen_t");
    }
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_stride_get_pattern)]]
SEXP rcpp_stride_get_pattern(
  List stride
) {
  RObject extraction = stride[3];
  return extraction;
}







inline SEXP rcpp_slice_seq_x_Logical(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, rcpp_stride_get_Rxlent(stride, 4)));
   int *pout = LOGICAL(out);
  
  const int *px = LOGICAL_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_SEQ(
    MACRO_SET_ATOMIC(pout, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_seq_set_Logical(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
   int *px = LOGICAL(x);
  const int *prp = LOGICAL_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop("recycling not allowed");
  }
  
  MACRO_SLICE_SEQ(
    MACRO_SET_ATOMIC(px, i, prp[index]); index += add
  );
}



inline SEXP rcpp_slice_ptrn_x_Logical(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, rcpp_stride_get_Rxlent(stride, 4)));
   int *pout = LOGICAL(out);
  
  const int *px = LOGICAL_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_PTRN(
    MACRO_SET_ATOMIC(pout, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_ptrn_set_Logical(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
   int *px = LOGICAL(x);
  const int *prp = LOGICAL_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop("recycling not allowed");
  }
  
  MACRO_SLICE_PTRN(
    MACRO_SET_ATOMIC(px, i, prp[index]); index += add
  );
}







inline SEXP rcpp_slice_seq_x_Integer(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(INTSXP, rcpp_stride_get_Rxlent(stride, 4)));
   int *pout = INTEGER(out);
  
  const int *px = INTEGER_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_SEQ(
    MACRO_SET_ATOMIC(pout, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_seq_set_Integer(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
   int *px = INTEGER(x);
  const int *prp = INTEGER_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop("recycling not allowed");
  }
  
  MACRO_SLICE_SEQ(
    MACRO_SET_ATOMIC(px, i, prp[index]); index += add
  );
}



inline SEXP rcpp_slice_ptrn_x_Integer(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(INTSXP, rcpp_stride_get_Rxlent(stride, 4)));
   int *pout = INTEGER(out);
  
  const int *px = INTEGER_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_PTRN(
    MACRO_SET_ATOMIC(pout, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_ptrn_set_Integer(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
   int *px = INTEGER(x);
  const int *prp = INTEGER_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop("recycling not allowed");
  }
  
  MACRO_SLICE_PTRN(
    MACRO_SET_ATOMIC(px, i, prp[index]); index += add
  );
}







inline SEXP rcpp_slice_seq_x_Numeric(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(REALSXP, rcpp_stride_get_Rxlent(stride, 4)));
   double *pout = REAL(out);
  
  const double *px = REAL_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_SEQ(
    MACRO_SET_ATOMIC(pout, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_seq_set_Numeric(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
   double *px = REAL(x);
  const double *prp = REAL_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop("recycling not allowed");
  }
  
  MACRO_SLICE_SEQ(
    MACRO_SET_ATOMIC(px, i, prp[index]); index += add
  );
}



inline SEXP rcpp_slice_ptrn_x_Numeric(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(REALSXP, rcpp_stride_get_Rxlent(stride, 4)));
   double *pout = REAL(out);
  
  const double *px = REAL_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_PTRN(
    MACRO_SET_ATOMIC(pout, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_ptrn_set_Numeric(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
   double *px = REAL(x);
  const double *prp = REAL_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop("recycling not allowed");
  }
  
  MACRO_SLICE_PTRN(
    MACRO_SET_ATOMIC(px, i, prp[index]); index += add
  );
}







inline SEXP rcpp_slice_seq_x_Complex(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(CPLXSXP, rcpp_stride_get_Rxlent(stride, 4)));
   Rcomplex *pout = COMPLEX(out);
  
  const Rcomplex *px = COMPLEX_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_SEQ(
    MACRO_SET_ATOMIC(pout, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_seq_set_Complex(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
   Rcomplex *px = COMPLEX(x);
  const Rcomplex *prp = COMPLEX_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop("recycling not allowed");
  }
  
  MACRO_SLICE_SEQ(
    MACRO_SET_ATOMIC(px, i, prp[index]); index += add
  );
}



inline SEXP rcpp_slice_ptrn_x_Complex(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(CPLXSXP, rcpp_stride_get_Rxlent(stride, 4)));
   Rcomplex *pout = COMPLEX(out);
  
  const Rcomplex *px = COMPLEX_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_PTRN(
    MACRO_SET_ATOMIC(pout, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_ptrn_set_Complex(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
   Rcomplex *px = COMPLEX(x);
  const Rcomplex *prp = COMPLEX_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop("recycling not allowed");
  }
  
  MACRO_SLICE_PTRN(
    MACRO_SET_ATOMIC(px, i, prp[index]); index += add
  );
}







inline SEXP rcpp_slice_seq_x_Raw(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, rcpp_stride_get_Rxlent(stride, 4)));
   Rbyte *pout = RAW(out);
  
  const Rbyte *px = RAW_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_SEQ(
    MACRO_SET_ATOMIC(pout, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_seq_set_Raw(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
   Rbyte *px = RAW(x);
  const Rbyte *prp = RAW_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop("recycling not allowed");
  }
  
  MACRO_SLICE_SEQ(
    MACRO_SET_ATOMIC(px, i, prp[index]); index += add
  );
}



inline SEXP rcpp_slice_ptrn_x_Raw(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, rcpp_stride_get_Rxlent(stride, 4)));
   Rbyte *pout = RAW(out);
  
  const Rbyte *px = RAW_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_PTRN(
    MACRO_SET_ATOMIC(pout, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_ptrn_set_Raw(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
   Rbyte *px = RAW(x);
  const Rbyte *prp = RAW_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop("recycling not allowed");
  }
  
  MACRO_SLICE_PTRN(
    MACRO_SET_ATOMIC(px, i, prp[index]); index += add
  );
}







inline SEXP rcpp_slice_seq_x_Character(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(STRSXP, rcpp_stride_get_Rxlent(stride, 4)));
  // SEXP *pout = STRING_PTR(out);
  
  const SEXP *px = STRING_PTR_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_SEQ(
    SET_STRING_ELT(out, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_seq_set_Character(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
  // SEXP *px = STRING_PTR(x);
  const SEXP *prp = STRING_PTR_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop("recycling not allowed");
  }
  
  MACRO_SLICE_SEQ(
    SET_STRING_ELT(x, i, prp[index]); index += add
  );
}



inline SEXP rcpp_slice_ptrn_x_Character(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(STRSXP, rcpp_stride_get_Rxlent(stride, 4)));
  // SEXP *pout = STRING_PTR(out);
  
  const SEXP *px = STRING_PTR_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_PTRN(
    SET_STRING_ELT(out, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_ptrn_set_Character(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
  // SEXP *px = STRING_PTR(x);
  const SEXP *prp = STRING_PTR_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop("recycling not allowed");
  }
  
  MACRO_SLICE_PTRN(
    SET_STRING_ELT(x, i, prp[index]); index += add
  );
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_seq_x_atomic)]]
SEXP rcpp_slice_seq_x_atomic(
  const SEXP x, const SEXP stride, int use
) {


    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
        return rcpp_slice_seq_x_Logical(x, stride, use);
        break;
      }
      case INTSXP:
      {
        return rcpp_slice_seq_x_Integer(x, stride, use);
        break;
      }
      case REALSXP:
      {
        return rcpp_slice_seq_x_Numeric(x, stride, use);
        break;
      }
      case CPLXSXP:
      {
        return rcpp_slice_seq_x_Complex(x, stride, use);
        break;
      }
      case RAWSXP:
      {
        return rcpp_slice_seq_x_Raw(x, stride, use);
        break;
      }
      case STRSXP:
      {
        return rcpp_slice_seq_x_Character(x, stride, use);
        break;
      }
      default: stop("unsupported type given");
    }
    
  

return R_NilValue;

}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_seq_set_atomic)]]
void rcpp_slice_seq_set_atomic(
  SEXP x, const SEXP rp, const SEXP stride, int use
) {


    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
         rcpp_slice_seq_set_Logical(x, rp, stride, use);
        break;
      }
      case INTSXP:
      {
         rcpp_slice_seq_set_Integer(x, rp, stride, use);
        break;
      }
      case REALSXP:
      {
         rcpp_slice_seq_set_Numeric(x, rp, stride, use);
        break;
      }
      case CPLXSXP:
      {
         rcpp_slice_seq_set_Complex(x, rp, stride, use);
        break;
      }
      case RAWSXP:
      {
         rcpp_slice_seq_set_Raw(x, rp, stride, use);
        break;
      }
      case STRSXP:
      {
         rcpp_slice_seq_set_Character(x, rp, stride, use);
        break;
      }
      default: stop("unsupported type given");
    }
    
  



}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_ptrn_x_atomic)]]
SEXP rcpp_slice_ptrn_x_atomic(
  const SEXP x, const SEXP stride, int use
) {


    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
        return rcpp_slice_ptrn_x_Logical(x, stride, use);
        break;
      }
      case INTSXP:
      {
        return rcpp_slice_ptrn_x_Integer(x, stride, use);
        break;
      }
      case REALSXP:
      {
        return rcpp_slice_ptrn_x_Numeric(x, stride, use);
        break;
      }
      case CPLXSXP:
      {
        return rcpp_slice_ptrn_x_Complex(x, stride, use);
        break;
      }
      case RAWSXP:
      {
        return rcpp_slice_ptrn_x_Raw(x, stride, use);
        break;
      }
      case STRSXP:
      {
        return rcpp_slice_ptrn_x_Character(x, stride, use);
        break;
      }
      default: stop("unsupported type given");
    }
    
  

return R_NilValue;

}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_ptrn_set_atomic)]]
void rcpp_slice_ptrn_set_atomic(
  SEXP x, const SEXP rp, const SEXP stride, int use
) {


    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
         rcpp_slice_ptrn_set_Logical(x, rp, stride, use);
        break;
      }
      case INTSXP:
      {
         rcpp_slice_ptrn_set_Integer(x, rp, stride, use);
        break;
      }
      case REALSXP:
      {
         rcpp_slice_ptrn_set_Numeric(x, rp, stride, use);
        break;
      }
      case CPLXSXP:
      {
         rcpp_slice_ptrn_set_Complex(x, rp, stride, use);
        break;
      }
      case RAWSXP:
      {
         rcpp_slice_ptrn_set_Raw(x, rp, stride, use);
        break;
      }
      case STRSXP:
      {
         rcpp_slice_ptrn_set_Character(x, rp, stride, use);
        break;
      }
      default: stop("unsupported type given");
    }
    
  



}

