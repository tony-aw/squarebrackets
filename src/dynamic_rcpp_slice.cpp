

#include <Rcpp.h>

#include "squarebrackets.h"

using namespace Rcpp;







inline SEXP rcpp_slice_x_Logical(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, len));
   int *pout = LOGICAL(out);
  const int *px = LOGICAL_RO(x);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(pout, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(pout, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}
  


inline void rcpp_slice_set_Logical(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   int *px = LOGICAL(x);
  const int *prp = LOGICAL_RO(rp);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(px, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(px, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else {
    stop("recycling not allowed");
  }
}


inline SEXP rcpp_slice_xrev_Logical(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, len));
   int *pout = LOGICAL(out);
  const int *px = LOGICAL_RO(x);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(pout, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(pout, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}


inline void rcpp_slice_setrev_Logical(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   int *px = LOGICAL(x);
  const int *prp = LOGICAL_RO(rp);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(px, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(px, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else {
    stop("recylcing not allowed");
  }
}


inline SEXP rcpp_slice_wo_Logical(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, len));
   int *pout = LOGICAL(out);
  const int *px = LOGICAL_RO(x);
  
  R_xlen_t counter = 0;
  if(start > 0) {
    // Comment
    for(R_xlen_t i = 0; i < start; ++i) {
      MACRO_SET_ATOMIC(pout, i, px[i]);
    }
    counter = start;
  }
  
  if(by == 2) {
    for(R_xlen_t i = start; i < end; i+=2) {
      MACRO_SET_ATOMIC(pout, counter, px[i+1]);
      counter++;
    }
  }
  
  if(by > 2) {
    for(R_xlen_t i = start; i < end; i += by) {
      R_xlen_t startx = i + 1;
      for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
        MACRO_SET_ATOMIC(pout, counter, px[j]);
        counter++;
      }
    }
  }
  
  if(end < (Rf_xlength(x) - 1)) {
    // Comment
    for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
      MACRO_SET_ATOMIC(pout, i - (end + 1) + counter, px[i]);
    }
  }
  
  UNPROTECT(1);
  return out;
}



inline void rcpp_slice_setinv_Logical(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   int *px = LOGICAL(x);
  const int *prp = LOGICAL_RO(rp);
  
  if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[i]);
      }
      counter = start;
    }
    
    if(by == 2) {
      for(R_xlen_t i = start; i < end; i+=2) {
        MACRO_SET_ATOMIC(px, i+1, prp[counter]);
        counter++;
      }
    }
    
    if(by > 2) {
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          MACRO_SET_ATOMIC(px, j, prp[counter]);
          counter++;
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        MACRO_SET_ATOMIC(px, i, prp[i - (end + 1) + counter]);
      }
    }
  }
  else if(Rf_xlength(rp) == 1) {
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[0]);
      }
    }
    
    if(by == 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i+=2) {
        MACRO_SET_ATOMIC(px, i+1, prp[0]);
      }
    }
    
    if(by > 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          MACRO_SET_ATOMIC(px, j, prp[0]);
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        MACRO_SET_ATOMIC(px, i, prp[0]);
      }
    }
  }
  else {
    stop("recycling not allowed");
  }
}






inline SEXP rcpp_slice_x_Integer(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(INTSXP, len));
   int *pout = INTEGER(out);
  const int *px = INTEGER_RO(x);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(pout, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(pout, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}
  


inline void rcpp_slice_set_Integer(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   int *px = INTEGER(x);
  const int *prp = INTEGER_RO(rp);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(px, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(px, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else {
    stop("recycling not allowed");
  }
}


inline SEXP rcpp_slice_xrev_Integer(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(INTSXP, len));
   int *pout = INTEGER(out);
  const int *px = INTEGER_RO(x);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(pout, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(pout, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}


inline void rcpp_slice_setrev_Integer(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   int *px = INTEGER(x);
  const int *prp = INTEGER_RO(rp);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(px, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(px, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else {
    stop("recylcing not allowed");
  }
}


inline SEXP rcpp_slice_wo_Integer(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  SEXP out = PROTECT(Rf_allocVector(INTSXP, len));
   int *pout = INTEGER(out);
  const int *px = INTEGER_RO(x);
  
  R_xlen_t counter = 0;
  if(start > 0) {
    // Comment
    for(R_xlen_t i = 0; i < start; ++i) {
      MACRO_SET_ATOMIC(pout, i, px[i]);
    }
    counter = start;
  }
  
  if(by == 2) {
    for(R_xlen_t i = start; i < end; i+=2) {
      MACRO_SET_ATOMIC(pout, counter, px[i+1]);
      counter++;
    }
  }
  
  if(by > 2) {
    for(R_xlen_t i = start; i < end; i += by) {
      R_xlen_t startx = i + 1;
      for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
        MACRO_SET_ATOMIC(pout, counter, px[j]);
        counter++;
      }
    }
  }
  
  if(end < (Rf_xlength(x) - 1)) {
    // Comment
    for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
      MACRO_SET_ATOMIC(pout, i - (end + 1) + counter, px[i]);
    }
  }
  
  UNPROTECT(1);
  return out;
}



inline void rcpp_slice_setinv_Integer(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   int *px = INTEGER(x);
  const int *prp = INTEGER_RO(rp);
  
  if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[i]);
      }
      counter = start;
    }
    
    if(by == 2) {
      for(R_xlen_t i = start; i < end; i+=2) {
        MACRO_SET_ATOMIC(px, i+1, prp[counter]);
        counter++;
      }
    }
    
    if(by > 2) {
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          MACRO_SET_ATOMIC(px, j, prp[counter]);
          counter++;
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        MACRO_SET_ATOMIC(px, i, prp[i - (end + 1) + counter]);
      }
    }
  }
  else if(Rf_xlength(rp) == 1) {
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[0]);
      }
    }
    
    if(by == 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i+=2) {
        MACRO_SET_ATOMIC(px, i+1, prp[0]);
      }
    }
    
    if(by > 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          MACRO_SET_ATOMIC(px, j, prp[0]);
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        MACRO_SET_ATOMIC(px, i, prp[0]);
      }
    }
  }
  else {
    stop("recycling not allowed");
  }
}






inline SEXP rcpp_slice_x_Numeric(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(REALSXP, len));
   double *pout = REAL(out);
  const double *px = REAL_RO(x);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(pout, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(pout, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}
  


inline void rcpp_slice_set_Numeric(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   double *px = REAL(x);
  const double *prp = REAL_RO(rp);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(px, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(px, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else {
    stop("recycling not allowed");
  }
}


inline SEXP rcpp_slice_xrev_Numeric(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(REALSXP, len));
   double *pout = REAL(out);
  const double *px = REAL_RO(x);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(pout, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(pout, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}


inline void rcpp_slice_setrev_Numeric(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   double *px = REAL(x);
  const double *prp = REAL_RO(rp);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(px, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(px, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else {
    stop("recylcing not allowed");
  }
}


inline SEXP rcpp_slice_wo_Numeric(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  SEXP out = PROTECT(Rf_allocVector(REALSXP, len));
   double *pout = REAL(out);
  const double *px = REAL_RO(x);
  
  R_xlen_t counter = 0;
  if(start > 0) {
    // Comment
    for(R_xlen_t i = 0; i < start; ++i) {
      MACRO_SET_ATOMIC(pout, i, px[i]);
    }
    counter = start;
  }
  
  if(by == 2) {
    for(R_xlen_t i = start; i < end; i+=2) {
      MACRO_SET_ATOMIC(pout, counter, px[i+1]);
      counter++;
    }
  }
  
  if(by > 2) {
    for(R_xlen_t i = start; i < end; i += by) {
      R_xlen_t startx = i + 1;
      for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
        MACRO_SET_ATOMIC(pout, counter, px[j]);
        counter++;
      }
    }
  }
  
  if(end < (Rf_xlength(x) - 1)) {
    // Comment
    for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
      MACRO_SET_ATOMIC(pout, i - (end + 1) + counter, px[i]);
    }
  }
  
  UNPROTECT(1);
  return out;
}



inline void rcpp_slice_setinv_Numeric(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   double *px = REAL(x);
  const double *prp = REAL_RO(rp);
  
  if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[i]);
      }
      counter = start;
    }
    
    if(by == 2) {
      for(R_xlen_t i = start; i < end; i+=2) {
        MACRO_SET_ATOMIC(px, i+1, prp[counter]);
        counter++;
      }
    }
    
    if(by > 2) {
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          MACRO_SET_ATOMIC(px, j, prp[counter]);
          counter++;
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        MACRO_SET_ATOMIC(px, i, prp[i - (end + 1) + counter]);
      }
    }
  }
  else if(Rf_xlength(rp) == 1) {
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[0]);
      }
    }
    
    if(by == 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i+=2) {
        MACRO_SET_ATOMIC(px, i+1, prp[0]);
      }
    }
    
    if(by > 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          MACRO_SET_ATOMIC(px, j, prp[0]);
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        MACRO_SET_ATOMIC(px, i, prp[0]);
      }
    }
  }
  else {
    stop("recycling not allowed");
  }
}






inline SEXP rcpp_slice_x_Complex(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(CPLXSXP, len));
   Rcomplex *pout = COMPLEX(out);
  const Rcomplex *px = COMPLEX_RO(x);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(pout, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(pout, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}
  


inline void rcpp_slice_set_Complex(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   Rcomplex *px = COMPLEX(x);
  const Rcomplex *prp = COMPLEX_RO(rp);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(px, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(px, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else {
    stop("recycling not allowed");
  }
}


inline SEXP rcpp_slice_xrev_Complex(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(CPLXSXP, len));
   Rcomplex *pout = COMPLEX(out);
  const Rcomplex *px = COMPLEX_RO(x);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(pout, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(pout, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}


inline void rcpp_slice_setrev_Complex(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   Rcomplex *px = COMPLEX(x);
  const Rcomplex *prp = COMPLEX_RO(rp);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(px, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(px, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else {
    stop("recylcing not allowed");
  }
}


inline SEXP rcpp_slice_wo_Complex(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  SEXP out = PROTECT(Rf_allocVector(CPLXSXP, len));
   Rcomplex *pout = COMPLEX(out);
  const Rcomplex *px = COMPLEX_RO(x);
  
  R_xlen_t counter = 0;
  if(start > 0) {
    // Comment
    for(R_xlen_t i = 0; i < start; ++i) {
      MACRO_SET_ATOMIC(pout, i, px[i]);
    }
    counter = start;
  }
  
  if(by == 2) {
    for(R_xlen_t i = start; i < end; i+=2) {
      MACRO_SET_ATOMIC(pout, counter, px[i+1]);
      counter++;
    }
  }
  
  if(by > 2) {
    for(R_xlen_t i = start; i < end; i += by) {
      R_xlen_t startx = i + 1;
      for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
        MACRO_SET_ATOMIC(pout, counter, px[j]);
        counter++;
      }
    }
  }
  
  if(end < (Rf_xlength(x) - 1)) {
    // Comment
    for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
      MACRO_SET_ATOMIC(pout, i - (end + 1) + counter, px[i]);
    }
  }
  
  UNPROTECT(1);
  return out;
}



inline void rcpp_slice_setinv_Complex(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   Rcomplex *px = COMPLEX(x);
  const Rcomplex *prp = COMPLEX_RO(rp);
  
  if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[i]);
      }
      counter = start;
    }
    
    if(by == 2) {
      for(R_xlen_t i = start; i < end; i+=2) {
        MACRO_SET_ATOMIC(px, i+1, prp[counter]);
        counter++;
      }
    }
    
    if(by > 2) {
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          MACRO_SET_ATOMIC(px, j, prp[counter]);
          counter++;
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        MACRO_SET_ATOMIC(px, i, prp[i - (end + 1) + counter]);
      }
    }
  }
  else if(Rf_xlength(rp) == 1) {
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[0]);
      }
    }
    
    if(by == 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i+=2) {
        MACRO_SET_ATOMIC(px, i+1, prp[0]);
      }
    }
    
    if(by > 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          MACRO_SET_ATOMIC(px, j, prp[0]);
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        MACRO_SET_ATOMIC(px, i, prp[0]);
      }
    }
  }
  else {
    stop("recycling not allowed");
  }
}






inline SEXP rcpp_slice_x_Raw(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, len));
   Rbyte *pout = RAW(out);
  const Rbyte *px = RAW_RO(x);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(pout, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(pout, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}
  


inline void rcpp_slice_set_Raw(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   Rbyte *px = RAW(x);
  const Rbyte *prp = RAW_RO(rp);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(px, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(px, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i <= end; i += by) {
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else {
    stop("recycling not allowed");
  }
}


inline SEXP rcpp_slice_xrev_Raw(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, len));
   Rbyte *pout = RAW(out);
  const Rbyte *px = RAW_RO(x);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(pout, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(pout, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}


inline void rcpp_slice_setrev_Raw(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   Rbyte *px = RAW(x);
  const Rbyte *prp = RAW_RO(rp);
  
  if(len == 1) {
    MACRO_SET_ATOMIC(px, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(px, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i >= end; i -= by) {
      MACRO_SET_ATOMIC(px, i, prp[0]);
    }
  }
  else {
    stop("recylcing not allowed");
  }
}


inline SEXP rcpp_slice_wo_Raw(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, len));
   Rbyte *pout = RAW(out);
  const Rbyte *px = RAW_RO(x);
  
  R_xlen_t counter = 0;
  if(start > 0) {
    // Comment
    for(R_xlen_t i = 0; i < start; ++i) {
      MACRO_SET_ATOMIC(pout, i, px[i]);
    }
    counter = start;
  }
  
  if(by == 2) {
    for(R_xlen_t i = start; i < end; i+=2) {
      MACRO_SET_ATOMIC(pout, counter, px[i+1]);
      counter++;
    }
  }
  
  if(by > 2) {
    for(R_xlen_t i = start; i < end; i += by) {
      R_xlen_t startx = i + 1;
      for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
        MACRO_SET_ATOMIC(pout, counter, px[j]);
        counter++;
      }
    }
  }
  
  if(end < (Rf_xlength(x) - 1)) {
    // Comment
    for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
      MACRO_SET_ATOMIC(pout, i - (end + 1) + counter, px[i]);
    }
  }
  
  UNPROTECT(1);
  return out;
}



inline void rcpp_slice_setinv_Raw(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
   Rbyte *px = RAW(x);
  const Rbyte *prp = RAW_RO(rp);
  
  if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[i]);
      }
      counter = start;
    }
    
    if(by == 2) {
      for(R_xlen_t i = start; i < end; i+=2) {
        MACRO_SET_ATOMIC(px, i+1, prp[counter]);
        counter++;
      }
    }
    
    if(by > 2) {
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          MACRO_SET_ATOMIC(px, j, prp[counter]);
          counter++;
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        MACRO_SET_ATOMIC(px, i, prp[i - (end + 1) + counter]);
      }
    }
  }
  else if(Rf_xlength(rp) == 1) {
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[0]);
      }
    }
    
    if(by == 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i+=2) {
        MACRO_SET_ATOMIC(px, i+1, prp[0]);
      }
    }
    
    if(by > 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          MACRO_SET_ATOMIC(px, j, prp[0]);
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        MACRO_SET_ATOMIC(px, i, prp[0]);
      }
    }
  }
  else {
    stop("recycling not allowed");
  }
}






inline SEXP rcpp_slice_x_Character(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(STRSXP, len));
  // SEXP *pout = STRING_PTR(out);
  const SEXP *px = STRING_PTR_RO(x);
  
  if(len == 1) {
    SET_STRING_ELT(out, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      SET_STRING_ELT(out, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}
  


inline void rcpp_slice_set_Character(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  // SEXP *px = STRING_PTR(x);
  const SEXP *prp = STRING_PTR_RO(rp);
  
  if(len == 1) {
    SET_STRING_ELT(x, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      SET_STRING_ELT(x, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i <= end; i += by) {
      SET_STRING_ELT(x, i, prp[0]);
    }
  }
  else {
    stop("recycling not allowed");
  }
}


inline SEXP rcpp_slice_xrev_Character(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(STRSXP, len));
  // SEXP *pout = STRING_PTR(out);
  const SEXP *px = STRING_PTR_RO(x);
  
  if(len == 1) {
    SET_STRING_ELT(out, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      SET_STRING_ELT(out, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}


inline void rcpp_slice_setrev_Character(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  // SEXP *px = STRING_PTR(x);
  const SEXP *prp = STRING_PTR_RO(rp);
  
  if(len == 1) {
    SET_STRING_ELT(x, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      SET_STRING_ELT(x, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i >= end; i -= by) {
      SET_STRING_ELT(x, i, prp[0]);
    }
  }
  else {
    stop("recylcing not allowed");
  }
}


inline SEXP rcpp_slice_wo_Character(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  SEXP out = PROTECT(Rf_allocVector(STRSXP, len));
  // SEXP *pout = STRING_PTR(out);
  const SEXP *px = STRING_PTR_RO(x);
  
  R_xlen_t counter = 0;
  if(start > 0) {
    // Comment
    for(R_xlen_t i = 0; i < start; ++i) {
      SET_STRING_ELT(out, i, px[i]);
    }
    counter = start;
  }
  
  if(by == 2) {
    for(R_xlen_t i = start; i < end; i+=2) {
      SET_STRING_ELT(out, counter, px[i+1]);
      counter++;
    }
  }
  
  if(by > 2) {
    for(R_xlen_t i = start; i < end; i += by) {
      R_xlen_t startx = i + 1;
      for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
        SET_STRING_ELT(out, counter, px[j]);
        counter++;
      }
    }
  }
  
  if(end < (Rf_xlength(x) - 1)) {
    // Comment
    for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
      SET_STRING_ELT(out, i - (end + 1) + counter, px[i]);
    }
  }
  
  UNPROTECT(1);
  return out;
}



inline void rcpp_slice_setinv_Character(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  // SEXP *px = STRING_PTR(x);
  const SEXP *prp = STRING_PTR_RO(rp);
  
  if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        SET_STRING_ELT(x, i, prp[i]);
      }
      counter = start;
    }
    
    if(by == 2) {
      for(R_xlen_t i = start; i < end; i+=2) {
        SET_STRING_ELT(x, i+1, prp[counter]);
        counter++;
      }
    }
    
    if(by > 2) {
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          SET_STRING_ELT(x, j, prp[counter]);
          counter++;
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        SET_STRING_ELT(x, i, prp[i - (end + 1) + counter]);
      }
    }
  }
  else if(Rf_xlength(rp) == 1) {
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        SET_STRING_ELT(x, i, prp[0]);
      }
    }
    
    if(by == 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i+=2) {
        SET_STRING_ELT(x, i+1, prp[0]);
      }
    }
    
    if(by > 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          SET_STRING_ELT(x, j, prp[0]);
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        SET_STRING_ELT(x, i, prp[0]);
      }
    }
  }
  else {
    stop("recycling not allowed");
  }
}





//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_x_atomic)]]
SEXP rcpp_slice_x_atomic(
  const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
) {


    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
        return rcpp_slice_x_Logical(x, start, end, by, len);
        break;
      }
      case INTSXP:
      {
        return rcpp_slice_x_Integer(x, start, end, by, len);
        break;
      }
      case REALSXP:
      {
        return rcpp_slice_x_Numeric(x, start, end, by, len);
        break;
      }
      case CPLXSXP:
      {
        return rcpp_slice_x_Complex(x, start, end, by, len);
        break;
      }
      case RAWSXP:
      {
        return rcpp_slice_x_Raw(x, start, end, by, len);
        break;
      }
      case STRSXP:
      {
        return rcpp_slice_x_Character(x, start, end, by, len);
        break;
      }
      default: stop("unsupported type given");
    }
    
  

return R_NilValue;

}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_set_atomic)]]
void rcpp_slice_set_atomic(
  SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
) {


    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
         rcpp_slice_set_Logical(x, rp, start, end, by, len);
        break;
      }
      case INTSXP:
      {
         rcpp_slice_set_Integer(x, rp, start, end, by, len);
        break;
      }
      case REALSXP:
      {
         rcpp_slice_set_Numeric(x, rp, start, end, by, len);
        break;
      }
      case CPLXSXP:
      {
         rcpp_slice_set_Complex(x, rp, start, end, by, len);
        break;
      }
      case RAWSXP:
      {
         rcpp_slice_set_Raw(x, rp, start, end, by, len);
        break;
      }
      case STRSXP:
      {
         rcpp_slice_set_Character(x, rp, start, end, by, len);
        break;
      }
      default: stop("unsupported type given");
    }
    
  



}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_xrev_atomic)]]
SEXP rcpp_slice_xrev_atomic(
  const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
) {


    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
        return rcpp_slice_xrev_Logical(x, start, end, by, len);
        break;
      }
      case INTSXP:
      {
        return rcpp_slice_xrev_Integer(x, start, end, by, len);
        break;
      }
      case REALSXP:
      {
        return rcpp_slice_xrev_Numeric(x, start, end, by, len);
        break;
      }
      case CPLXSXP:
      {
        return rcpp_slice_xrev_Complex(x, start, end, by, len);
        break;
      }
      case RAWSXP:
      {
        return rcpp_slice_xrev_Raw(x, start, end, by, len);
        break;
      }
      case STRSXP:
      {
        return rcpp_slice_xrev_Character(x, start, end, by, len);
        break;
      }
      default: stop("unsupported type given");
    }
    
  

return R_NilValue;

}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_setrev_atomic)]]
void rcpp_slice_setrev_atomic(
  SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
) {


    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
         rcpp_slice_setrev_Logical(x, rp, start, end, by, len);
        break;
      }
      case INTSXP:
      {
         rcpp_slice_setrev_Integer(x, rp, start, end, by, len);
        break;
      }
      case REALSXP:
      {
         rcpp_slice_setrev_Numeric(x, rp, start, end, by, len);
        break;
      }
      case CPLXSXP:
      {
         rcpp_slice_setrev_Complex(x, rp, start, end, by, len);
        break;
      }
      case RAWSXP:
      {
         rcpp_slice_setrev_Raw(x, rp, start, end, by, len);
        break;
      }
      case STRSXP:
      {
         rcpp_slice_setrev_Character(x, rp, start, end, by, len);
        break;
      }
      default: stop("unsupported type given");
    }
    
  



}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_wo_atomic)]]
SEXP rcpp_slice_wo_atomic(
  const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
) {


    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
        return rcpp_slice_wo_Logical(x, start, end, by, len);
        break;
      }
      case INTSXP:
      {
        return rcpp_slice_wo_Integer(x, start, end, by, len);
        break;
      }
      case REALSXP:
      {
        return rcpp_slice_wo_Numeric(x, start, end, by, len);
        break;
      }
      case CPLXSXP:
      {
        return rcpp_slice_wo_Complex(x, start, end, by, len);
        break;
      }
      case RAWSXP:
      {
        return rcpp_slice_wo_Raw(x, start, end, by, len);
        break;
      }
      case STRSXP:
      {
        return rcpp_slice_wo_Character(x, start, end, by, len);
        break;
      }
      default: stop("unsupported type given");
    }
    
  

return R_NilValue;

}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_setinv_atomic)]]
void rcpp_slice_setinv_atomic(
  SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
) {


    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
         rcpp_slice_setinv_Logical(x, rp, start, end, by, len);
        break;
      }
      case INTSXP:
      {
         rcpp_slice_setinv_Integer(x, rp, start, end, by, len);
        break;
      }
      case REALSXP:
      {
         rcpp_slice_setinv_Numeric(x, rp, start, end, by, len);
        break;
      }
      case CPLXSXP:
      {
         rcpp_slice_setinv_Complex(x, rp, start, end, by, len);
        break;
      }
      case RAWSXP:
      {
         rcpp_slice_setinv_Raw(x, rp, start, end, by, len);
        break;
      }
      case STRSXP:
      {
         rcpp_slice_setinv_Character(x, rp, start, end, by, len);
        break;
      }
      default: stop("unsupported type given");
    }
    
  



}

