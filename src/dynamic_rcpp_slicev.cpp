

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






SEXP rcpp_slicev_x_Logical(
  SEXP x, List preplist, NumericVector prepvector, SEXP pool
) {
  
  const int *px = LOGICAL_RO(x);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  
  if(count_total == 0) {
    stop("no matches");
  }
  
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, count_total));
   int *pout = LOGICAL(out);

  
  NumericVector first = preplist[0];
  NumericVector last = preplist[1];
  NumericVector count = preplist[2];
  NumericVector rnglen = preplist[3];
  const int n_chunks = Rf_length(first);
  
  R_xlen_t outcount = 0;
  
  for(int j = 0; j < n_chunks; ++j) {
    SEXP b32 = VECTOR_ELT(pool, j);
    
    const R_xlen_t current_count = count[j];
    const R_xlen_t current_rnglen = rnglen[j];
    
    if(current_count == 0) {
      continue;
    }
    else if(current_count == 1) {
      const R_xlen_t first0 = first[j];
      MACRO_SET_ATOMIC(pout, outcount, px[first0]);
      outcount++;
    }
    else if(current_count == 2) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      MACRO_SET_ATOMIC(pout, outcount, px[first0]);
      outcount++;
      
      MACRO_SET_ATOMIC(pout, outcount, px[last0]);
      outcount++;
    }
    else if(current_count == current_rnglen) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      for(R_xlen_t i = first0; i <= last0; ++i) {
        MACRO_SET_ATOMIC(pout, outcount, px[i]);
        outcount++;
      }
    }
    else {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      R_xlen_t i = first0;
      MACRO_STRIDEV_BITS_TRANSFER(
        MACRO_SET_ATOMIC(pout, outcount++, px[i]),
        first0,
        last0
      );
    }
  }
    
  UNPROTECT(1);
  return out;
    
}




SEXP rcpp_slicev_x_Integer(
  SEXP x, List preplist, NumericVector prepvector, SEXP pool
) {
  
  const int *px = INTEGER_RO(x);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  
  if(count_total == 0) {
    stop("no matches");
  }
  
  SEXP out = PROTECT(Rf_allocVector(INTSXP, count_total));
   int *pout = INTEGER(out);

  
  NumericVector first = preplist[0];
  NumericVector last = preplist[1];
  NumericVector count = preplist[2];
  NumericVector rnglen = preplist[3];
  const int n_chunks = Rf_length(first);
  
  R_xlen_t outcount = 0;
  
  for(int j = 0; j < n_chunks; ++j) {
    SEXP b32 = VECTOR_ELT(pool, j);
    
    const R_xlen_t current_count = count[j];
    const R_xlen_t current_rnglen = rnglen[j];
    
    if(current_count == 0) {
      continue;
    }
    else if(current_count == 1) {
      const R_xlen_t first0 = first[j];
      MACRO_SET_ATOMIC(pout, outcount, px[first0]);
      outcount++;
    }
    else if(current_count == 2) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      MACRO_SET_ATOMIC(pout, outcount, px[first0]);
      outcount++;
      
      MACRO_SET_ATOMIC(pout, outcount, px[last0]);
      outcount++;
    }
    else if(current_count == current_rnglen) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      for(R_xlen_t i = first0; i <= last0; ++i) {
        MACRO_SET_ATOMIC(pout, outcount, px[i]);
        outcount++;
      }
    }
    else {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      R_xlen_t i = first0;
      MACRO_STRIDEV_BITS_TRANSFER(
        MACRO_SET_ATOMIC(pout, outcount++, px[i]),
        first0,
        last0
      );
    }
  }
    
  UNPROTECT(1);
  return out;
    
}




SEXP rcpp_slicev_x_Numeric(
  SEXP x, List preplist, NumericVector prepvector, SEXP pool
) {
  
  const double *px = REAL_RO(x);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  
  if(count_total == 0) {
    stop("no matches");
  }
  
  SEXP out = PROTECT(Rf_allocVector(REALSXP, count_total));
   double *pout = REAL(out);

  
  NumericVector first = preplist[0];
  NumericVector last = preplist[1];
  NumericVector count = preplist[2];
  NumericVector rnglen = preplist[3];
  const int n_chunks = Rf_length(first);
  
  R_xlen_t outcount = 0;
  
  for(int j = 0; j < n_chunks; ++j) {
    SEXP b32 = VECTOR_ELT(pool, j);
    
    const R_xlen_t current_count = count[j];
    const R_xlen_t current_rnglen = rnglen[j];
    
    if(current_count == 0) {
      continue;
    }
    else if(current_count == 1) {
      const R_xlen_t first0 = first[j];
      MACRO_SET_ATOMIC(pout, outcount, px[first0]);
      outcount++;
    }
    else if(current_count == 2) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      MACRO_SET_ATOMIC(pout, outcount, px[first0]);
      outcount++;
      
      MACRO_SET_ATOMIC(pout, outcount, px[last0]);
      outcount++;
    }
    else if(current_count == current_rnglen) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      for(R_xlen_t i = first0; i <= last0; ++i) {
        MACRO_SET_ATOMIC(pout, outcount, px[i]);
        outcount++;
      }
    }
    else {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      R_xlen_t i = first0;
      MACRO_STRIDEV_BITS_TRANSFER(
        MACRO_SET_ATOMIC(pout, outcount++, px[i]),
        first0,
        last0
      );
    }
  }
    
  UNPROTECT(1);
  return out;
    
}




SEXP rcpp_slicev_x_Complex(
  SEXP x, List preplist, NumericVector prepvector, SEXP pool
) {
  
  const Rcomplex *px = COMPLEX_RO(x);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  
  if(count_total == 0) {
    stop("no matches");
  }
  
  SEXP out = PROTECT(Rf_allocVector(CPLXSXP, count_total));
   Rcomplex *pout = COMPLEX(out);

  
  NumericVector first = preplist[0];
  NumericVector last = preplist[1];
  NumericVector count = preplist[2];
  NumericVector rnglen = preplist[3];
  const int n_chunks = Rf_length(first);
  
  R_xlen_t outcount = 0;
  
  for(int j = 0; j < n_chunks; ++j) {
    SEXP b32 = VECTOR_ELT(pool, j);
    
    const R_xlen_t current_count = count[j];
    const R_xlen_t current_rnglen = rnglen[j];
    
    if(current_count == 0) {
      continue;
    }
    else if(current_count == 1) {
      const R_xlen_t first0 = first[j];
      MACRO_SET_ATOMIC(pout, outcount, px[first0]);
      outcount++;
    }
    else if(current_count == 2) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      MACRO_SET_ATOMIC(pout, outcount, px[first0]);
      outcount++;
      
      MACRO_SET_ATOMIC(pout, outcount, px[last0]);
      outcount++;
    }
    else if(current_count == current_rnglen) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      for(R_xlen_t i = first0; i <= last0; ++i) {
        MACRO_SET_ATOMIC(pout, outcount, px[i]);
        outcount++;
      }
    }
    else {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      R_xlen_t i = first0;
      MACRO_STRIDEV_BITS_TRANSFER(
        MACRO_SET_ATOMIC(pout, outcount++, px[i]),
        first0,
        last0
      );
    }
  }
    
  UNPROTECT(1);
  return out;
    
}




SEXP rcpp_slicev_x_Raw(
  SEXP x, List preplist, NumericVector prepvector, SEXP pool
) {
  
  const Rbyte *px = RAW_RO(x);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  
  if(count_total == 0) {
    stop("no matches");
  }
  
  SEXP out = PROTECT(Rf_allocVector(RAWSXP, count_total));
   Rbyte *pout = RAW(out);

  
  NumericVector first = preplist[0];
  NumericVector last = preplist[1];
  NumericVector count = preplist[2];
  NumericVector rnglen = preplist[3];
  const int n_chunks = Rf_length(first);
  
  R_xlen_t outcount = 0;
  
  for(int j = 0; j < n_chunks; ++j) {
    SEXP b32 = VECTOR_ELT(pool, j);
    
    const R_xlen_t current_count = count[j];
    const R_xlen_t current_rnglen = rnglen[j];
    
    if(current_count == 0) {
      continue;
    }
    else if(current_count == 1) {
      const R_xlen_t first0 = first[j];
      MACRO_SET_ATOMIC(pout, outcount, px[first0]);
      outcount++;
    }
    else if(current_count == 2) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      MACRO_SET_ATOMIC(pout, outcount, px[first0]);
      outcount++;
      
      MACRO_SET_ATOMIC(pout, outcount, px[last0]);
      outcount++;
    }
    else if(current_count == current_rnglen) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      for(R_xlen_t i = first0; i <= last0; ++i) {
        MACRO_SET_ATOMIC(pout, outcount, px[i]);
        outcount++;
      }
    }
    else {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      R_xlen_t i = first0;
      MACRO_STRIDEV_BITS_TRANSFER(
        MACRO_SET_ATOMIC(pout, outcount++, px[i]),
        first0,
        last0
      );
    }
  }
    
  UNPROTECT(1);
  return out;
    
}




SEXP rcpp_slicev_x_Character(
  SEXP x, List preplist, NumericVector prepvector, SEXP pool
) {
  
  const SEXP *px = STRING_PTR_RO(x);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  
  if(count_total == 0) {
    stop("no matches");
  }
  
  SEXP out = PROTECT(Rf_allocVector(STRSXP, count_total));
  // SEXP *pout = STRING_PTR(out);

  
  NumericVector first = preplist[0];
  NumericVector last = preplist[1];
  NumericVector count = preplist[2];
  NumericVector rnglen = preplist[3];
  const int n_chunks = Rf_length(first);
  
  R_xlen_t outcount = 0;
  
  for(int j = 0; j < n_chunks; ++j) {
    SEXP b32 = VECTOR_ELT(pool, j);
    
    const R_xlen_t current_count = count[j];
    const R_xlen_t current_rnglen = rnglen[j];
    
    if(current_count == 0) {
      continue;
    }
    else if(current_count == 1) {
      const R_xlen_t first0 = first[j];
      SET_STRING_ELT(out, outcount, px[first0]);
      outcount++;
    }
    else if(current_count == 2) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      SET_STRING_ELT(out, outcount, px[first0]);
      outcount++;
      
      SET_STRING_ELT(out, outcount, px[last0]);
      outcount++;
    }
    else if(current_count == current_rnglen) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      for(R_xlen_t i = first0; i <= last0; ++i) {
        SET_STRING_ELT(out, outcount, px[i]);
        outcount++;
      }
    }
    else {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      R_xlen_t i = first0;
      MACRO_STRIDEV_BITS_TRANSFER(
        SET_STRING_ELT(out, outcount++, px[i]),
        first0,
        last0
      );
    }
  }
    
  UNPROTECT(1);
  return out;
    
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slicev_x_atomic)]]
SEXP rcpp_slicev_x_atomic(
  SEXP x, List preplist, NumericVector prepvector, SEXP pool
) {

    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
        return rcpp_slicev_x_Logical(x, preplist, prepvector, pool);
        break;
      }
      case INTSXP:
      {
        return rcpp_slicev_x_Integer(x, preplist, prepvector, pool);
        break;
      }
      case REALSXP:
      {
        return rcpp_slicev_x_Numeric(x, preplist, prepvector, pool);
        break;
      }
      case CPLXSXP:
      {
        return rcpp_slicev_x_Complex(x, preplist, prepvector, pool);
        break;
      }
      case RAWSXP:
      {
        return rcpp_slicev_x_Raw(x, preplist, prepvector, pool);
        break;
      }
      case STRSXP:
      {
        return rcpp_slicev_x_Character(x, preplist, prepvector, pool);
        break;
      }
      default: stop("unsupported type given");
    }
    
  
  return R_NilValue;
}





void rcpp_slicev_set_Logical(
  SEXP x, SEXP rp, List preplist, NumericVector prepvector, SEXP pool
) {
  
   int *px = LOGICAL(x);
  const int *prp = LOGICAL_RO(rp);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  
  R_xlen_t rpcount = 0;
  
  int by_rp;
  if(Rf_xlength(rp) == count_total) {
    by_rp = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    by_rp = 0;
  }
  else {
    stop("vector recycling not supported");
  }
  
  if(count_total == 0) {
    stop("no matches");
  }
  
  
  NumericVector first = preplist[0];
  NumericVector last = preplist[1];
  NumericVector count = preplist[2];
  NumericVector rnglen = preplist[3];
  const int n_chunks = Rf_length(first);
  
  
  for(int j = 0; j < n_chunks; ++j) {
    SEXP b32 = VECTOR_ELT(pool, j);
    
    const R_xlen_t current_count = count[j];
    const R_xlen_t current_rnglen = rnglen[j];
    
    
    if(current_count == 0) {
      continue;
    }
    else if(current_count == 1) {
      const R_xlen_t first0 = first[j];
      MACRO_SET_ATOMIC(px, first0, prp[rpcount]);
      rpcount += by_rp;
    }
    else if(current_count == 2) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      MACRO_SET_ATOMIC(px, first0, prp[rpcount]);
      rpcount += by_rp;
      
      MACRO_SET_ATOMIC(px, last0, prp[rpcount]);
      rpcount += by_rp;
    }
    else if(current_count == current_rnglen) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      for(R_xlen_t i = first0; i <= last0; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[rpcount]);
        rpcount += by_rp;
      }
    }
    else {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      R_xlen_t i = first0;
      MACRO_STRIDEV_BITS_TRANSFER(
        MACRO_SET_ATOMIC(px, i, prp[rpcount]); rpcount += by_rp,
        first0,
        last0
      );
    }
  }
}






void rcpp_slicev_set_Integer(
  SEXP x, SEXP rp, List preplist, NumericVector prepvector, SEXP pool
) {
  
   int *px = INTEGER(x);
  const int *prp = INTEGER_RO(rp);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  
  R_xlen_t rpcount = 0;
  
  int by_rp;
  if(Rf_xlength(rp) == count_total) {
    by_rp = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    by_rp = 0;
  }
  else {
    stop("vector recycling not supported");
  }
  
  if(count_total == 0) {
    stop("no matches");
  }
  
  
  NumericVector first = preplist[0];
  NumericVector last = preplist[1];
  NumericVector count = preplist[2];
  NumericVector rnglen = preplist[3];
  const int n_chunks = Rf_length(first);
  
  
  for(int j = 0; j < n_chunks; ++j) {
    SEXP b32 = VECTOR_ELT(pool, j);
    
    const R_xlen_t current_count = count[j];
    const R_xlen_t current_rnglen = rnglen[j];
    
    
    if(current_count == 0) {
      continue;
    }
    else if(current_count == 1) {
      const R_xlen_t first0 = first[j];
      MACRO_SET_ATOMIC(px, first0, prp[rpcount]);
      rpcount += by_rp;
    }
    else if(current_count == 2) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      MACRO_SET_ATOMIC(px, first0, prp[rpcount]);
      rpcount += by_rp;
      
      MACRO_SET_ATOMIC(px, last0, prp[rpcount]);
      rpcount += by_rp;
    }
    else if(current_count == current_rnglen) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      for(R_xlen_t i = first0; i <= last0; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[rpcount]);
        rpcount += by_rp;
      }
    }
    else {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      R_xlen_t i = first0;
      MACRO_STRIDEV_BITS_TRANSFER(
        MACRO_SET_ATOMIC(px, i, prp[rpcount]); rpcount += by_rp,
        first0,
        last0
      );
    }
  }
}






void rcpp_slicev_set_Numeric(
  SEXP x, SEXP rp, List preplist, NumericVector prepvector, SEXP pool
) {
  
   double *px = REAL(x);
  const double *prp = REAL_RO(rp);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  
  R_xlen_t rpcount = 0;
  
  int by_rp;
  if(Rf_xlength(rp) == count_total) {
    by_rp = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    by_rp = 0;
  }
  else {
    stop("vector recycling not supported");
  }
  
  if(count_total == 0) {
    stop("no matches");
  }
  
  
  NumericVector first = preplist[0];
  NumericVector last = preplist[1];
  NumericVector count = preplist[2];
  NumericVector rnglen = preplist[3];
  const int n_chunks = Rf_length(first);
  
  
  for(int j = 0; j < n_chunks; ++j) {
    SEXP b32 = VECTOR_ELT(pool, j);
    
    const R_xlen_t current_count = count[j];
    const R_xlen_t current_rnglen = rnglen[j];
    
    
    if(current_count == 0) {
      continue;
    }
    else if(current_count == 1) {
      const R_xlen_t first0 = first[j];
      MACRO_SET_ATOMIC(px, first0, prp[rpcount]);
      rpcount += by_rp;
    }
    else if(current_count == 2) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      MACRO_SET_ATOMIC(px, first0, prp[rpcount]);
      rpcount += by_rp;
      
      MACRO_SET_ATOMIC(px, last0, prp[rpcount]);
      rpcount += by_rp;
    }
    else if(current_count == current_rnglen) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      for(R_xlen_t i = first0; i <= last0; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[rpcount]);
        rpcount += by_rp;
      }
    }
    else {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      R_xlen_t i = first0;
      MACRO_STRIDEV_BITS_TRANSFER(
        MACRO_SET_ATOMIC(px, i, prp[rpcount]); rpcount += by_rp,
        first0,
        last0
      );
    }
  }
}






void rcpp_slicev_set_Complex(
  SEXP x, SEXP rp, List preplist, NumericVector prepvector, SEXP pool
) {
  
   Rcomplex *px = COMPLEX(x);
  const Rcomplex *prp = COMPLEX_RO(rp);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  
  R_xlen_t rpcount = 0;
  
  int by_rp;
  if(Rf_xlength(rp) == count_total) {
    by_rp = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    by_rp = 0;
  }
  else {
    stop("vector recycling not supported");
  }
  
  if(count_total == 0) {
    stop("no matches");
  }
  
  
  NumericVector first = preplist[0];
  NumericVector last = preplist[1];
  NumericVector count = preplist[2];
  NumericVector rnglen = preplist[3];
  const int n_chunks = Rf_length(first);
  
  
  for(int j = 0; j < n_chunks; ++j) {
    SEXP b32 = VECTOR_ELT(pool, j);
    
    const R_xlen_t current_count = count[j];
    const R_xlen_t current_rnglen = rnglen[j];
    
    
    if(current_count == 0) {
      continue;
    }
    else if(current_count == 1) {
      const R_xlen_t first0 = first[j];
      MACRO_SET_ATOMIC(px, first0, prp[rpcount]);
      rpcount += by_rp;
    }
    else if(current_count == 2) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      MACRO_SET_ATOMIC(px, first0, prp[rpcount]);
      rpcount += by_rp;
      
      MACRO_SET_ATOMIC(px, last0, prp[rpcount]);
      rpcount += by_rp;
    }
    else if(current_count == current_rnglen) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      for(R_xlen_t i = first0; i <= last0; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[rpcount]);
        rpcount += by_rp;
      }
    }
    else {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      R_xlen_t i = first0;
      MACRO_STRIDEV_BITS_TRANSFER(
        MACRO_SET_ATOMIC(px, i, prp[rpcount]); rpcount += by_rp,
        first0,
        last0
      );
    }
  }
}






void rcpp_slicev_set_Raw(
  SEXP x, SEXP rp, List preplist, NumericVector prepvector, SEXP pool
) {
  
   Rbyte *px = RAW(x);
  const Rbyte *prp = RAW_RO(rp);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  
  R_xlen_t rpcount = 0;
  
  int by_rp;
  if(Rf_xlength(rp) == count_total) {
    by_rp = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    by_rp = 0;
  }
  else {
    stop("vector recycling not supported");
  }
  
  if(count_total == 0) {
    stop("no matches");
  }
  
  
  NumericVector first = preplist[0];
  NumericVector last = preplist[1];
  NumericVector count = preplist[2];
  NumericVector rnglen = preplist[3];
  const int n_chunks = Rf_length(first);
  
  
  for(int j = 0; j < n_chunks; ++j) {
    SEXP b32 = VECTOR_ELT(pool, j);
    
    const R_xlen_t current_count = count[j];
    const R_xlen_t current_rnglen = rnglen[j];
    
    
    if(current_count == 0) {
      continue;
    }
    else if(current_count == 1) {
      const R_xlen_t first0 = first[j];
      MACRO_SET_ATOMIC(px, first0, prp[rpcount]);
      rpcount += by_rp;
    }
    else if(current_count == 2) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      MACRO_SET_ATOMIC(px, first0, prp[rpcount]);
      rpcount += by_rp;
      
      MACRO_SET_ATOMIC(px, last0, prp[rpcount]);
      rpcount += by_rp;
    }
    else if(current_count == current_rnglen) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      for(R_xlen_t i = first0; i <= last0; ++i) {
        MACRO_SET_ATOMIC(px, i, prp[rpcount]);
        rpcount += by_rp;
      }
    }
    else {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      R_xlen_t i = first0;
      MACRO_STRIDEV_BITS_TRANSFER(
        MACRO_SET_ATOMIC(px, i, prp[rpcount]); rpcount += by_rp,
        first0,
        last0
      );
    }
  }
}






void rcpp_slicev_set_Character(
  SEXP x, SEXP rp, List preplist, NumericVector prepvector, SEXP pool
) {
  
  // SEXP *px = STRING_PTR(x);
  const SEXP *prp = STRING_PTR_RO(rp);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  
  R_xlen_t rpcount = 0;
  
  int by_rp;
  if(Rf_xlength(rp) == count_total) {
    by_rp = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    by_rp = 0;
  }
  else {
    stop("vector recycling not supported");
  }
  
  if(count_total == 0) {
    stop("no matches");
  }
  
  
  NumericVector first = preplist[0];
  NumericVector last = preplist[1];
  NumericVector count = preplist[2];
  NumericVector rnglen = preplist[3];
  const int n_chunks = Rf_length(first);
  
  
  for(int j = 0; j < n_chunks; ++j) {
    SEXP b32 = VECTOR_ELT(pool, j);
    
    const R_xlen_t current_count = count[j];
    const R_xlen_t current_rnglen = rnglen[j];
    
    
    if(current_count == 0) {
      continue;
    }
    else if(current_count == 1) {
      const R_xlen_t first0 = first[j];
      SET_STRING_ELT(x, first0, prp[rpcount]);
      rpcount += by_rp;
    }
    else if(current_count == 2) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      SET_STRING_ELT(x, first0, prp[rpcount]);
      rpcount += by_rp;
      
      SET_STRING_ELT(x, last0, prp[rpcount]);
      rpcount += by_rp;
    }
    else if(current_count == current_rnglen) {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      
      for(R_xlen_t i = first0; i <= last0; ++i) {
        SET_STRING_ELT(x, i, prp[rpcount]);
        rpcount += by_rp;
      }
    }
    else {
      const R_xlen_t first0 = first[j];
      const R_xlen_t last0 = last[j];
      R_xlen_t i = first0;
      MACRO_STRIDEV_BITS_TRANSFER(
        SET_STRING_ELT(x, i, prp[rpcount]); rpcount += by_rp,
        first0,
        last0
      );
    }
  }
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slicev_set_atomic)]]
void rcpp_slicev_set_atomic(
  SEXP x, SEXP rp, List preplist, NumericVector prepvector, SEXP pool
) {

    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
         rcpp_slicev_set_Logical(x, rp, preplist, prepvector, pool);
        break;
      }
      case INTSXP:
      {
         rcpp_slicev_set_Integer(x, rp, preplist, prepvector, pool);
        break;
      }
      case REALSXP:
      {
         rcpp_slicev_set_Numeric(x, rp, preplist, prepvector, pool);
        break;
      }
      case CPLXSXP:
      {
         rcpp_slicev_set_Complex(x, rp, preplist, prepvector, pool);
        break;
      }
      case RAWSXP:
      {
         rcpp_slicev_set_Raw(x, rp, preplist, prepvector, pool);
        break;
      }
      case STRSXP:
      {
         rcpp_slicev_set_Character(x, rp, preplist, prepvector, pool);
        break;
      }
      default: stop("unsupported type given");
    }
    
  
}

