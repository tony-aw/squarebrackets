

#include <Rcpp.h>

#include "squarebrackets.h"

using namespace Rcpp;



inline int rcpp_count_stringmatches(String y, SEXP v) {
  int n = Rf_length(v);
  const SEXP *pv = STRING_PTR_RO(v);
  int count = 0;
  for(int i = 0; i < n; ++i) {
    if(y == pv[i]) {
      count++;
    }
  }
  return count;
}

#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \
  POINTER[INDEX] = REPLACEMENT; \
} while(0)



#define MACRO_SLICEV_DO_NARM(DOCODE) do {	\
  	\
  switch(TYPEOF(y)) {	\
    	\
    case LGLSXP:	\
    {	\
      bool condition = !invert[0];	\
      const int *py = LOGICAL(y);	\
      const int pv = LOGICAL(v)[0];	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if(py[i] != NA_LOGICAL && (py[i] == pv) == condition) {	\
          DOCODE;	\
        }	\
      }	\
      break;	\
    }	\
    case CPLXSXP:	\
    {	\
      bool condition = !invert[0];	\
      const Rcomplex *py = COMPLEX(y);	\
      const Rcomplex pv = COMPLEX(v)[0];	\
      bool checkNA;	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);	\
        if(!checkNA && (py[i].r == pv.r && py[i].i == pv.i) == condition) {	\
          DOCODE;	\
        }	\
      }	\
      break;	\
    }	\
    case RAWSXP :	\
    {	\
      bool condition = !invert[0];	\
      const Rbyte *py = RAW(y);	\
      const Rbyte pv = RAW(v)[0];	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if((py[i] == pv) == condition) {	\
          DOCODE;	\
        }	\
      }	\
      break;	\
    }	\
    case INTSXP:	\
    {	\
      bool condition = !invert[0];	\
      const int *py = INTEGER(y);	\
      if(Rf_xlength(v) == 1) {	\
        int pv;	\
        pv = Rf_asInteger(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(py[i] != NA_INTEGER && (py[i] == pv) == condition) {	\
            DOCODE;	\
          }	\
        }	\
      }	\
      else if(Rf_xlength(v) == 2) {	\
        double *pv = REAL(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(py[i] != NA_INTEGER && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {	\
            DOCODE;	\
          }	\
        }	\
      }	\
      else {	\
        stop("improper length for `v`");	\
      }	\
      break;	\
    }	\
    case REALSXP:	\
    {	\
      bool condition = !invert[0];	\
      const double *py = REAL(y);	\
      if(Rf_xlength(v) == 1) {	\
        double pv;	\
        pv = Rf_asReal(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(!R_isnancpp(py[i]) && (py[i] == pv) == condition) {	\
            DOCODE;	\
          }	\
        }	\
      }	\
      else if(Rf_xlength(v) == 2) {	\
        double *pv = REAL(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(!R_isnancpp(py[i]) && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {	\
            DOCODE;	\
          }	\
        }	\
      }	\
      else {	\
        stop("improper length for `v`");	\
      }	\
      	\
      break;	\
    }	\
    case STRSXP:	\
    {	\
      const SEXP *py = STRING_PTR_RO(y);	\
      if(Rf_length(v) == 1) {	\
        const SEXP *pv = STRING_PTR_RO(v);	\
        if(invert[0]) {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] != NA_STRING && py[i] != pv[0]) {	\
              DOCODE;	\
            }	\
          }	\
        }	\
        else {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] == pv[0]) {	\
              DOCODE;	\
            }	\
          }	\
        }	\
      }	\
      if(Rf_length(v) > 1) {	\
        if(invert[0]) {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] != NA_STRING) {	\
              if(rcpp_count_stringmatches(py[i], v) == 0) {	\
                DOCODE;	\
              }	\
              	\
            }	\
          }	\
        }	\
        else {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(rcpp_count_stringmatches(py[i], v) > 0) {	\
              DOCODE;	\
            }	\
          }	\
        }	\
      }	\
      	\
      break;	\
    }	\
    default: stop("Unsupported type ");	\
  }	\
} while(0)


#define MACRO_SLICEV_DO_NAKEEP(DOCODE) do {	\
  switch(TYPEOF(y)) {	\
    	\
    case LGLSXP:	\
    {	\
      bool condition = !invert[0];	\
      const int *py = LOGICAL(y);	\
      const int pv = LOGICAL(v)[0];	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if(py[i] == NA_LOGICAL || (py[i] == pv) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case CPLXSXP:	\
    {	\
      bool condition = !invert[0];	\
      const Rcomplex *py = COMPLEX(y);	\
      const Rcomplex pv = COMPLEX(v)[0];	\
      bool checkNA;	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);	\
        if(checkNA || (py[i].r == pv.r && py[i].i == pv.i) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case RAWSXP :	\
    {	\
      bool condition = !invert[0];	\
      const Rbyte *py = RAW(y);	\
      const Rbyte pv = RAW(v)[0];	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if((py[i] == pv) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case INTSXP:	\
    {	\
      bool condition = !invert[0];	\
      const int *py = INTEGER(y);	\
      if(Rf_xlength(v) == 1) {	\
        int pv;	\
        pv = Rf_asInteger(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(py[i] == NA_INTEGER || (py[i] == pv) == condition) {	\
            DOCODE;	\
            	\
          }	\
        }	\
      }	\
      else if(Rf_xlength(v) == 2) {	\
        double *pv = REAL(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(py[i] == NA_INTEGER || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {	\
            DOCODE;	\
            	\
          }	\
        }	\
      }	\
      else {	\
        stop("improper length for `v`");	\
      }	\
      break;	\
    }	\
    case REALSXP:	\
    {	\
      bool condition = !invert[0];	\
      const double *py = REAL(y);	\
      if(Rf_xlength(v) == 1) {	\
        double pv;	\
        pv = Rf_asReal(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(R_isnancpp(py[i]) || (py[i] == pv) == condition) {	\
            DOCODE;	\
            	\
          }	\
        }	\
      }	\
      else if(Rf_xlength(v) == 2) {	\
        double *pv = REAL(v);	\
        for(R_xlen_t i = start; i != (end + by); i += by) {	\
          if(R_isnancpp(py[i]) || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {	\
            DOCODE;	\
            	\
          }	\
        }	\
      }	\
      else {	\
        stop("improper length for `v`");	\
      }	\
      	\
      break;	\
    }	\
    case STRSXP:	\
    {	\
      const SEXP *py = STRING_PTR_RO(y);	\
      if(Rf_length(v) == 1) {	\
        const SEXP *pv = STRING_PTR_RO(v);	\
        if(invert[0]) {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] == NA_STRING || py[i] != pv[0]) {	\
              DOCODE;	\
              	\
            }	\
          }	\
        }	\
        else {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] == NA_STRING || py[i] == pv[0]) {	\
              DOCODE;	\
              	\
            }	\
          }	\
        }	\
      }	\
      if(Rf_length(v) > 1) {	\
        if(invert[0]) {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) == 0) {	\
              DOCODE;	\
              	\
            }	\
          }	\
        }	\
        else {	\
          for(R_xlen_t i = start; i != (end + by); i += by) {	\
            if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) > 0) {	\
              DOCODE;	\
              	\
            }	\
          }	\
        }	\
      }	\
      	\
      break;	\
    }	\
    default: stop("Unsupported type ");	\
  }	\
} while(0)


#define MACRO_SLICEV_DO_NAONLY(DOCODE) do {	\
  switch(TYPEOF(y)) {	\
    	\
    case LGLSXP:	\
    {	\
      bool condition = !invert[0];	\
      const int *py = LOGICAL(y);	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if((py[i] == NA_LOGICAL) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case INTSXP:	\
    {	\
      bool condition = !invert[0];	\
      const int *py = INTEGER(y);	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if((py[i] == NA_INTEGER) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case REALSXP:	\
    {	\
      bool condition = !invert[0];	\
      const double *py = REAL(y);	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if(R_isnancpp(py[i]) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case CPLXSXP:	\
    {	\
      const Rcomplex *py = COMPLEX(y);	\
      bool condition = !invert[0];	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if((R_isnancpp(py[i].r) || R_isnancpp(py[i].i)) == condition){	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case STRSXP:	\
    {	\
      const SEXP *py = STRING_PTR_RO(y);	\
      bool condition = !invert[0];	\
      for(R_xlen_t i = start; i != (end + by); i += by) {	\
        if((py[i] == NA_STRING) == condition) {	\
          DOCODE;	\
          	\
        }	\
      }	\
      break;	\
    }	\
    case RAWSXP :	\
    {	\
      stop("NAs not defined for type `raw`");	\
    }	\
    default: stop("Unsupported type ");	\
  }	\
} while(0)


#define MACRO_SLICEV_DO(DOCODE) do {	\
  if(LogicalVector::is_na(na[0])) {	\
    MACRO_SLICEV_DO_NAONLY(DOCODE);	\
  }	\
  else if(na[0]) {	\
    MACRO_SLICEV_DO_NAKEEP(DOCODE);	\
  }	\
  else if(!na[0]) {	\
    MACRO_SLICEV_DO_NARM(DOCODE);	\
  }	\
  else {	\
    stop("unknow value for `na` given");	\
  }	\
} while(0)



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_countv)]]
R_xlen_t rcpp_countv(
    SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    if(len == 0) {
      return count;
    }
    
    MACRO_SLICEV_DO(count++);
  
    return count;
  }

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_whichv_32)]]
IntegerVector rcpp_whichv_32(
    SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
   
    R_xlen_t amount = rcpp_countv(y, v, na, invert, start, end, by, len);
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
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    int *px = LOGICAL(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert, start, end, by, len);
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
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    int *px = INTEGER(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert, start, end, by, len);
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
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    double *px = REAL(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert, start, end, by, len);
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
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    Rcomplex *px = COMPLEX(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert, start, end, by, len);
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
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    Rbyte *px = RAW(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert, start, end, by, len);
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
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    SEXP *px = STRING_PTR(x);
    
    R_xlen_t size = rcpp_countv(y, v, na, invert, start, end, by, len);
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
  SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
) {

    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
        return rcpp_slicev_x_Logical(x, y, v, na, invert, start, end, by, len);
        break;
      }
      case INTSXP:
      {
        return rcpp_slicev_x_Integer(x, y, v, na, invert, start, end, by, len);
        break;
      }
      case REALSXP:
      {
        return rcpp_slicev_x_Numeric(x, y, v, na, invert, start, end, by, len);
        break;
      }
      case CPLXSXP:
      {
        return rcpp_slicev_x_Complex(x, y, v, na, invert, start, end, by, len);
        break;
      }
      case RAWSXP:
      {
        return rcpp_slicev_x_Raw(x, y, v, na, invert, start, end, by, len);
        break;
      }
      case STRSXP:
      {
        return rcpp_slicev_x_Character(x, y, v, na, invert, start, end, by, len);
        break;
      }
      default: stop("unsupported type given");
    }
    
  
  return R_NilValue;
}


void rcpp_slicev_set_Logical(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
  }
  
   int *px = LOGICAL(x);
  int *prp = LOGICAL(rp);

  if(Rf_xlength(rp) == 1 && len > 0) {
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && len > 0) {
    R_xlen_t count = 0;
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[count]); count++);
  }
  // don't use error for replacement length:
  // that is handled in 'R';
  // placing it here ruins replacement through transformation, which may be of len zero
  
}
  
  



void rcpp_slicev_set_Integer(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
  }
  
   int *px = INTEGER(x);
  int *prp = INTEGER(rp);

  if(Rf_xlength(rp) == 1 && len > 0) {
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && len > 0) {
    R_xlen_t count = 0;
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[count]); count++);
  }
  // don't use error for replacement length:
  // that is handled in 'R';
  // placing it here ruins replacement through transformation, which may be of len zero
  
}
  
  



void rcpp_slicev_set_Numeric(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
  }
  
   double *px = REAL(x);
  double *prp = REAL(rp);

  if(Rf_xlength(rp) == 1 && len > 0) {
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && len > 0) {
    R_xlen_t count = 0;
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[count]); count++);
  }
  // don't use error for replacement length:
  // that is handled in 'R';
  // placing it here ruins replacement through transformation, which may be of len zero
  
}
  
  



void rcpp_slicev_set_Complex(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
  }
  
   Rcomplex *px = COMPLEX(x);
  Rcomplex *prp = COMPLEX(rp);

  if(Rf_xlength(rp) == 1 && len > 0) {
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && len > 0) {
    R_xlen_t count = 0;
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[count]); count++);
  }
  // don't use error for replacement length:
  // that is handled in 'R';
  // placing it here ruins replacement through transformation, which may be of len zero
  
}
  
  



void rcpp_slicev_set_Raw(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
  }
  
   Rbyte *px = RAW(x);
  Rbyte *prp = RAW(rp);

  if(Rf_xlength(rp) == 1 && len > 0) {
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && len > 0) {
    R_xlen_t count = 0;
    MACRO_SLICEV_DO(MACRO_SET_ATOMIC(px, i, prp[count]); count++);
  }
  // don't use error for replacement length:
  // that is handled in 'R';
  // placing it here ruins replacement through transformation, which may be of len zero
  
}
  
  



void rcpp_slicev_set_Character(
    SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len, SEXP rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
  }
  
  // SEXP *px = STRING_PTR(x);
  SEXP *prp = STRING_PTR(rp);

  if(Rf_xlength(rp) == 1 && len > 0) {
    MACRO_SLICEV_DO(SET_STRING_ELT(x, i, prp[0]));
  }
  else if(Rf_xlength(rp) > 1 && len > 0) {
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
  SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len, SEXP rp
) {

    switch(TYPEOF(x)){
    
      case LGLSXP:
      {
         rcpp_slicev_set_Logical(x, y, v, na, invert, start, end, by, len, rp);
        break;
      }
      case INTSXP:
      {
         rcpp_slicev_set_Integer(x, y, v, na, invert, start, end, by, len, rp);
        break;
      }
      case REALSXP:
      {
         rcpp_slicev_set_Numeric(x, y, v, na, invert, start, end, by, len, rp);
        break;
      }
      case CPLXSXP:
      {
         rcpp_slicev_set_Complex(x, y, v, na, invert, start, end, by, len, rp);
        break;
      }
      case RAWSXP:
      {
         rcpp_slicev_set_Raw(x, y, v, na, invert, start, end, by, len, rp);
        break;
      }
      case STRSXP:
      {
         rcpp_slicev_set_Character(x, y, v, na, invert, start, end, by, len, rp);
        break;
      }
      default: stop("unsupported type given");
    }
    
  
}

