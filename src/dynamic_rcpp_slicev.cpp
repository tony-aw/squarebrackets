

#include <Rcpp.h>

using namespace Rcpp;


int rcpp_count_stringmatches(String y, SEXP v) {
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
    
    
  if(LogicalVector::is_na(na[0])) {
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_LOGICAL) == condition) {
        count++;
        
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_INTEGER) == condition) {
        count++;
        
      }
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(R_isnancpp(py[i]) == condition) {
        count++;
        
      }
    }
    break;
  }
  case CPLXSXP:
  {
    const Rcomplex *py = COMPLEX(y);
    bool condition = !invert[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((R_isnancpp(py[i].r) || R_isnancpp(py[i].i)) == condition){
        count++;
        
      }
    }
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    bool condition = !invert[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_STRING) == condition) {
        count++;
        
      }
    }
    break;
  }
  case RAWSXP :
  {
    stop("NAs not defined for type `raw`");
  }
  default: stop("Unsupported type ");
}


  }
  
  else if(na[0]) {
    
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    const int pv = LOGICAL(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(py[i] == NA_LOGICAL || (py[i] == pv) == condition) {
        count++;
        
      }
    }
    break;
  }
  case CPLXSXP:
  {
    bool condition = !invert[0];
    const Rcomplex *py = COMPLEX(y);
    const Rcomplex pv = COMPLEX(v)[0];
    bool checkNA;
    for(R_xlen_t i = start; i != (end + by); i += by) {
      checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);
      if(checkNA || (py[i].r == pv.r && py[i].i == pv.i) == condition) {
        count++;
        
      }
    }
    break;
  }
  case RAWSXP :
  {
    bool condition = !invert[0];
    const Rbyte *py = RAW(y);
    const Rbyte pv = RAW(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == pv) == condition) {
        count++;
        
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    if(Rf_xlength(v) == 1) {
      int pv;
      pv = Rf_asInteger(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] == NA_INTEGER || (py[i] == pv) == condition) {
          count++;
          
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] == NA_INTEGER || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          count++;
          
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    if(Rf_xlength(v) == 1) {
      double pv;
      pv = Rf_asReal(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(R_isnancpp(py[i]) || (py[i] == pv) == condition) {
          count++;
          
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(R_isnancpp(py[i]) || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          count++;
          
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    if(Rf_length(v) == 1) {
      const SEXP *pv = STRING_PTR_RO(v);
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || py[i] != pv[0]) {
            count++;
            
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || py[i] == pv[0]) {
            count++;
            
          }
        }
      }
    }
    if(Rf_length(v) > 1) {
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) == 0) {
            count++;
            
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) > 0) {
            count++;
            
          }
        }
      }
    }
    
    break;
  }
  default: stop("Unsupported type ");
}


  }
  else if(!na[0]) {
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    const int pv = LOGICAL(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(py[i] != NA_LOGICAL && (py[i] == pv) == condition) {
        count++;
        
      }
    }
    break;
  }
  case CPLXSXP:
  {
    bool condition = !invert[0];
    const Rcomplex *py = COMPLEX(y);
    const Rcomplex pv = COMPLEX(v)[0];
    bool checkNA;
    for(R_xlen_t i = start; i != (end + by); i += by) {
      checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);
      if(!checkNA && (py[i].r == pv.r && py[i].i == pv.i) == condition) {
        count++;
        
      }
    }
    break;
  }
  case RAWSXP :
  {
    bool condition = !invert[0];
    const Rbyte *py = RAW(y);
    const Rbyte pv = RAW(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == pv) == condition) {
        count++;
        
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    if(Rf_xlength(v) == 1) {
      int pv;
      pv = Rf_asInteger(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] != NA_INTEGER && (py[i] == pv) == condition) {
          count++;
          
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] != NA_INTEGER && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          count++;
          
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    if(Rf_xlength(v) == 1) {
      double pv;
      pv = Rf_asReal(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(!R_isnancpp(py[i]) && (py[i] == pv) == condition) {
          count++;
          
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(!R_isnancpp(py[i]) && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          count++;
          
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    if(Rf_length(v) == 1) {
      const SEXP *pv = STRING_PTR_RO(v);
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_STRING && py[i] != pv[0]) {
            count++;
            
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == pv[0]) {
            count++;
            
          }
        }
      }
    }
    if(Rf_length(v) > 1) {
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_STRING) {
            if(rcpp_count_stringmatches(py[i], v) == 0) {
              count++;
              
            }
            
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(rcpp_count_stringmatches(py[i], v) > 0) {
            count++;
            
          }
        }
      }
    }
    
    break;
  }
  default: stop("Unsupported type ");
}


    
  }
  
  
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
    
    
  if(LogicalVector::is_na(na[0])) {
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_LOGICAL) == condition) {
        pout[count] = i + 1;
        count++;
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_INTEGER) == condition) {
        pout[count] = i + 1;
        count++;
      }
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(R_isnancpp(py[i]) == condition) {
        pout[count] = i + 1;
        count++;
      }
    }
    break;
  }
  case CPLXSXP:
  {
    const Rcomplex *py = COMPLEX(y);
    bool condition = !invert[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((R_isnancpp(py[i].r) || R_isnancpp(py[i].i)) == condition){
        pout[count] = i + 1;
        count++;
      }
    }
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    bool condition = !invert[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_STRING) == condition) {
        pout[count] = i + 1;
        count++;
      }
    }
    break;
  }
  case RAWSXP :
  {
    stop("NAs not defined for type `raw`");
  }
  default: stop("Unsupported type ");
}


  }
  
  else if(na[0]) {
    
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    const int pv = LOGICAL(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(py[i] == NA_LOGICAL || (py[i] == pv) == condition) {
        pout[count] = i + 1;
        count++;
      }
    }
    break;
  }
  case CPLXSXP:
  {
    bool condition = !invert[0];
    const Rcomplex *py = COMPLEX(y);
    const Rcomplex pv = COMPLEX(v)[0];
    bool checkNA;
    for(R_xlen_t i = start; i != (end + by); i += by) {
      checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);
      if(checkNA || (py[i].r == pv.r && py[i].i == pv.i) == condition) {
        pout[count] = i + 1;
        count++;
      }
    }
    break;
  }
  case RAWSXP :
  {
    bool condition = !invert[0];
    const Rbyte *py = RAW(y);
    const Rbyte pv = RAW(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == pv) == condition) {
        pout[count] = i + 1;
        count++;
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    if(Rf_xlength(v) == 1) {
      int pv;
      pv = Rf_asInteger(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] == NA_INTEGER || (py[i] == pv) == condition) {
          pout[count] = i + 1;
          count++;
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] == NA_INTEGER || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          pout[count] = i + 1;
          count++;
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    if(Rf_xlength(v) == 1) {
      double pv;
      pv = Rf_asReal(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(R_isnancpp(py[i]) || (py[i] == pv) == condition) {
          pout[count] = i + 1;
          count++;
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(R_isnancpp(py[i]) || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          pout[count] = i + 1;
          count++;
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    if(Rf_length(v) == 1) {
      const SEXP *pv = STRING_PTR_RO(v);
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || py[i] != pv[0]) {
            pout[count] = i + 1;
            count++;
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || py[i] == pv[0]) {
            pout[count] = i + 1;
            count++;
          }
        }
      }
    }
    if(Rf_length(v) > 1) {
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) == 0) {
            pout[count] = i + 1;
            count++;
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) > 0) {
            pout[count] = i + 1;
            count++;
          }
        }
      }
    }
    
    break;
  }
  default: stop("Unsupported type ");
}


  }
  else if(!na[0]) {
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    const int pv = LOGICAL(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(py[i] != NA_LOGICAL && (py[i] == pv) == condition) {
        pout[count] = i + 1;
        count++;
      }
    }
    break;
  }
  case CPLXSXP:
  {
    bool condition = !invert[0];
    const Rcomplex *py = COMPLEX(y);
    const Rcomplex pv = COMPLEX(v)[0];
    bool checkNA;
    for(R_xlen_t i = start; i != (end + by); i += by) {
      checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);
      if(!checkNA && (py[i].r == pv.r && py[i].i == pv.i) == condition) {
        pout[count] = i + 1;
        count++;
      }
    }
    break;
  }
  case RAWSXP :
  {
    bool condition = !invert[0];
    const Rbyte *py = RAW(y);
    const Rbyte pv = RAW(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == pv) == condition) {
        pout[count] = i + 1;
        count++;
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    if(Rf_xlength(v) == 1) {
      int pv;
      pv = Rf_asInteger(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] != NA_INTEGER && (py[i] == pv) == condition) {
          pout[count] = i + 1;
          count++;
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] != NA_INTEGER && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          pout[count] = i + 1;
          count++;
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    if(Rf_xlength(v) == 1) {
      double pv;
      pv = Rf_asReal(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(!R_isnancpp(py[i]) && (py[i] == pv) == condition) {
          pout[count] = i + 1;
          count++;
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(!R_isnancpp(py[i]) && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          pout[count] = i + 1;
          count++;
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    if(Rf_length(v) == 1) {
      const SEXP *pv = STRING_PTR_RO(v);
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_STRING && py[i] != pv[0]) {
            pout[count] = i + 1;
            count++;
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == pv[0]) {
            pout[count] = i + 1;
            count++;
          }
        }
      }
    }
    if(Rf_length(v) > 1) {
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_STRING) {
            if(rcpp_count_stringmatches(py[i], v) == 0) {
              pout[count] = i + 1;
              count++;
            }
            
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(rcpp_count_stringmatches(py[i], v) > 0) {
            pout[count] = i + 1;
            count++;
          }
        }
      }
    }
    
    break;
  }
  default: stop("Unsupported type ");
}


    
  }
  


  
  UNPROTECT(1);
    return out;
  }
  
  

template<int RTYPE> Vector<RTYPE> rcpp_slicev_x_template(
    Vector<RTYPE> x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
    
    R_xlen_t size = rcpp_countv(y, v, na, invert, start, end, by, len);
    Vector<RTYPE> out(size);
    
    if(size == 0) {
      return out;
    }
    
  
  if(LogicalVector::is_na(na[0])) {
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_LOGICAL) == condition) {
        out[count] = x[i];
        count++;
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_INTEGER) == condition) {
        out[count] = x[i];
        count++;
      }
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(R_isnancpp(py[i]) == condition) {
        out[count] = x[i];
        count++;
      }
    }
    break;
  }
  case CPLXSXP:
  {
    const Rcomplex *py = COMPLEX(y);
    bool condition = !invert[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((R_isnancpp(py[i].r) || R_isnancpp(py[i].i)) == condition){
        out[count] = x[i];
        count++;
      }
    }
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    bool condition = !invert[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_STRING) == condition) {
        out[count] = x[i];
        count++;
      }
    }
    break;
  }
  case RAWSXP :
  {
    stop("NAs not defined for type `raw`");
  }
  default: stop("Unsupported type ");
}


  }
  
  else if(na[0]) {
    
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    const int pv = LOGICAL(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(py[i] == NA_LOGICAL || (py[i] == pv) == condition) {
        out[count] = x[i];
        count++;
      }
    }
    break;
  }
  case CPLXSXP:
  {
    bool condition = !invert[0];
    const Rcomplex *py = COMPLEX(y);
    const Rcomplex pv = COMPLEX(v)[0];
    bool checkNA;
    for(R_xlen_t i = start; i != (end + by); i += by) {
      checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);
      if(checkNA || (py[i].r == pv.r && py[i].i == pv.i) == condition) {
        out[count] = x[i];
        count++;
      }
    }
    break;
  }
  case RAWSXP :
  {
    bool condition = !invert[0];
    const Rbyte *py = RAW(y);
    const Rbyte pv = RAW(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == pv) == condition) {
        out[count] = x[i];
        count++;
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    if(Rf_xlength(v) == 1) {
      int pv;
      pv = Rf_asInteger(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] == NA_INTEGER || (py[i] == pv) == condition) {
          out[count] = x[i];
          count++;
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] == NA_INTEGER || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          out[count] = x[i];
          count++;
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    if(Rf_xlength(v) == 1) {
      double pv;
      pv = Rf_asReal(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(R_isnancpp(py[i]) || (py[i] == pv) == condition) {
          out[count] = x[i];
          count++;
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(R_isnancpp(py[i]) || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          out[count] = x[i];
          count++;
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    if(Rf_length(v) == 1) {
      const SEXP *pv = STRING_PTR_RO(v);
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || py[i] != pv[0]) {
            out[count] = x[i];
            count++;
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || py[i] == pv[0]) {
            out[count] = x[i];
            count++;
          }
        }
      }
    }
    if(Rf_length(v) > 1) {
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) == 0) {
            out[count] = x[i];
            count++;
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) > 0) {
            out[count] = x[i];
            count++;
          }
        }
      }
    }
    
    break;
  }
  default: stop("Unsupported type ");
}


  }
  else if(!na[0]) {
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    const int pv = LOGICAL(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(py[i] != NA_LOGICAL && (py[i] == pv) == condition) {
        out[count] = x[i];
        count++;
      }
    }
    break;
  }
  case CPLXSXP:
  {
    bool condition = !invert[0];
    const Rcomplex *py = COMPLEX(y);
    const Rcomplex pv = COMPLEX(v)[0];
    bool checkNA;
    for(R_xlen_t i = start; i != (end + by); i += by) {
      checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);
      if(!checkNA && (py[i].r == pv.r && py[i].i == pv.i) == condition) {
        out[count] = x[i];
        count++;
      }
    }
    break;
  }
  case RAWSXP :
  {
    bool condition = !invert[0];
    const Rbyte *py = RAW(y);
    const Rbyte pv = RAW(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == pv) == condition) {
        out[count] = x[i];
        count++;
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    if(Rf_xlength(v) == 1) {
      int pv;
      pv = Rf_asInteger(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] != NA_INTEGER && (py[i] == pv) == condition) {
          out[count] = x[i];
          count++;
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] != NA_INTEGER && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          out[count] = x[i];
          count++;
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    if(Rf_xlength(v) == 1) {
      double pv;
      pv = Rf_asReal(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(!R_isnancpp(py[i]) && (py[i] == pv) == condition) {
          out[count] = x[i];
          count++;
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(!R_isnancpp(py[i]) && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          out[count] = x[i];
          count++;
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    if(Rf_length(v) == 1) {
      const SEXP *pv = STRING_PTR_RO(v);
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_STRING && py[i] != pv[0]) {
            out[count] = x[i];
            count++;
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == pv[0]) {
            out[count] = x[i];
            count++;
          }
        }
      }
    }
    if(Rf_length(v) > 1) {
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_STRING) {
            if(rcpp_count_stringmatches(py[i], v) == 0) {
              out[count] = x[i];
              count++;
            }
            
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(rcpp_count_stringmatches(py[i], v) > 0) {
            out[count] = x[i];
            count++;
          }
        }
      }
    }
    
    break;
  }
  default: stop("Unsupported type ");
}


    
  }
  
  
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
    return rcpp_slicev_x_template<LGLSXP>(as<LogicalVector>(x), y, v, na, invert, start, end, by, len);
  }


  case INTSXP:
  {
    return rcpp_slicev_x_template<INTSXP>(as<IntegerVector>(x), y, v, na, invert, start, end, by, len);
  }


  case REALSXP:
  {
    return rcpp_slicev_x_template<REALSXP>(as<NumericVector>(x), y, v, na, invert, start, end, by, len);
  }


  case CPLXSXP:
  {
    return rcpp_slicev_x_template<CPLXSXP>(as<ComplexVector>(x), y, v, na, invert, start, end, by, len);
  }


  case STRSXP:
  {
    return rcpp_slicev_x_template<STRSXP>(as<CharacterVector>(x), y, v, na, invert, start, end, by, len);
  }


  case RAWSXP:
  {
    return rcpp_slicev_x_template<RAWSXP>(as<RawVector>(x), y, v, na, invert, start, end, by, len);
  }

  default: stop("unsupported type");
}
  return R_NilValue;
}


template<int RTYPE> void rcpp_slicev_set_template(
    Vector<RTYPE> x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len, Vector<RTYPE> rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop("`x` and `y` must have equal lengths");
    }
  
  if(Rf_xlength(rp) == 1 && len > 0) {
    
  if(LogicalVector::is_na(na[0])) {
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_LOGICAL) == condition) {
        x[i] = rp[0];
        
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_INTEGER) == condition) {
        x[i] = rp[0];
        
      }
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(R_isnancpp(py[i]) == condition) {
        x[i] = rp[0];
        
      }
    }
    break;
  }
  case CPLXSXP:
  {
    const Rcomplex *py = COMPLEX(y);
    bool condition = !invert[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((R_isnancpp(py[i].r) || R_isnancpp(py[i].i)) == condition){
        x[i] = rp[0];
        
      }
    }
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    bool condition = !invert[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_STRING) == condition) {
        x[i] = rp[0];
        
      }
    }
    break;
  }
  case RAWSXP :
  {
    stop("NAs not defined for type `raw`");
  }
  default: stop("Unsupported type ");
}


  }
  
  else if(na[0]) {
    
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    const int pv = LOGICAL(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(py[i] == NA_LOGICAL || (py[i] == pv) == condition) {
        x[i] = rp[0];
        
      }
    }
    break;
  }
  case CPLXSXP:
  {
    bool condition = !invert[0];
    const Rcomplex *py = COMPLEX(y);
    const Rcomplex pv = COMPLEX(v)[0];
    bool checkNA;
    for(R_xlen_t i = start; i != (end + by); i += by) {
      checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);
      if(checkNA || (py[i].r == pv.r && py[i].i == pv.i) == condition) {
        x[i] = rp[0];
        
      }
    }
    break;
  }
  case RAWSXP :
  {
    bool condition = !invert[0];
    const Rbyte *py = RAW(y);
    const Rbyte pv = RAW(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == pv) == condition) {
        x[i] = rp[0];
        
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    if(Rf_xlength(v) == 1) {
      int pv;
      pv = Rf_asInteger(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] == NA_INTEGER || (py[i] == pv) == condition) {
          x[i] = rp[0];
          
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] == NA_INTEGER || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          x[i] = rp[0];
          
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    if(Rf_xlength(v) == 1) {
      double pv;
      pv = Rf_asReal(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(R_isnancpp(py[i]) || (py[i] == pv) == condition) {
          x[i] = rp[0];
          
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(R_isnancpp(py[i]) || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          x[i] = rp[0];
          
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    if(Rf_length(v) == 1) {
      const SEXP *pv = STRING_PTR_RO(v);
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || py[i] != pv[0]) {
            x[i] = rp[0];
            
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || py[i] == pv[0]) {
            x[i] = rp[0];
            
          }
        }
      }
    }
    if(Rf_length(v) > 1) {
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) == 0) {
            x[i] = rp[0];
            
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) > 0) {
            x[i] = rp[0];
            
          }
        }
      }
    }
    
    break;
  }
  default: stop("Unsupported type ");
}


  }
  else if(!na[0]) {
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    const int pv = LOGICAL(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(py[i] != NA_LOGICAL && (py[i] == pv) == condition) {
        x[i] = rp[0];
        
      }
    }
    break;
  }
  case CPLXSXP:
  {
    bool condition = !invert[0];
    const Rcomplex *py = COMPLEX(y);
    const Rcomplex pv = COMPLEX(v)[0];
    bool checkNA;
    for(R_xlen_t i = start; i != (end + by); i += by) {
      checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);
      if(!checkNA && (py[i].r == pv.r && py[i].i == pv.i) == condition) {
        x[i] = rp[0];
        
      }
    }
    break;
  }
  case RAWSXP :
  {
    bool condition = !invert[0];
    const Rbyte *py = RAW(y);
    const Rbyte pv = RAW(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == pv) == condition) {
        x[i] = rp[0];
        
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    if(Rf_xlength(v) == 1) {
      int pv;
      pv = Rf_asInteger(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] != NA_INTEGER && (py[i] == pv) == condition) {
          x[i] = rp[0];
          
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] != NA_INTEGER && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          x[i] = rp[0];
          
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    if(Rf_xlength(v) == 1) {
      double pv;
      pv = Rf_asReal(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(!R_isnancpp(py[i]) && (py[i] == pv) == condition) {
          x[i] = rp[0];
          
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(!R_isnancpp(py[i]) && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          x[i] = rp[0];
          
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    if(Rf_length(v) == 1) {
      const SEXP *pv = STRING_PTR_RO(v);
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_STRING && py[i] != pv[0]) {
            x[i] = rp[0];
            
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == pv[0]) {
            x[i] = rp[0];
            
          }
        }
      }
    }
    if(Rf_length(v) > 1) {
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_STRING) {
            if(rcpp_count_stringmatches(py[i], v) == 0) {
              x[i] = rp[0];
              
            }
            
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(rcpp_count_stringmatches(py[i], v) > 0) {
            x[i] = rp[0];
            
          }
        }
      }
    }
    
    break;
  }
  default: stop("Unsupported type ");
}


    
  }
  
  }
  else if(Rf_xlength(rp) > 1 && len > 0) {
    R_xlen_t count = 0;
    
  if(LogicalVector::is_na(na[0])) {
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_LOGICAL) == condition) {
        x[i] = rp[count];
        count++;
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_INTEGER) == condition) {
        x[i] = rp[count];
        count++;
      }
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(R_isnancpp(py[i]) == condition) {
        x[i] = rp[count];
        count++;
      }
    }
    break;
  }
  case CPLXSXP:
  {
    const Rcomplex *py = COMPLEX(y);
    bool condition = !invert[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((R_isnancpp(py[i].r) || R_isnancpp(py[i].i)) == condition){
        x[i] = rp[count];
        count++;
      }
    }
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    bool condition = !invert[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_STRING) == condition) {
        x[i] = rp[count];
        count++;
      }
    }
    break;
  }
  case RAWSXP :
  {
    stop("NAs not defined for type `raw`");
  }
  default: stop("Unsupported type ");
}


  }
  
  else if(na[0]) {
    
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    const int pv = LOGICAL(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(py[i] == NA_LOGICAL || (py[i] == pv) == condition) {
        x[i] = rp[count];
        count++;
      }
    }
    break;
  }
  case CPLXSXP:
  {
    bool condition = !invert[0];
    const Rcomplex *py = COMPLEX(y);
    const Rcomplex pv = COMPLEX(v)[0];
    bool checkNA;
    for(R_xlen_t i = start; i != (end + by); i += by) {
      checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);
      if(checkNA || (py[i].r == pv.r && py[i].i == pv.i) == condition) {
        x[i] = rp[count];
        count++;
      }
    }
    break;
  }
  case RAWSXP :
  {
    bool condition = !invert[0];
    const Rbyte *py = RAW(y);
    const Rbyte pv = RAW(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == pv) == condition) {
        x[i] = rp[count];
        count++;
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    if(Rf_xlength(v) == 1) {
      int pv;
      pv = Rf_asInteger(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] == NA_INTEGER || (py[i] == pv) == condition) {
          x[i] = rp[count];
          count++;
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] == NA_INTEGER || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          x[i] = rp[count];
          count++;
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    if(Rf_xlength(v) == 1) {
      double pv;
      pv = Rf_asReal(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(R_isnancpp(py[i]) || (py[i] == pv) == condition) {
          x[i] = rp[count];
          count++;
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(R_isnancpp(py[i]) || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          x[i] = rp[count];
          count++;
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    if(Rf_length(v) == 1) {
      const SEXP *pv = STRING_PTR_RO(v);
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || py[i] != pv[0]) {
            x[i] = rp[count];
            count++;
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || py[i] == pv[0]) {
            x[i] = rp[count];
            count++;
          }
        }
      }
    }
    if(Rf_length(v) > 1) {
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) == 0) {
            x[i] = rp[count];
            count++;
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) > 0) {
            x[i] = rp[count];
            count++;
          }
        }
      }
    }
    
    break;
  }
  default: stop("Unsupported type ");
}


  }
  else if(!na[0]) {
    

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    const int pv = LOGICAL(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(py[i] != NA_LOGICAL && (py[i] == pv) == condition) {
        x[i] = rp[count];
        count++;
      }
    }
    break;
  }
  case CPLXSXP:
  {
    bool condition = !invert[0];
    const Rcomplex *py = COMPLEX(y);
    const Rcomplex pv = COMPLEX(v)[0];
    bool checkNA;
    for(R_xlen_t i = start; i != (end + by); i += by) {
      checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);
      if(!checkNA && (py[i].r == pv.r && py[i].i == pv.i) == condition) {
        x[i] = rp[count];
        count++;
      }
    }
    break;
  }
  case RAWSXP :
  {
    bool condition = !invert[0];
    const Rbyte *py = RAW(y);
    const Rbyte pv = RAW(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == pv) == condition) {
        x[i] = rp[count];
        count++;
      }
    }
    break;
  }
  case INTSXP:
  {
    bool condition = !invert[0];
    const int *py = INTEGER(y);
    if(Rf_xlength(v) == 1) {
      int pv;
      pv = Rf_asInteger(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] != NA_INTEGER && (py[i] == pv) == condition) {
          x[i] = rp[count];
          count++;
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] != NA_INTEGER && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          x[i] = rp[count];
          count++;
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    break;
  }
  case REALSXP:
  {
    bool condition = !invert[0];
    const double *py = REAL(y);
    if(Rf_xlength(v) == 1) {
      double pv;
      pv = Rf_asReal(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(!R_isnancpp(py[i]) && (py[i] == pv) == condition) {
          x[i] = rp[count];
          count++;
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(!R_isnancpp(py[i]) && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          x[i] = rp[count];
          count++;
        }
      }
    }
    else {
      stop("improper length for `v`");
    }
    
    break;
  }
  case STRSXP:
  {
    const SEXP *py = STRING_PTR_RO(y);
    if(Rf_length(v) == 1) {
      const SEXP *pv = STRING_PTR_RO(v);
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_STRING && py[i] != pv[0]) {
            x[i] = rp[count];
            count++;
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == pv[0]) {
            x[i] = rp[count];
            count++;
          }
        }
      }
    }
    if(Rf_length(v) > 1) {
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_STRING) {
            if(rcpp_count_stringmatches(py[i], v) == 0) {
              x[i] = rp[count];
              count++;
            }
            
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(rcpp_count_stringmatches(py[i], v) > 0) {
            x[i] = rp[count];
            count++;
          }
        }
      }
    }
    
    break;
  }
  default: stop("Unsupported type ");
}


    
  }
  
  }
   
  
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
    rcpp_slicev_set_template<LGLSXP>(as<LogicalVector>(x), y, v, na, invert, start, end, by, len, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_slicev_set_template<INTSXP>(as<IntegerVector>(x), y, v, na, invert, start, end, by, len, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_slicev_set_template<REALSXP>(as<NumericVector>(x), y, v, na, invert, start, end, by, len, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_slicev_set_template<CPLXSXP>(as<ComplexVector>(x), y, v, na, invert, start, end, by, len, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_slicev_set_template<STRSXP>(as<CharacterVector>(x), y, v, na, invert, start, end, by, len, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_slicev_set_template<RAWSXP>(as<RawVector>(x), y, v, na, invert, start, end, by, len, as<RawVector>(rp));
    break;
  }

  default: stop("unsupported type");
}
}

