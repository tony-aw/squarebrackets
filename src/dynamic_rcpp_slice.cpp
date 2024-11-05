

#include <Rcpp.h>

using namespace Rcpp;








template<int RTYPE> Vector<RTYPE> rcpp_slice_x_template(
    const Vector<RTYPE> x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  Vector<RTYPE> out(len);
  
  if(len == 1) {
    out[0] = x[start];
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      out[counter] = x[i];
      counter++;
    }
  }
  return out;
}
  


template<int RTYPE> void rcpp_slice_set_template(
    Vector<RTYPE> x, const Vector<RTYPE> rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  if(len == 1) {
    x[start] = rp[0];
  }
  else if(rp.length() == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      x[i] = rp[counter];
      counter++;
    }
  }
  else if(rp.length() == 1) {
    // Comment
    for(R_xlen_t i = start; i <= end; i += by) {
      x[i] = rp[0];
    }
  }
  else {
    stop("recycling not allowed");
  }
}


template<int RTYPE> Vector<RTYPE> rcpp_slice_xrev_template(
    const Vector<RTYPE> x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  Vector<RTYPE> out(len);
  
  if(len == 1) {
    out[0] = x[start];
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      out[counter] = x[i];
      counter++;
    }
  }
  return out;
}


template<int RTYPE> void rcpp_slice_setrev_template(
    Vector<RTYPE> x, const Vector<RTYPE> rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  if(len == 1) {
    x[start] = rp[0];
  }
  else if(rp.length() == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      x[i] = rp[counter];
      counter++;
    }
  }
  else if(rp.length() == 1) {
    // Comment
    for(R_xlen_t i = start; i >= end; i -= by) {
      x[i] = rp[0];
    }
  }
  else {
    stop("recylcing not allowed");
  }
}


template<int RTYPE> Vector<RTYPE> rcpp_slice_rm_template(
    const Vector<RTYPE> x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  Vector<RTYPE> out(len);
  
  R_xlen_t counter = 0;
  if(start > 0) {
    // Comment
    for(R_xlen_t i = 0; i < start; ++i) {
      out[i] = x[i];
    }
    counter = start;
  }
  
  if(by == 2) {
    for(R_xlen_t i = start; i < end; i+=2) {
      out[counter] = x[i+1];
      counter++;
    }
  }
  
  if(by > 2) {
    for(R_xlen_t i = start; i < end; i += by) {
      R_xlen_t startx = i + 1;
      for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
        out[counter] = x[j];
        counter++;
      }
    }
  }
  
  if(end < (x.length() - 1)) {
    // Comment
    for(R_xlen_t i = end + 1; i < x.length(); ++i) {
      out[i - (end + 1) + counter] = x[i];
    }
  }
  return out;
}



template<int RTYPE> void rcpp_slice_setinv_template(
    Vector<RTYPE> x, const Vector<RTYPE> rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  
  
  if(rp.length() == len) {
    R_xlen_t counter = 0;
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        x[i] = rp[i];
      }
      counter = start;
    }
    
    if(by == 2) {
      for(R_xlen_t i = start; i < end; i+=2) {
        x[i+1] = rp[counter];
        counter++;
      }
    }
    
    if(by > 2) {
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          x[j] = rp[counter];
          counter++;
        }
      }
    }
    
    if(end < (x.length() - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < x.length(); ++i) {
        x[i] = rp[i - (end + 1) + counter];
      }
    }
  }
  else if(rp.length() == 1) {
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        x[i] = rp[0];
      }
    }
    
    if(by == 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i+=2) {
        x[i+1] = rp[0];
      }
    }
    
    if(by > 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          x[j] = rp[0];
        }
      }
    }
    
    if(end < (x.length() - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < x.length(); ++i) {
        x[i] = rp[0];
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
    return rcpp_slice_x_template<LGLSXP>(as<LogicalVector>(x), start, end, by, len);
    break;
  }


  case INTSXP:
  {
    return rcpp_slice_x_template<INTSXP>(as<IntegerVector>(x), start, end, by, len);
    break;
  }


  case REALSXP:
  {
    return rcpp_slice_x_template<REALSXP>(as<NumericVector>(x), start, end, by, len);
    break;
  }


  case CPLXSXP:
  {
    return rcpp_slice_x_template<CPLXSXP>(as<ComplexVector>(x), start, end, by, len);
    break;
  }


  case STRSXP:
  {
    return rcpp_slice_x_template<STRSXP>(as<CharacterVector>(x), start, end, by, len);
    break;
  }


  case RAWSXP:
  {
    return rcpp_slice_x_template<RAWSXP>(as<RawVector>(x), start, end, by, len);
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
     rcpp_slice_set_template<LGLSXP>(as<LogicalVector>(x), as<LogicalVector>(rp), start, end, by, len);
    break;
  }


  case INTSXP:
  {
     rcpp_slice_set_template<INTSXP>(as<IntegerVector>(x), as<IntegerVector>(rp), start, end, by, len);
    break;
  }


  case REALSXP:
  {
     rcpp_slice_set_template<REALSXP>(as<NumericVector>(x), as<NumericVector>(rp), start, end, by, len);
    break;
  }


  case CPLXSXP:
  {
     rcpp_slice_set_template<CPLXSXP>(as<ComplexVector>(x), as<ComplexVector>(rp), start, end, by, len);
    break;
  }


  case STRSXP:
  {
     rcpp_slice_set_template<STRSXP>(as<CharacterVector>(x), as<CharacterVector>(rp), start, end, by, len);
    break;
  }


  case RAWSXP:
  {
     rcpp_slice_set_template<RAWSXP>(as<RawVector>(x), as<RawVector>(rp), start, end, by, len);
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
    return rcpp_slice_xrev_template<LGLSXP>(as<LogicalVector>(x), start, end, by, len);
    break;
  }


  case INTSXP:
  {
    return rcpp_slice_xrev_template<INTSXP>(as<IntegerVector>(x), start, end, by, len);
    break;
  }


  case REALSXP:
  {
    return rcpp_slice_xrev_template<REALSXP>(as<NumericVector>(x), start, end, by, len);
    break;
  }


  case CPLXSXP:
  {
    return rcpp_slice_xrev_template<CPLXSXP>(as<ComplexVector>(x), start, end, by, len);
    break;
  }


  case STRSXP:
  {
    return rcpp_slice_xrev_template<STRSXP>(as<CharacterVector>(x), start, end, by, len);
    break;
  }


  case RAWSXP:
  {
    return rcpp_slice_xrev_template<RAWSXP>(as<RawVector>(x), start, end, by, len);
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
     rcpp_slice_setrev_template<LGLSXP>(as<LogicalVector>(x), as<LogicalVector>(rp), start, end, by, len);
    break;
  }


  case INTSXP:
  {
     rcpp_slice_setrev_template<INTSXP>(as<IntegerVector>(x), as<IntegerVector>(rp), start, end, by, len);
    break;
  }


  case REALSXP:
  {
     rcpp_slice_setrev_template<REALSXP>(as<NumericVector>(x), as<NumericVector>(rp), start, end, by, len);
    break;
  }


  case CPLXSXP:
  {
     rcpp_slice_setrev_template<CPLXSXP>(as<ComplexVector>(x), as<ComplexVector>(rp), start, end, by, len);
    break;
  }


  case STRSXP:
  {
     rcpp_slice_setrev_template<STRSXP>(as<CharacterVector>(x), as<CharacterVector>(rp), start, end, by, len);
    break;
  }


  case RAWSXP:
  {
     rcpp_slice_setrev_template<RAWSXP>(as<RawVector>(x), as<RawVector>(rp), start, end, by, len);
    break;
  }

  default: stop("unsupported type given");
}



}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_rm_atomic)]]
SEXP rcpp_slice_rm_atomic(
  const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
) {


switch(TYPEOF(x)){

  case LGLSXP:
  {
    return rcpp_slice_rm_template<LGLSXP>(as<LogicalVector>(x), start, end, by, len);
    break;
  }


  case INTSXP:
  {
    return rcpp_slice_rm_template<INTSXP>(as<IntegerVector>(x), start, end, by, len);
    break;
  }


  case REALSXP:
  {
    return rcpp_slice_rm_template<REALSXP>(as<NumericVector>(x), start, end, by, len);
    break;
  }


  case CPLXSXP:
  {
    return rcpp_slice_rm_template<CPLXSXP>(as<ComplexVector>(x), start, end, by, len);
    break;
  }


  case STRSXP:
  {
    return rcpp_slice_rm_template<STRSXP>(as<CharacterVector>(x), start, end, by, len);
    break;
  }


  case RAWSXP:
  {
    return rcpp_slice_rm_template<RAWSXP>(as<RawVector>(x), start, end, by, len);
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
     rcpp_slice_setinv_template<LGLSXP>(as<LogicalVector>(x), as<LogicalVector>(rp), start, end, by, len);
    break;
  }


  case INTSXP:
  {
     rcpp_slice_setinv_template<INTSXP>(as<IntegerVector>(x), as<IntegerVector>(rp), start, end, by, len);
    break;
  }


  case REALSXP:
  {
     rcpp_slice_setinv_template<REALSXP>(as<NumericVector>(x), as<NumericVector>(rp), start, end, by, len);
    break;
  }


  case CPLXSXP:
  {
     rcpp_slice_setinv_template<CPLXSXP>(as<ComplexVector>(x), as<ComplexVector>(rp), start, end, by, len);
    break;
  }


  case STRSXP:
  {
     rcpp_slice_setinv_template<STRSXP>(as<CharacterVector>(x), as<CharacterVector>(rp), start, end, by, len);
    break;
  }


  case RAWSXP:
  {
     rcpp_slice_setinv_template<RAWSXP>(as<RawVector>(x), as<RawVector>(rp), start, end, by, len);
    break;
  }

  default: stop("unsupported type given");
}



}

