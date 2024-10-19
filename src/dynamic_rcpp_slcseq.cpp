

#include <Rcpp.h>

using namespace Rcpp;








//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_x_Logical)]]
LogicalVector rcpp_slcseq_x_Logical(
    const LogicalVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  LogicalVector out(len);
  
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
  

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_set_Logical)]]
void rcpp_slcseq_set_Logical(
    LogicalVector x, const LogicalVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_xrev_Logical)]]
LogicalVector rcpp_slcseq_xrev_Logical(
    const LogicalVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  LogicalVector out(len);
  
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_setrev_Logical)]]
void rcpp_range_slcseq_setrev_Logical(
    LogicalVector x, const LogicalVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_rm_Logical)]]
LogicalVector rcpp_slcseq_rm_Logical(
    const LogicalVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  LogicalVector out(len);
  
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



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_setinv_Logical)]]
void rcpp_slcseq_setinv_Logical(
    LogicalVector x, const LogicalVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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
// [[Rcpp::export(.rcpp_slcseq_x_Integer)]]
IntegerVector rcpp_slcseq_x_Integer(
    const IntegerVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  IntegerVector out(len);
  
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
  

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_set_Integer)]]
void rcpp_slcseq_set_Integer(
    IntegerVector x, const IntegerVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_xrev_Integer)]]
IntegerVector rcpp_slcseq_xrev_Integer(
    const IntegerVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  IntegerVector out(len);
  
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_setrev_Integer)]]
void rcpp_range_slcseq_setrev_Integer(
    IntegerVector x, const IntegerVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_rm_Integer)]]
IntegerVector rcpp_slcseq_rm_Integer(
    const IntegerVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  IntegerVector out(len);
  
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



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_setinv_Integer)]]
void rcpp_slcseq_setinv_Integer(
    IntegerVector x, const IntegerVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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
// [[Rcpp::export(.rcpp_slcseq_x_Numeric)]]
NumericVector rcpp_slcseq_x_Numeric(
    const NumericVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  NumericVector out(len);
  
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
  

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_set_Numeric)]]
void rcpp_slcseq_set_Numeric(
    NumericVector x, const NumericVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_xrev_Numeric)]]
NumericVector rcpp_slcseq_xrev_Numeric(
    const NumericVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  NumericVector out(len);
  
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_setrev_Numeric)]]
void rcpp_range_slcseq_setrev_Numeric(
    NumericVector x, const NumericVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_rm_Numeric)]]
NumericVector rcpp_slcseq_rm_Numeric(
    const NumericVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  NumericVector out(len);
  
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



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_setinv_Numeric)]]
void rcpp_slcseq_setinv_Numeric(
    NumericVector x, const NumericVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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
// [[Rcpp::export(.rcpp_slcseq_x_Character)]]
CharacterVector rcpp_slcseq_x_Character(
    const CharacterVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  CharacterVector out(len);
  
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
  

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_set_Character)]]
void rcpp_slcseq_set_Character(
    CharacterVector x, const CharacterVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_xrev_Character)]]
CharacterVector rcpp_slcseq_xrev_Character(
    const CharacterVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  CharacterVector out(len);
  
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_setrev_Character)]]
void rcpp_range_slcseq_setrev_Character(
    CharacterVector x, const CharacterVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_rm_Character)]]
CharacterVector rcpp_slcseq_rm_Character(
    const CharacterVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  CharacterVector out(len);
  
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



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_setinv_Character)]]
void rcpp_slcseq_setinv_Character(
    CharacterVector x, const CharacterVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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
// [[Rcpp::export(.rcpp_slcseq_x_Complex)]]
ComplexVector rcpp_slcseq_x_Complex(
    const ComplexVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  ComplexVector out(len);
  
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
  

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_set_Complex)]]
void rcpp_slcseq_set_Complex(
    ComplexVector x, const ComplexVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_xrev_Complex)]]
ComplexVector rcpp_slcseq_xrev_Complex(
    const ComplexVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  ComplexVector out(len);
  
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_setrev_Complex)]]
void rcpp_range_slcseq_setrev_Complex(
    ComplexVector x, const ComplexVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_rm_Complex)]]
ComplexVector rcpp_slcseq_rm_Complex(
    const ComplexVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  ComplexVector out(len);
  
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



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_setinv_Complex)]]
void rcpp_slcseq_setinv_Complex(
    ComplexVector x, const ComplexVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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
// [[Rcpp::export(.rcpp_slcseq_x_Raw)]]
RawVector rcpp_slcseq_x_Raw(
    const RawVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  RawVector out(len);
  
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
  

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_set_Raw)]]
void rcpp_slcseq_set_Raw(
    RawVector x, const RawVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_xrev_Raw)]]
RawVector rcpp_slcseq_xrev_Raw(
    const RawVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  RawVector out(len);
  
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_setrev_Raw)]]
void rcpp_range_slcseq_setrev_Raw(
    RawVector x, const RawVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_rm_Raw)]]
RawVector rcpp_slcseq_rm_Raw(
    const RawVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  RawVector out(len);
  
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



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slcseq_setinv_Raw)]]
void rcpp_slcseq_setinv_Raw(
    RawVector x, const RawVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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


