

#include <Rcpp.h>

using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_safe_Complex)]]
void rcpp_setrv_safe_Complex(ComplexVector x, ComplexVector v, ComplexVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!ComplexVector::is_na(x[i])) {
        if(x[i] == v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!ComplexVector::is_na(x[i])) {
        if(!(x[i] == v[0])) {
          x[i] = rp[0];
        }
      }
    }
  }
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_fast_Complex)]]
void rcpp_setrv_fast_Complex(ComplexVector x, ComplexVector v, ComplexVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] == v[0]) {
        x[i] = rp[0];
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!(x[i] == v[0])) {
          x[i] = rp[0];
      }
    }
  }
  
}








//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_safe_Logical)]]
void rcpp_setrv_safe_Logical(LogicalVector x, LogicalVector v, LogicalVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!LogicalVector::is_na(x[i])) {
        if(x[i] == v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!LogicalVector::is_na(x[i])) {
        if(x[i] != v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_fast_Logical)]]
void rcpp_setrv_fast_Logical(LogicalVector x, LogicalVector v, LogicalVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] == v[0]) {
        x[i] = rp[0];
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] != v[0]) {
          x[i] = rp[0];
      }
    }
  }
  
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_safe_Integer)]]
void rcpp_setrv_safe_Integer(IntegerVector x, IntegerVector v, IntegerVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!IntegerVector::is_na(x[i])) {
        if(x[i] == v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!IntegerVector::is_na(x[i])) {
        if(x[i] != v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_fast_Integer)]]
void rcpp_setrv_fast_Integer(IntegerVector x, IntegerVector v, IntegerVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] == v[0]) {
        x[i] = rp[0];
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] != v[0]) {
          x[i] = rp[0];
      }
    }
  }
  
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_safe_Numeric)]]
void rcpp_setrv_safe_Numeric(NumericVector x, NumericVector v, NumericVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!NumericVector::is_na(x[i])) {
        if(x[i] == v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!NumericVector::is_na(x[i])) {
        if(x[i] != v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_fast_Numeric)]]
void rcpp_setrv_fast_Numeric(NumericVector x, NumericVector v, NumericVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] == v[0]) {
        x[i] = rp[0];
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] != v[0]) {
          x[i] = rp[0];
      }
    }
  }
  
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_safe_Character)]]
void rcpp_setrv_safe_Character(CharacterVector x, CharacterVector v, CharacterVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!CharacterVector::is_na(x[i])) {
        if(x[i] == v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!CharacterVector::is_na(x[i])) {
        if(x[i] != v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_fast_Character)]]
void rcpp_setrv_fast_Character(CharacterVector x, CharacterVector v, CharacterVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] == v[0]) {
        x[i] = rp[0];
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] != v[0]) {
          x[i] = rp[0];
      }
    }
  }
  
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_safe_Raw)]]
void rcpp_setrv_safe_Raw(RawVector x, RawVector v, RawVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!RawVector::is_na(x[i])) {
        if(x[i] == v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!RawVector::is_na(x[i])) {
        if(x[i] != v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_fast_Raw)]]
void rcpp_setrv_fast_Raw(RawVector x, RawVector v, RawVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] == v[0]) {
        x[i] = rp[0];
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] != v[0]) {
          x[i] = rp[0];
      }
    }
  }
  
}


