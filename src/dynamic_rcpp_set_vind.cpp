

#include <Rcpp.h>

using namespace Rcpp;









//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_64_Logical)]]
void rcpp_set_vind_64_Logical(LogicalVector x, const NumericVector ind, const LogicalVector rp) {
  R_xlen_t n = ind.length();
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_32_Logical)]]
void rcpp_set_vind_32_Logical(LogicalVector x, const IntegerVector ind, const LogicalVector rp) {
  R_xlen_t n = ind.length();
  
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_64_Integer)]]
void rcpp_set_vind_64_Integer(IntegerVector x, const NumericVector ind, const IntegerVector rp) {
  R_xlen_t n = ind.length();
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_32_Integer)]]
void rcpp_set_vind_32_Integer(IntegerVector x, const IntegerVector ind, const IntegerVector rp) {
  R_xlen_t n = ind.length();
  
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_64_Numeric)]]
void rcpp_set_vind_64_Numeric(NumericVector x, const NumericVector ind, const NumericVector rp) {
  R_xlen_t n = ind.length();
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_32_Numeric)]]
void rcpp_set_vind_32_Numeric(NumericVector x, const IntegerVector ind, const NumericVector rp) {
  R_xlen_t n = ind.length();
  
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_64_Character)]]
void rcpp_set_vind_64_Character(CharacterVector x, const NumericVector ind, const CharacterVector rp) {
  R_xlen_t n = ind.length();
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_32_Character)]]
void rcpp_set_vind_32_Character(CharacterVector x, const IntegerVector ind, const CharacterVector rp) {
  R_xlen_t n = ind.length();
  
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_64_Complex)]]
void rcpp_set_vind_64_Complex(ComplexVector x, const NumericVector ind, const ComplexVector rp) {
  R_xlen_t n = ind.length();
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_32_Complex)]]
void rcpp_set_vind_32_Complex(ComplexVector x, const IntegerVector ind, const ComplexVector rp) {
  R_xlen_t n = ind.length();
  
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_64_Raw)]]
void rcpp_set_vind_64_Raw(RawVector x, const NumericVector ind, const RawVector rp) {
  R_xlen_t n = ind.length();
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_32_Raw)]]
void rcpp_set_vind_32_Raw(RawVector x, const IntegerVector ind, const RawVector rp) {
  R_xlen_t n = ind.length();
  
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop("recycling not allowed");
}


