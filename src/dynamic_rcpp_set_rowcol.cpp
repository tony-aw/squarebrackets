

#include <Rcpp.h>

using namespace Rcpp;






  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_bool)]]
void rcpp_set_rowcol_bool(LogicalMatrix x, IntegerVector rowind, IntegerVector colind, LogicalVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    LogicalMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_bool1)]]
void rcpp_set_rowcol_bool1(LogicalMatrix x, IntegerVector rowind, IntegerVector colind, bool rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    LogicalMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp;
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_bool)]]
void rcpp_set_row_bool(LogicalMatrix x, IntegerVector rowind, LogicalVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    LogicalMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_bool1)]]
void rcpp_set_row_bool1(LogicalMatrix x, IntegerVector rowind, bool rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    LogicalMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp;
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_bool)]]
void rcpp_set_col_bool(LogicalMatrix x, IntegerVector colind, LogicalVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    LogicalMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_bool1)]]
void rcpp_set_col_bool1(LogicalMatrix x, IntegerVector colind, bool rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    LogicalMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp;
      counter += 1;
    }
  }
}





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_int)]]
void rcpp_set_rowcol_int(IntegerMatrix x, IntegerVector rowind, IntegerVector colind, IntegerVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    IntegerMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_int1)]]
void rcpp_set_rowcol_int1(IntegerMatrix x, IntegerVector rowind, IntegerVector colind, int rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    IntegerMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp;
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_int)]]
void rcpp_set_row_int(IntegerMatrix x, IntegerVector rowind, IntegerVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    IntegerMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_int1)]]
void rcpp_set_row_int1(IntegerMatrix x, IntegerVector rowind, int rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    IntegerMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp;
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_int)]]
void rcpp_set_col_int(IntegerMatrix x, IntegerVector colind, IntegerVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    IntegerMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_int1)]]
void rcpp_set_col_int1(IntegerMatrix x, IntegerVector colind, int rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    IntegerMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp;
      counter += 1;
    }
  }
}





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_double)]]
void rcpp_set_rowcol_double(NumericMatrix x, IntegerVector rowind, IntegerVector colind, NumericVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    NumericMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_double1)]]
void rcpp_set_rowcol_double1(NumericMatrix x, IntegerVector rowind, IntegerVector colind, double rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    NumericMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp;
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_double)]]
void rcpp_set_row_double(NumericMatrix x, IntegerVector rowind, NumericVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    NumericMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_double1)]]
void rcpp_set_row_double1(NumericMatrix x, IntegerVector rowind, double rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    NumericMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp;
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_double)]]
void rcpp_set_col_double(NumericMatrix x, IntegerVector colind, NumericVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    NumericMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_double1)]]
void rcpp_set_col_double1(NumericMatrix x, IntegerVector colind, double rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    NumericMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp;
      counter += 1;
    }
  }
}





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_String)]]
void rcpp_set_rowcol_String(CharacterMatrix x, IntegerVector rowind, IntegerVector colind, CharacterVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    CharacterMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_String1)]]
void rcpp_set_rowcol_String1(CharacterMatrix x, IntegerVector rowind, IntegerVector colind, String rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    CharacterMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp;
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_String)]]
void rcpp_set_row_String(CharacterMatrix x, IntegerVector rowind, CharacterVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    CharacterMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_String1)]]
void rcpp_set_row_String1(CharacterMatrix x, IntegerVector rowind, String rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    CharacterMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp;
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_String)]]
void rcpp_set_col_String(CharacterMatrix x, IntegerVector colind, CharacterVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    CharacterMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_String1)]]
void rcpp_set_col_String1(CharacterMatrix x, IntegerVector colind, String rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    CharacterMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp;
      counter += 1;
    }
  }
}





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_Rcomplex)]]
void rcpp_set_rowcol_Rcomplex(ComplexMatrix x, IntegerVector rowind, IntegerVector colind, ComplexVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    ComplexMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_Rcomplex1)]]
void rcpp_set_rowcol_Rcomplex1(ComplexMatrix x, IntegerVector rowind, IntegerVector colind, Rcomplex rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    ComplexMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp;
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_Rcomplex)]]
void rcpp_set_row_Rcomplex(ComplexMatrix x, IntegerVector rowind, ComplexVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    ComplexMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_Rcomplex1)]]
void rcpp_set_row_Rcomplex1(ComplexMatrix x, IntegerVector rowind, Rcomplex rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    ComplexMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp;
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_Rcomplex)]]
void rcpp_set_col_Rcomplex(ComplexMatrix x, IntegerVector colind, ComplexVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    ComplexMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_Rcomplex1)]]
void rcpp_set_col_Rcomplex1(ComplexMatrix x, IntegerVector colind, Rcomplex rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    ComplexMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp;
      counter += 1;
    }
  }
}


