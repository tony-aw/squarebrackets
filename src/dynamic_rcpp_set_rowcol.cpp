

#include <Rcpp.h>

using namespace Rcpp;






  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_Logical)]]
void rcpp_set_rowcol_Logical(LogicalMatrix x, IntegerVector rowind, IntegerVector colind, LogicalVector rp) {
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
// [[Rcpp::export(.rcpp_set_rowcol_Logical1)]]
void rcpp_set_rowcol_Logical1(LogicalMatrix x, IntegerVector rowind, IntegerVector colind, LogicalVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    LogicalMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_Logical)]]
void rcpp_set_row_Logical(LogicalMatrix x, IntegerVector rowind, LogicalVector rp) {
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
// [[Rcpp::export(.rcpp_set_row_Logical1)]]
void rcpp_set_row_Logical1(LogicalMatrix x, IntegerVector rowind, LogicalVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    LogicalMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_Logical)]]
void rcpp_set_col_Logical(LogicalMatrix x, IntegerVector colind, LogicalVector rp) {
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
// [[Rcpp::export(.rcpp_set_col_Logical1)]]
void rcpp_set_col_Logical1(LogicalMatrix x, IntegerVector colind, LogicalVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    LogicalMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[0];
      counter += 1;
    }
  }
}





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_Integer)]]
void rcpp_set_rowcol_Integer(IntegerMatrix x, IntegerVector rowind, IntegerVector colind, IntegerVector rp) {
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
// [[Rcpp::export(.rcpp_set_rowcol_Integer1)]]
void rcpp_set_rowcol_Integer1(IntegerMatrix x, IntegerVector rowind, IntegerVector colind, IntegerVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    IntegerMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_Integer)]]
void rcpp_set_row_Integer(IntegerMatrix x, IntegerVector rowind, IntegerVector rp) {
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
// [[Rcpp::export(.rcpp_set_row_Integer1)]]
void rcpp_set_row_Integer1(IntegerMatrix x, IntegerVector rowind, IntegerVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    IntegerMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_Integer)]]
void rcpp_set_col_Integer(IntegerMatrix x, IntegerVector colind, IntegerVector rp) {
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
// [[Rcpp::export(.rcpp_set_col_Integer1)]]
void rcpp_set_col_Integer1(IntegerMatrix x, IntegerVector colind, IntegerVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    IntegerMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[0];
      counter += 1;
    }
  }
}





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_Numeric)]]
void rcpp_set_rowcol_Numeric(NumericMatrix x, IntegerVector rowind, IntegerVector colind, NumericVector rp) {
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
// [[Rcpp::export(.rcpp_set_rowcol_Numeric1)]]
void rcpp_set_rowcol_Numeric1(NumericMatrix x, IntegerVector rowind, IntegerVector colind, NumericVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    NumericMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_Numeric)]]
void rcpp_set_row_Numeric(NumericMatrix x, IntegerVector rowind, NumericVector rp) {
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
// [[Rcpp::export(.rcpp_set_row_Numeric1)]]
void rcpp_set_row_Numeric1(NumericMatrix x, IntegerVector rowind, NumericVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    NumericMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_Numeric)]]
void rcpp_set_col_Numeric(NumericMatrix x, IntegerVector colind, NumericVector rp) {
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
// [[Rcpp::export(.rcpp_set_col_Numeric1)]]
void rcpp_set_col_Numeric1(NumericMatrix x, IntegerVector colind, NumericVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    NumericMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[0];
      counter += 1;
    }
  }
}





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_Character)]]
void rcpp_set_rowcol_Character(CharacterMatrix x, IntegerVector rowind, IntegerVector colind, CharacterVector rp) {
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
// [[Rcpp::export(.rcpp_set_rowcol_Character1)]]
void rcpp_set_rowcol_Character1(CharacterMatrix x, IntegerVector rowind, IntegerVector colind, CharacterVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    CharacterMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_Character)]]
void rcpp_set_row_Character(CharacterMatrix x, IntegerVector rowind, CharacterVector rp) {
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
// [[Rcpp::export(.rcpp_set_row_Character1)]]
void rcpp_set_row_Character1(CharacterMatrix x, IntegerVector rowind, CharacterVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    CharacterMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_Character)]]
void rcpp_set_col_Character(CharacterMatrix x, IntegerVector colind, CharacterVector rp) {
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
// [[Rcpp::export(.rcpp_set_col_Character1)]]
void rcpp_set_col_Character1(CharacterMatrix x, IntegerVector colind, CharacterVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    CharacterMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[0];
      counter += 1;
    }
  }
}





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_Complex)]]
void rcpp_set_rowcol_Complex(ComplexMatrix x, IntegerVector rowind, IntegerVector colind, ComplexVector rp) {
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
// [[Rcpp::export(.rcpp_set_rowcol_Complex1)]]
void rcpp_set_rowcol_Complex1(ComplexMatrix x, IntegerVector rowind, IntegerVector colind, ComplexVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    ComplexMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_Complex)]]
void rcpp_set_row_Complex(ComplexMatrix x, IntegerVector rowind, ComplexVector rp) {
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
// [[Rcpp::export(.rcpp_set_row_Complex1)]]
void rcpp_set_row_Complex1(ComplexMatrix x, IntegerVector rowind, ComplexVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    ComplexMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_Complex)]]
void rcpp_set_col_Complex(ComplexMatrix x, IntegerVector colind, ComplexVector rp) {
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
// [[Rcpp::export(.rcpp_set_col_Complex1)]]
void rcpp_set_col_Complex1(ComplexMatrix x, IntegerVector colind, ComplexVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    ComplexMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[0];
      counter += 1;
    }
  }
}





  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_Raw)]]
void rcpp_set_rowcol_Raw(RawMatrix x, IntegerVector rowind, IntegerVector colind, RawVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    RawMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_Raw1)]]
void rcpp_set_rowcol_Raw1(RawMatrix x, IntegerVector rowind, IntegerVector colind, RawVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    RawMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_Raw)]]
void rcpp_set_row_Raw(RawMatrix x, IntegerVector rowind, RawVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    RawMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_Raw1)]]
void rcpp_set_row_Raw1(RawMatrix x, IntegerVector rowind, RawVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    RawMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_Raw)]]
void rcpp_set_col_Raw(RawMatrix x, IntegerVector colind, RawVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    RawMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[counter];
      counter += 1;
    }
  }
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_Raw1)]]
void rcpp_set_col_Raw1(RawMatrix x, IntegerVector colind, RawVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    RawMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[0];
      counter += 1;
    }
  }
}


