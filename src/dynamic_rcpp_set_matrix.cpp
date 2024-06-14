

#include <Rcpp.h>

using namespace Rcpp;








  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_rowcol_Logical)]]
void rcpp_set_matrix_rowcol_Logical(
  LogicalMatrix x, IntegerVector rowind, IntegerVector colind, LogicalVector rp
) {
  int ni = rowind.length();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      LogicalMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      LogicalMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_row_Logical)]]
void rcpp_set_matrix_row_Logical(LogicalMatrix x, IntegerVector rowind, LogicalVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j) {
      LogicalMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      LogicalMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_col_Logical)]]
void rcpp_set_matrix_col_Logical(LogicalMatrix x, IntegerVector colind, LogicalVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      LogicalMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      LogicalMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_Logical)]]
void rcpp_set_matrix_Logical(
  LogicalMatrix x, IntegerVector rowind, IntegerVector colind, LogicalVector rp
) {
  if(colind.length() == 1 && colind[0] == -1) {
    rcpp_set_matrix_row_Logical(x, rowind, rp);
  }
  else if(rowind.length() == 1 && rowind[0] == -1) {
    rcpp_set_matrix_col_Logical(x, colind, rp);
  }
  else {
    rcpp_set_matrix_rowcol_Logical(x, rowind, colind, rp);
  }
}






  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_rowcol_Integer)]]
void rcpp_set_matrix_rowcol_Integer(
  IntegerMatrix x, IntegerVector rowind, IntegerVector colind, IntegerVector rp
) {
  int ni = rowind.length();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      IntegerMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      IntegerMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_row_Integer)]]
void rcpp_set_matrix_row_Integer(IntegerMatrix x, IntegerVector rowind, IntegerVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j) {
      IntegerMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      IntegerMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_col_Integer)]]
void rcpp_set_matrix_col_Integer(IntegerMatrix x, IntegerVector colind, IntegerVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      IntegerMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      IntegerMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_Integer)]]
void rcpp_set_matrix_Integer(
  IntegerMatrix x, IntegerVector rowind, IntegerVector colind, IntegerVector rp
) {
  if(colind.length() == 1 && colind[0] == -1) {
    rcpp_set_matrix_row_Integer(x, rowind, rp);
  }
  else if(rowind.length() == 1 && rowind[0] == -1) {
    rcpp_set_matrix_col_Integer(x, colind, rp);
  }
  else {
    rcpp_set_matrix_rowcol_Integer(x, rowind, colind, rp);
  }
}






  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_rowcol_Numeric)]]
void rcpp_set_matrix_rowcol_Numeric(
  NumericMatrix x, IntegerVector rowind, IntegerVector colind, NumericVector rp
) {
  int ni = rowind.length();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      NumericMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      NumericMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_row_Numeric)]]
void rcpp_set_matrix_row_Numeric(NumericMatrix x, IntegerVector rowind, NumericVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j) {
      NumericMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      NumericMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_col_Numeric)]]
void rcpp_set_matrix_col_Numeric(NumericMatrix x, IntegerVector colind, NumericVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      NumericMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      NumericMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_Numeric)]]
void rcpp_set_matrix_Numeric(
  NumericMatrix x, IntegerVector rowind, IntegerVector colind, NumericVector rp
) {
  if(colind.length() == 1 && colind[0] == -1) {
    rcpp_set_matrix_row_Numeric(x, rowind, rp);
  }
  else if(rowind.length() == 1 && rowind[0] == -1) {
    rcpp_set_matrix_col_Numeric(x, colind, rp);
  }
  else {
    rcpp_set_matrix_rowcol_Numeric(x, rowind, colind, rp);
  }
}






  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_rowcol_Character)]]
void rcpp_set_matrix_rowcol_Character(
  CharacterMatrix x, IntegerVector rowind, IntegerVector colind, CharacterVector rp
) {
  int ni = rowind.length();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      CharacterMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      CharacterMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_row_Character)]]
void rcpp_set_matrix_row_Character(CharacterMatrix x, IntegerVector rowind, CharacterVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j) {
      CharacterMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      CharacterMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_col_Character)]]
void rcpp_set_matrix_col_Character(CharacterMatrix x, IntegerVector colind, CharacterVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      CharacterMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      CharacterMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_Character)]]
void rcpp_set_matrix_Character(
  CharacterMatrix x, IntegerVector rowind, IntegerVector colind, CharacterVector rp
) {
  if(colind.length() == 1 && colind[0] == -1) {
    rcpp_set_matrix_row_Character(x, rowind, rp);
  }
  else if(rowind.length() == 1 && rowind[0] == -1) {
    rcpp_set_matrix_col_Character(x, colind, rp);
  }
  else {
    rcpp_set_matrix_rowcol_Character(x, rowind, colind, rp);
  }
}






  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_rowcol_Complex)]]
void rcpp_set_matrix_rowcol_Complex(
  ComplexMatrix x, IntegerVector rowind, IntegerVector colind, ComplexVector rp
) {
  int ni = rowind.length();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      ComplexMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      ComplexMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_row_Complex)]]
void rcpp_set_matrix_row_Complex(ComplexMatrix x, IntegerVector rowind, ComplexVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j) {
      ComplexMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      ComplexMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_col_Complex)]]
void rcpp_set_matrix_col_Complex(ComplexMatrix x, IntegerVector colind, ComplexVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      ComplexMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      ComplexMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_Complex)]]
void rcpp_set_matrix_Complex(
  ComplexMatrix x, IntegerVector rowind, IntegerVector colind, ComplexVector rp
) {
  if(colind.length() == 1 && colind[0] == -1) {
    rcpp_set_matrix_row_Complex(x, rowind, rp);
  }
  else if(rowind.length() == 1 && rowind[0] == -1) {
    rcpp_set_matrix_col_Complex(x, colind, rp);
  }
  else {
    rcpp_set_matrix_rowcol_Complex(x, rowind, colind, rp);
  }
}






  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_rowcol_Raw)]]
void rcpp_set_matrix_rowcol_Raw(
  RawMatrix x, IntegerVector rowind, IntegerVector colind, RawVector rp
) {
  int ni = rowind.length();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      RawMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      RawMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_row_Raw)]]
void rcpp_set_matrix_row_Raw(RawMatrix x, IntegerVector rowind, RawVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j) {
      RawMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      RawMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_col_Raw)]]
void rcpp_set_matrix_col_Raw(RawMatrix x, IntegerVector colind, RawVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      RawMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      RawMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[0];
      }
    }
  }
  else stop("recycling not allowed");
  
}

 

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_Raw)]]
void rcpp_set_matrix_Raw(
  RawMatrix x, IntegerVector rowind, IntegerVector colind, RawVector rp
) {
  if(colind.length() == 1 && colind[0] == -1) {
    rcpp_set_matrix_row_Raw(x, rowind, rp);
  }
  else if(rowind.length() == 1 && rowind[0] == -1) {
    rcpp_set_matrix_col_Raw(x, colind, rp);
  }
  else {
    rcpp_set_matrix_rowcol_Raw(x, rowind, colind, rp);
  }
}



