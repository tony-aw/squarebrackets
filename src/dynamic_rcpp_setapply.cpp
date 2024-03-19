

#include <Rcpp.h>

using namespace Rcpp;







//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_row_Logical)]]
void rcpp_setapply_col_Logical(LogicalMatrix x, Function f) {
  int nvec = x.nrow();
  int lenvec = x.ncol();
  LogicalVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    LogicalMatrix::Row row = x(j, _);
    subset1 = f(x(j, _));
    row = subset1;
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_col_Logical)]]
void rcpp_setapply_row_Logical(LogicalMatrix x, Function f) {
  int nvec = x.ncol();
  int lenvec = x.nrow();
  LogicalVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    LogicalMatrix::Column col = x(_, j);
    subset1 = f(x(_, j));
    col = subset1;
  }
}





//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_row_Integer)]]
void rcpp_setapply_col_Integer(IntegerMatrix x, Function f) {
  int nvec = x.nrow();
  int lenvec = x.ncol();
  IntegerVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    IntegerMatrix::Row row = x(j, _);
    subset1 = f(x(j, _));
    row = subset1;
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_col_Integer)]]
void rcpp_setapply_row_Integer(IntegerMatrix x, Function f) {
  int nvec = x.ncol();
  int lenvec = x.nrow();
  IntegerVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    IntegerMatrix::Column col = x(_, j);
    subset1 = f(x(_, j));
    col = subset1;
  }
}





//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_row_Numeric)]]
void rcpp_setapply_col_Numeric(NumericMatrix x, Function f) {
  int nvec = x.nrow();
  int lenvec = x.ncol();
  NumericVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    NumericMatrix::Row row = x(j, _);
    subset1 = f(x(j, _));
    row = subset1;
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_col_Numeric)]]
void rcpp_setapply_row_Numeric(NumericMatrix x, Function f) {
  int nvec = x.ncol();
  int lenvec = x.nrow();
  NumericVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    NumericMatrix::Column col = x(_, j);
    subset1 = f(x(_, j));
    col = subset1;
  }
}





//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_row_Character)]]
void rcpp_setapply_col_Character(CharacterMatrix x, Function f) {
  int nvec = x.nrow();
  int lenvec = x.ncol();
  CharacterVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    CharacterMatrix::Row row = x(j, _);
    subset1 = f(x(j, _));
    row = subset1;
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_col_Character)]]
void rcpp_setapply_row_Character(CharacterMatrix x, Function f) {
  int nvec = x.ncol();
  int lenvec = x.nrow();
  CharacterVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    CharacterMatrix::Column col = x(_, j);
    subset1 = f(x(_, j));
    col = subset1;
  }
}





//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_row_Complex)]]
void rcpp_setapply_col_Complex(ComplexMatrix x, Function f) {
  int nvec = x.nrow();
  int lenvec = x.ncol();
  ComplexVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    ComplexMatrix::Row row = x(j, _);
    subset1 = f(x(j, _));
    row = subset1;
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_col_Complex)]]
void rcpp_setapply_row_Complex(ComplexMatrix x, Function f) {
  int nvec = x.ncol();
  int lenvec = x.nrow();
  ComplexVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    ComplexMatrix::Column col = x(_, j);
    subset1 = f(x(_, j));
    col = subset1;
  }
}





//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_row_Raw)]]
void rcpp_setapply_col_Raw(RawMatrix x, Function f) {
  int nvec = x.nrow();
  int lenvec = x.ncol();
  RawVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    RawMatrix::Row row = x(j, _);
    subset1 = f(x(j, _));
    row = subset1;
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_col_Raw)]]
void rcpp_setapply_row_Raw(RawMatrix x, Function f) {
  int nvec = x.ncol();
  int lenvec = x.nrow();
  RawVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    RawMatrix::Column col = x(_, j);
    subset1 = f(x(_, j));
    col = subset1;
  }
}

