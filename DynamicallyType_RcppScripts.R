# set-up ====

library(stringi)


# set matrix scripts ====

vectortypenames <- c("Logical", "Integer", "Numeric", "Character", "Complex")
scalartypenames <- c("bool", "int", "double", "String", "Rcomplex")

templatecode <- list()

templatecode[["set_rowcol"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_SCALARTYPE)]]
void rcpp_set_rowcol_SCALARTYPE(VECTORTYPEMatrix x, IntegerVector rowind, IntegerVector colind, VECTORTYPEVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    VECTORTYPEMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

"


templatecode[["set_rowcol1"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_SCALARTYPE1)]]
void rcpp_set_rowcol_SCALARTYPE1(VECTORTYPEMatrix x, IntegerVector rowind, IntegerVector colind, SCALARTYPE rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    VECTORTYPEMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp;
      counter += 1;
    }
  }
}

"

templatecode[["set_row"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_SCALARTYPE)]]
void rcpp_set_row_SCALARTYPE(VECTORTYPEMatrix x, IntegerVector rowind, VECTORTYPEVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    VECTORTYPEMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[counter];
      counter += 1;
    }
  }
}

"

templatecode[["set_row1"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_SCALARTYPE1)]]
void rcpp_set_row_SCALARTYPE1(VECTORTYPEMatrix x, IntegerVector rowind, SCALARTYPE rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    VECTORTYPEMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp;
      counter += 1;
    }
  }
}

"

templatecode[["set_col"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_SCALARTYPE)]]
void rcpp_set_col_SCALARTYPE(VECTORTYPEMatrix x, IntegerVector colind, VECTORTYPEVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    VECTORTYPEMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[counter];
      counter += 1;
    }
  }
}

"

templatecode[["set_col1"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_SCALARTYPE1)]]
void rcpp_set_col_SCALARTYPE1(VECTORTYPEMatrix x, IntegerVector colind, SCALARTYPE rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    VECTORTYPEMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp;
      counter += 1;
    }
  }
}

"

templatecode <- do.call(paste, templatecode)

rcpp_scripts <- character(length(vectortypenames))
names(rcpp_scripts) <- scalartypenames
for(i in seq_along(vectortypenames)) {
  rcpp_scripts[[i]] <- stri_replace_all(
    templatecode,
    fixed = c("VECTORTYPE", "SCALARTYPE"),
    replacement = c(vectortypenames[i], scalartypenames[i]),
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
}


headers <- "

#include <Rcpp.h>

using namespace Rcpp;


"
rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)

fileConn <- file("src/dynamic_rcpp_set_rowcol.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)



Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)

################################################################################

# setapply ====

rtypes <- c("Logical", "Integer", "Numeric", "Character", "Complex")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_row_RTYPE)]]
void rcpp_setapply_col_RTYPE(RTYPEMatrix x, Function f) {
  int nvec = x.nrow();
  int lenvec = x.ncol();
  RTYPEVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    RTYPEMatrix::Row row = x(j, _);
    subset1 = f(x(j, _));
    row = subset1;
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_col_RTYPE)]]
void rcpp_setapply_row_RTYPE(RTYPEMatrix x, Function f) {
  int nvec = x.ncol();
  int lenvec = x.nrow();
  RTYPEVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    RTYPEMatrix::Column col = x(_, j);
    subset1 = f(x(_, j));
    col = subset1;
  }
}
"

rcpp_scripts <- character(length(rtypes))

for(i in seq_along(rtypes)) {
  rcpp_scripts[i] <- stri_replace_all(
    templatecode,
    fixed = "RTYPE",
    replacement = rtypes[i],
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
}


headers <- "

#include <Rcpp.h>

using namespace Rcpp;


"
rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)


Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)


fileConn <- file("src/dynamic_rcpp_setapply.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)



