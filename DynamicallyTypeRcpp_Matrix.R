# set-up ====

library(stringi)


################################################################################



# set matrix scripts ====

RTYPES <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode <- list()


templatecode[["set_matrix_rowcol"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_rowcol_RTYPE)]]
void rcpp_set_matrix_rowcol_RTYPE(
  RTYPEMatrix x, IntegerVector rowind, IntegerVector colind, RTYPEVector rp
) {
  int ni = rowind.length();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      RTYPEMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      RTYPEMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[0];
      }
    }
  }
  else stop(\"recycling not allowed\");
  
}

"


templatecode[["set_matrix_row"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_row_RTYPE)]]
void rcpp_set_matrix_row_RTYPE(RTYPEMatrix x, IntegerVector rowind, RTYPEVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j) {
      RTYPEMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      RTYPEMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[0];
      }
    }
  }
  else stop(\"recycling not allowed\");
  
}

"

templatecode[["set_matrix_col"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_col_RTYPE)]]
void rcpp_set_matrix_col_RTYPE(RTYPEMatrix x, IntegerVector colind, RTYPEVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      RTYPEMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      RTYPEMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[0];
      }
    }
  }
  else stop(\"recycling not allowed\");
  
}

"



templatecode[["set_matrix"]] <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_RTYPE)]]
void rcpp_set_matrix_RTYPE(
  RTYPEMatrix x, IntegerVector rowind, IntegerVector colind, RTYPEVector rp
) {
  if(colind.length() == 1 && colind[0] == -1) {
    rcpp_set_matrix_row_RTYPE(x, rowind, rp);
  }
  else if(rowind.length() == 1 && rowind[0] == -1) {
    rcpp_set_matrix_col_RTYPE(x, colind, rp);
  }
  else {
    rcpp_set_matrix_rowcol_RTYPE(x, rowind, colind, rp);
  }
}


"


templatecode <- do.call(paste, templatecode)

rcpp_scripts <- character(length(RTYPES))
names(rcpp_scripts) <- RTYPES
for(i in seq_along(RTYPES)) {
  rcpp_scripts[[i]] <- stri_replace_all(
    templatecode,
    fixed = c("RTYPE"),
    replacement = c(RTYPES[i]),
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

fileConn <- file("src/dynamic_rcpp_set_matrix.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)



################################################################################

# setapply ====

rtypes <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

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

