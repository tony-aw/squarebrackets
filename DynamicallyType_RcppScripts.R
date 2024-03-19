# set-up ====

library(stringi)

# Rcpp::cppFunction(
#   "
#   void rcpp_set_rowcol_Numeric1(NumericMatrix x, IntegerVector rowind, IntegerVector colind, NumericVector rp) {
#   int ni = rowind.length();
#   int nj = colind.length();
#   int counter = 0;
#   for(int j = 0; j < nj; ++j){
#     NumericMatrix::Column col = x(_, colind[j]);
#     for(int i = 0; i < ni; ++i) {
#       col[rowind[i]] = rp[0];
#       counter += 1;
#     }
#   }
# }
#   "
# )
# 
# x <- matrix(c(1:18, NA, NaN), ncol = 4)
# rcpp_set_rowcol_Numeric1(x, 2L, 2L, as.numeric(NaN))
# x

# set matrix scripts ====

RTYPES <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode <- list()

templatecode[["set_rowcol"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_rowcol_RTYPE)]]
void rcpp_set_rowcol_RTYPE(RTYPEMatrix x, IntegerVector rowind, IntegerVector colind, RTYPEVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    RTYPEMatrix::Column col = x(_, colind[j]);
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
// [[Rcpp::export(.rcpp_set_rowcol_RTYPE1)]]
void rcpp_set_rowcol_RTYPE1(RTYPEMatrix x, IntegerVector rowind, IntegerVector colind, RTYPEVector rp) {
  int ni = rowind.length();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    RTYPEMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

"

templatecode[["set_row"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_row_RTYPE)]]
void rcpp_set_row_RTYPE(RTYPEMatrix x, IntegerVector rowind, RTYPEVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    RTYPEMatrix::Column col = x(_, j);
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
// [[Rcpp::export(.rcpp_set_row_RTYPE1)]]
void rcpp_set_row_RTYPE1(RTYPEMatrix x, IntegerVector rowind, RTYPEVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    RTYPEMatrix::Column col = x(_, j);
    for(int i = 0; i < ni; ++i) {
      col[rowind[i]] = rp[0];
      counter += 1;
    }
  }
}

"

templatecode[["set_col"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_col_RTYPE)]]
void rcpp_set_col_RTYPE(RTYPEMatrix x, IntegerVector colind, RTYPEVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    RTYPEMatrix::Column col = x(_, colind[j]);
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
// [[Rcpp::export(.rcpp_set_col_RTYPE1)]]
void rcpp_set_col_RTYPE1(RTYPEMatrix x, IntegerVector colind, RTYPEVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  int counter = 0;
  for(int j = 0; j < nj; ++j){
    RTYPEMatrix::Column col = x(_, colind[j]);
    for(int i = 0; i < ni; ++i) {
      col[i] = rp[0];
      counter += 1;
    }
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

fileConn <- file("src/dynamic_rcpp_set_rowcol.cpp")
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



################################################################################

# setrv ====

rtypes <- c("Logical", "Integer", "Numeric", "Character", "Raw")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_safe_RTYPE)]]
void rcpp_setrv_safe_RTYPE(RTYPEVector x, RTYPEVector v, RTYPEVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!RTYPEVector::is_na(x[i])) {
        if(x[i] == v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!RTYPEVector::is_na(x[i])) {
        if(x[i] != v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_fast_RTYPE)]]
void rcpp_setrv_fast_RTYPE(RTYPEVector x, RTYPEVector v, RTYPEVector rp, bool invert) {
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



"
rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)


Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)


fileConn <- file("src/dynamic_rcpp_setrv.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)


