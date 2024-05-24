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

# set 3d ====

RTYPES <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_3d_RTYPE)]]
void rcpp_set_3d_RTYPE(
  RTYPEVector x,
  IntegerVector ind1, IntegerVector ind2, IntegerVector ind3,
  IntegerVector dimcumprod,
  RTYPEVector rp
) {
  int ni = ind1.length();
  int nj = ind2.length();
  int nk = ind3.length();
  int flatind = 0;
  
  if(rp.length() == 1) {
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[0];
        }
      }
    }
  }
  else {
    int counter = 0;
    for(int k = 0; k < nk; ++k){
      for(int j = 0; j < nj; ++j) {
        for(int i = 0; i < ni; ++i) {
          flatind = ind1[i] + dimcumprod[0]  * (ind2[j] - 1) + dimcumprod[1] * (ind3[k] - 1);
          x[flatind - 1] = rp[counter];
          counter++;
        }
      }
    }
  }
}

"

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

fileConn <- file("src/dynamic_rcpp_set_3d.cpp")
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



################################################################################

# seq_rec ====


inops_nms <- c("plus", "min", "x", "div")
inops_sym <- c("+", "-", "*", "/")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_seq_rec2_<INOP>)]]
NumericVector rcpp_seq_rec2_<INOP>(
  NumericVector inits, int n, NumericVector s, NumericVector m, int form, bool rev
) {

  if(inits.length() != 2 || s.length() != 2 || m.length() != 2) {
    stop(\"`inits`, `s`, `m` must each be of length 2\");
  }
  
  NumericVector x(n);
  Range idx(0, 1);
  x[idx] = inits;
  
  int prev1;
  int prev2;
  
  if(!rev) {
    prev1 = 1;
    prev2 = 2;
  }
  if(rev) {
    prev1 = 2;
    prev2 = 1;
  }
  
  if(form == 1) {
    for (int i = 2; i < n; i++){
      x[i] = (s[0] + m[0] * x[i-prev1]) %INOP% (s[1] + m[1] * x[i-prev2]);
    }
  }
  if(form == 2) {
    for (int i = 2; i < n; i++){
       x[i] =(m[0] * (x[i-prev1] + s[0])) %INOP% (m[1] * (x[i-prev2] + s[1]));
    }
  }
  
  return x;
}

"

rcpp_scripts <- character(length(inops_sym))

for(i in seq_along(inops_sym)) {
  rcpp_scripts[i] <- stri_replace_all(
    templatecode,
    fixed = c("%INOP%", "<INOP>"),
    replacement = c(inops_sym[i], inops_nms[i]),
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



fileConn <- file("src/dynamic_rcpp_seq_rec2.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)
