# set-up ====

library(stringi)


# set all ====

RTYPES <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_all_RTYPE)]]
void rcpp_set_all_RTYPE(RTYPEVector x, RTYPEVector rp) {
  R_xlen_t n = x.length();

  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[i] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      x[i] = rp[0];
    }
  }
  else stop(\"recycling not allowed\");

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

fileConn <- file("src/dynamic_rcpp_set_all.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)


################################################################################

# set vind ====

Rcpp::cppFunction(
  "
  bool rcpp_check_len(
    NumericVector rp
  ) {
    R_xlen_t n_rp = rp.length();
    bool out = n_rp == 1;
    return out;
  }
  "
)

rcpp_check_len(rp)


RTYPES <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_64_RTYPE)]]
void rcpp_set_vind_64_RTYPE(RTYPEVector x, const NumericVector ind, const RTYPEVector rp) {
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
  else stop(\"recycling not allowed\");
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_32_RTYPE)]]
void rcpp_set_vind_32_RTYPE(RTYPEVector x, const IntegerVector ind, const RTYPEVector rp) {
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
  else stop(\"recycling not allowed\");
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

fileConn <- file("src/dynamic_rcpp_set_vind.cpp")
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

# slice ====


RTYPES <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_x)]]
NumericVector rcpp_slice_x(
    const NumericVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  R_xlen_t counter = 0;
  NumericVector out(len);
  for(R_xlen_t i = start; i <= end; i += by) {
    out[counter] = x[i];
    counter++;
  }
  return out;
}
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_revx)]]
NumericVector rcpp_slice_revx(
    const NumericVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  R_xlen_t counter = 0;
  NumericVector out(len);
  for(R_xlen_t i = start; i >= end; i -= by) {
    out[counter] = x[i];
    counter++;
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_set)]]
void rcpp_slice_set(
    NumericVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len, const NumericVector rp
  ) {
  
  if(rp.length() == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      x[i] = rp[counter];
      counter++;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = start; i <= end; i += by) {
      x[i] = rp[0];
    }
  }
  else {
    stop(\"recycling not allowed\");
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_revset)]]
void rcpp_range_slice_revset(
    NumericVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len, const NumericVector rp
  ) {

  if(rp.length() == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      x[i] = rp[counter];
      counter++;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = start; i >= end; i -= by) {
      x[i] = rp[0];
    }
  }
  else {
    stop(\"recylcing not allowed\");
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

fileConn <- file("src/dynamic_rcpp_slice.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)

