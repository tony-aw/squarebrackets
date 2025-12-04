
# set-up ====

source("source.R")

library(stringi)


header_for_source <- "

#include <Rcpp.h>

using namespace Rcpp;

#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \\
  POINTER[INDEX] = REPLACEMENT; \\
} while(0)

"


header_for_package <- "

#include <Rcpp.h>

#include \"squarebrackets.h\"

using namespace Rcpp;


"



################################################################################
# template code ====

templatecode <- "

inline SEXP rcpp_slice_x_<RCPP_TYPE>(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(<SXP_TYPE>, len));
  <COMMENT> <scalar_type> *pout = <FUN_TYPE>(out);
  const <scalar_type> *px = <FUN_TYPE>_RO(x);
  
  if(len == 1) {
    <SET_FUN>out, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      <SET_FUN>out, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}
  


inline void rcpp_slice_set_<RCPP_TYPE>(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  <COMMENT> <scalar_type> *px = <FUN_TYPE>(x);
  const <scalar_type> *prp = <FUN_TYPE>_RO(rp);
  
  if(len == 1) {
    <SET_FUN>x, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      <SET_FUN>x, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i <= end; i += by) {
      <SET_FUN>x, i, prp[0]);
    }
  }
  else {
    stop(\"recycling not allowed\");
  }
}


inline SEXP rcpp_slice_xrev_<RCPP_TYPE>(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(<SXP_TYPE>, len));
  <COMMENT> <scalar_type> *pout = <FUN_TYPE>(out);
  const <scalar_type> *px = <FUN_TYPE>_RO(x);
  
  if(len == 1) {
    <SET_FUN>out, 0, px[start]);
    UNPROTECT(1);
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      <SET_FUN>out, counter, px[i]);
      counter++;
    }
  }
  
  UNPROTECT(1);
  return out;
}


inline void rcpp_slice_setrev_<RCPP_TYPE>(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  <COMMENT> <scalar_type> *px = <FUN_TYPE>(x);
  const <scalar_type> *prp = <FUN_TYPE>_RO(rp);
  
  if(len == 1) {
    <SET_FUN>x, start, prp[0]);
  }
  else if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      <SET_FUN>x, i, prp[counter]);
      counter++;
    }
  }
  else if(Rf_xlength(rp) == 1) {
    // Comment
    for(R_xlen_t i = start; i >= end; i -= by) {
      <SET_FUN>x, i, prp[0]);
    }
  }
  else {
    stop(\"recylcing not allowed\");
  }
}


inline SEXP rcpp_slice_wo_<RCPP_TYPE>(
    const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  SEXP out = PROTECT(Rf_allocVector(<SXP_TYPE>, len));
  <COMMENT> <scalar_type> *pout = <FUN_TYPE>(out);
  const <scalar_type> *px = <FUN_TYPE>_RO(x);
  
  R_xlen_t counter = 0;
  if(start > 0) {
    // Comment
    for(R_xlen_t i = 0; i < start; ++i) {
      <SET_FUN>out, i, px[i]);
    }
    counter = start;
  }
  
  if(by == 2) {
    for(R_xlen_t i = start; i < end; i+=2) {
      <SET_FUN>out, counter, px[i+1]);
      counter++;
    }
  }
  
  if(by > 2) {
    for(R_xlen_t i = start; i < end; i += by) {
      R_xlen_t startx = i + 1;
      for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
        <SET_FUN>out, counter, px[j]);
        counter++;
      }
    }
  }
  
  if(end < (Rf_xlength(x) - 1)) {
    // Comment
    for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
      <SET_FUN>out, i - (end + 1) + counter, px[i]);
    }
  }
  
  UNPROTECT(1);
  return out;
}



inline void rcpp_slice_setinv_<RCPP_TYPE>(
    SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  <COMMENT> <scalar_type> *px = <FUN_TYPE>(x);
  const <scalar_type> *prp = <FUN_TYPE>_RO(rp);
  
  if(Rf_xlength(rp) == len) {
    R_xlen_t counter = 0;
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        <SET_FUN>x, i, prp[i]);
      }
      counter = start;
    }
    
    if(by == 2) {
      for(R_xlen_t i = start; i < end; i+=2) {
        <SET_FUN>x, i+1, prp[counter]);
        counter++;
      }
    }
    
    if(by > 2) {
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          <SET_FUN>x, j, prp[counter]);
          counter++;
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        <SET_FUN>x, i, prp[i - (end + 1) + counter]);
      }
    }
  }
  else if(Rf_xlength(rp) == 1) {
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        <SET_FUN>x, i, prp[0]);
      }
    }
    
    if(by == 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i+=2) {
        <SET_FUN>x, i+1, prp[0]);
      }
    }
    
    if(by > 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          <SET_FUN>x, j, prp[0]);
        }
      }
    }
    
    if(end < (Rf_xlength(x) - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < Rf_xlength(x); ++i) {
        <SET_FUN>x, i, prp[0]);
      }
    }
  }
  else {
    stop(\"recycling not allowed\");
  }
}

"
cat(templatecode)

templatecodes <- character(6L)
for(i in 1:6) {
  
  search <- c("<RCPP_TYPE>", "<SXP_TYPE>", "<scalar_type>", "<FUN_TYPE>", "<SET_FUN>", "<COMMENT>")
  replace <- c(RCPP_TYPES[i], SXP_TYPES[i], scalar_types[i], FUN_TYPES[i], SET_FUNS[i], COMMENTS[i])
  
  templatecodes[i] <- stri_replace_all(
    templatecode,
    replace,
    fixed = search,
    vectorize_all = FALSE
  )
}

cat(templatecodes)

code <- stri_c(
  header_for_source,
  stri_c(templatecodes, collapse = "\n"),
  collapse = "\n\n"
)

readr::write_file(code, "template_slice.txt")

Rcpp::sourceCpp(
  code = code
)

################################################################################
# atomic switches ====


template_names <- c(
  "rcpp_slice_x",
  "rcpp_slice_set",
  "rcpp_slice_xrev",
  "rcpp_slice_setrev",
  "rcpp_slice_wo",
  "rcpp_slice_setinv"
)


template_inputs <- rep(c(
  "x, start, end, by, len",
  "x, rp, start, end, by, len"
), 3L)

template_returns <- rep(c(
  "return", ""
), 3L)

atomic_fun_args <- rep(c(
  "const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len",
  "SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len"
), 3L)


atomic_fun_names <- stri_c(
  template_names, "_atomic"
)

atomic_fun_returntypes <- rep(c(
  "SEXP", "void"
), 3L)

atomic_fun_returnoutes <- rep(c(
  "return R_NilValue;", ""
), 3L)

atomic_codes <- character(6)

for(i in 1:6) {
  
  temp <- stri_c(
    
    "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.<ATOMIC_FUN_NAME>)]]
<returntype> <ATOMIC_FUN_NAME>(
  <ATOMIC_FUN_ARGS>
) {

",
    
    
    make_atomic_switches("x", template_returns[i], template_names[i], template_inputs[i], SXP_TYPES, RCPP_TYPES),
    
    
    "

<returnout>

}
"
  )
  
  temp <- stri_replace_all_fixed(
    temp,
    c("<returntype>", "<ATOMIC_FUN_NAME>", "<ATOMIC_FUN_ARGS>", "<returnout>"),
    c(atomic_fun_returntypes[i], atomic_fun_names[i], atomic_fun_args[i], atomic_fun_returnoutes[i]),
    vectorise_all = FALSE
  )
  
  atomic_codes[i] <- temp
  
}

atomic_code <- stri_paste(atomic_codes, collapse = "\n \n")

cat(atomic_code)


################################################################################
# combining code ====


final_code <- atomic_code



rcpp_code <- paste(c(header_for_source, templatecodes, final_code), collapse = "\n\n\n")
cat(rcpp_code)

Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)

code <-  paste(c(header_for_package, templatecodes, final_code), collapse = "\n\n\n")

setwd("..")
fileConn <- file("src/dynamic_rcpp_slice.cpp")
writeLines(code, fileConn)
close(fileConn)

# 
# # speed check ====
# 
# x <- sample(1:1e8)
# n <- 1e7
# foo <- bench::mark(
#   new = .rcpp_slice_x_atomic(x, 1, n, 1L, n),
#   current = squarebrackets:::.rcpp_slice_x_atomic(x, 1, n, 1L, n),
#   min_iterations = 100
# )
# summary(foo)
# plot(foo)
# 
