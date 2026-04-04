
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

extraction_funs <- "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_stride_get_Rxlent)]]
  R_xlen_t rcpp_stride_get_Rxlent(
    List stride, int arg
  ) {
    RObject extraction = stride[arg];
    if(Rf_xlength(extraction) > 1) {
      stop(\"attempting to extract non-scalar R_xlen_t\");
    }
    if (is<IntegerVector>(extraction)) {
      return as<IntegerVector>(extraction)[0];
    } 
    else if (is<NumericVector>(extraction)) {
      return as<NumericVector>(extraction)[0];
    } 
    else {
      stop(\"attempting to extract non-numeric R_xlen_t\");
    }
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_stride_get_pattern)]]
SEXP rcpp_stride_get_pattern(
  List stride
) {
  RObject extraction = stride[3];
  return extraction;
}


"


macro_slice <- readr::read_file("macros_slice.txt")



header_for_package <- "

#include <Rcpp.h>

#include \"squarebrackets.h\"

using namespace Rcpp;


"



################################################################################
# template code ====

templatecode <- "

inline SEXP rcpp_slice_seq_x_<RCPP_TYPE>(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(<SXP_TYPE>, rcpp_stride_get_Rxlent(stride, 4)));
  <COMMENT> <scalar_type> *pout = <FUN_TYPE>(out);
  
  const <scalar_type> *px = <FUN_TYPE>_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_SEQ(
    <SET_FUN>out, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_seq_set_<RCPP_TYPE>(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
  <COMMENT> <scalar_type> *px = <FUN_TYPE>(x);
  const <scalar_type> *prp = <FUN_TYPE>_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop(\"recycling not allowed\");
  }
  
  MACRO_SLICE_SEQ(
    <SET_FUN>x, i, prp[index]); index += add
  );
}



inline SEXP rcpp_slice_ptrn_x_<RCPP_TYPE>(
    const SEXP x, const SEXP stride, int use
  ) {
  
  SEXP out = PROTECT(Rf_allocVector(<SXP_TYPE>, rcpp_stride_get_Rxlent(stride, 4)));
  <COMMENT> <scalar_type> *pout = <FUN_TYPE>(out);
  
  const <scalar_type> *px = <FUN_TYPE>_RO(x);
  
  // get indexing args:
  R_xlen_t index = 0;
  
  MACRO_SLICE_PTRN(
    <SET_FUN>out, index, px[i]); index++
  );
  
  UNPROTECT(1);
  return out;
  
}
  


inline void rcpp_slice_ptrn_set_<RCPP_TYPE>(
    SEXP x, const SEXP rp, SEXP stride, int use
  ) {
  
  <COMMENT> <scalar_type> *px = <FUN_TYPE>(x);
  const <scalar_type> *prp = <FUN_TYPE>_RO(rp);
  R_xlen_t index = 0;
  
  int add;
  
  if(Rf_xlength(rp) == rcpp_stride_get_Rxlent(stride, 4)) {
    add = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    add = 0;
  }
  else {
    stop(\"recycling not allowed\");
  }
  
  MACRO_SLICE_PTRN(
    <SET_FUN>x, i, prp[index]); index += add
  );
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
  macro_slice,
  extraction_funs,
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
  "rcpp_slice_seq_x",
  "rcpp_slice_seq_set",
  "rcpp_slice_ptrn_x",
  "rcpp_slice_ptrn_set"
)


template_inputs <- c(
  "x, stride, use",
  "x, rp, stride, use"
) |> rep(2)

template_returns <- rep(c(
  "return", ""
), 2L)

atomic_fun_args <- c(
  "const SEXP x, const SEXP stride, int use",
  "SEXP x, const SEXP rp, const SEXP stride, int use"
) |> rep(2L)


atomic_fun_names <- stri_c(
  template_names, "_atomic"
)

atomic_fun_returntypes <- c(
  "SEXP", "void"
) |> rep(2L)

atomic_fun_returnoutes <- c(
  "return R_NilValue;", ""
) |> rep(2L)

atomic_codes <- character(4)

for(i in seq_along(atomic_codes)) {
  
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

cat(atomic_codes)


################################################################################
# combining code ====


final_code <- atomic_code



rcpp_code <- paste(c(header_for_source, macro_slice, extraction_funs, templatecodes, final_code), collapse = "\n\n\n")
cat(rcpp_code)

Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)

code <-  paste(c(header_for_package, extraction_funs, templatecodes, final_code), collapse = "\n\n\n")

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
