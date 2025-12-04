
library(stringi)
source("source.R")
macros_arrays <- readr::read_file("macros_arrays.txt")


header_for_source <- stri_c("

#include <Rcpp.h>

using namespace Rcpp;

#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \\
  POINTER[INDEX] = REPLACEMENT; \\
} while(0)

",
   macros_arrays,
   collapse = "\n\n"

)


header_for_package <- "

#include <Rcpp.h>

#include \"squarebrackets.h\"

using namespace Rcpp;


"



################################################################################
# template codes ====
#

templatecode <- "

inline void rcpp_set_array_d_<RCPP_TYPE>(
  SEXP x, SEXP sub, SEXP dimcumprod, SEXP rp
) {


R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

<COMMENT> <scalar_type> *px = <FUN_TYPE>(x);
const <scalar_type> *prp = <FUN_TYPE>_RO(rp);

if(Rf_xlength(rp) == n) {
  R_xlen_t counter = 0;
  MACRO_DIM_DOCALL(<SET_FUN>x, flatind - 1, prp[counter]); counter++);
}
else if(Rf_xlength(rp) == 1) {
  
  MACRO_DIM_DOCALL(<SET_FUN>x, flatind - 1, prp[0]));
}
else stop(\"recycling not allowed\");

}

"


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

templatecodes <- stri_c(templatecodes, collapse = "\n\n")

cat(templatecodes)



################################################################################
# atomic switches ====
#

switches <- make_atomic_switches(
  "x", "", "rcpp_set_array_d", "x, sub, dimcumprod, rp", SXP_TYPES, RCPP_TYPES
)
cat(switches)


atomic_code <- stri_c("

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_d_atomic)]]
void rcpp_set_array_d_atomic(
  SEXP x, SEXP sub, SEXP dimcumprod, SEXP rp
) {
",
               
switches,
"

}


"
)



################################################################################
# check code ====
#


code <- stri_c(
  header_for_source,
  templatecodes,
  atomic_code,
  collapse = "\n\n"
)

cat(code)

readr::write_file(code, "template_setarray.txt")

Rcpp::sourceCpp(
  code = code
) # no errors; good!


################################################################################
# write code ====
#


code <-  paste(c(header_for_package, templatecodes, atomic_code), collapse = "\n\n\n")

setwd("..")
fileConn <- file("src/dynamic_rcpp_set_array_d.cpp")
writeLines(code, fileConn)
close(fileConn)


