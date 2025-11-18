
library(stringi)

################################################################################
# set-up ====
#

macros <- readr::read_file("macros.txt")

header_for_sourcing <- stri_c(
  "
  #include <Rcpp.h>
  
  using namespace Rcpp;
  ",
  macros
)

Rcpp::sourceCpp(code = header_for_sourcing)


header_for_package <- "

#include <Rcpp.h>

#include \"squarebrackets.h\"

using namespace Rcpp;


"



################################################################################
# rcpp_ss2ii_d_32 ====



ss2ii_32 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_d_32)]]
SEXP rcpp_ss2ii_d_32(
  SEXP sub, SEXP dimcumprod
) {


R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

R_xlen_t counter = 0;
int temp = 0;

int *pout;
SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
pout = INTEGER(out);
MACRO_DIM_DOCALL(MACRO_ss2ii);
UNPROTECT(1);
return out;

}


"


rcpp_code <- stri_c(header_for_sourcing, ss2ii_32, collapse = "\n\n\n")
cat(rcpp_code)
Rcpp::sourceCpp(code = rcpp_code)

rcpp_code <- stri_c(header_for_package, ss2ii_32, collapse = "\n\n\n")
setwd("..")
fileConn <- file("src/rcpp_ss2ii_d_32.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


################################################################################
# rcpp_ss2ii_d_64 ====


ss2ii_64 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_d_64)]]
SEXP rcpp_ss2ii_d_64(
  SEXP sub, SEXP dimcumprod
) {



R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

R_xlen_t counter = 0;
R_xlen_t temp = 0;

double *pout;
SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
pout = REAL(out);
MACRO_DIM_DOCALL(MACRO_ss2ii);
UNPROTECT(1);
return out;

}


"


rcpp_code <- stri_c(header_for_sourcing, ss2ii_64, collapse = "\n\n\n")
cat(rcpp_code)
Rcpp::sourceCpp(code = rcpp_code)

rcpp_code <- stri_c(header_for_package, ss2ii_64, collapse = "\n\n\n")
setwd("..")
fileConn <- file("src/rcpp_ss2ii_d_64.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



