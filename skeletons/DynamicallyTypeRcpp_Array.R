
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

SEXPTYPES <- c("LGLSXP", "INTSXP", "REALSXP", "CPLXSXP", "STRSXP", "RAWSXP")
RCPPTYPES <- c("Logical", "Integer", "Numeric", "Complex", "Character", "Raw")


################################################################################
# rcpp_sub2ind_d_32 ====



sub2ind_32 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_d_32)]]
SEXP rcpp_sub2ind_d_32(
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
MACRO_DIM_DOCALL(MACRO_SUB2IND);
UNPROTECT(1);
return out;

}


"


rcpp_code <- stri_c(header_for_sourcing, sub2ind_32, collapse = "\n\n\n")
cat(rcpp_code)
Rcpp::sourceCpp(code = rcpp_code)

rcpp_code <- stri_c(header_for_package, sub2ind_32, collapse = "\n\n\n")
setwd("..")
fileConn <- file("src/rcpp_sub2ind_d_32.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


################################################################################
# rcpp_sub2ind_d_64 ====


sub2ind_64 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_d_64)]]
SEXP rcpp_sub2ind_d_64(
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
MACRO_DIM_DOCALL(MACRO_SUB2IND);
UNPROTECT(1);
return out;

}


"


rcpp_code <- stri_c(header_for_sourcing, sub2ind_64, collapse = "\n\n\n")
cat(rcpp_code)
Rcpp::sourceCpp(code = rcpp_code)

rcpp_code <- stri_c(header_for_package, sub2ind_64, collapse = "\n\n\n")
setwd("..")
fileConn <- file("src/rcpp_sub2ind_d_64.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()




################################################################################
# rcpp_set_array_d ====


templatecode <- "

template<int RTYPE> void rcpp_set_array_d_template(
  Vector<RTYPE> x, SEXP sub, SEXP dimcumprod, Vector<RTYPE> rp
) {


R_xlen_t n = 1;
for(int i = 0; i < Rf_length(sub); ++i) {
  n *= Rf_length(VECTOR_ELT(sub, i));
}

if(rp.length() == n) {
  R_xlen_t counter = 0;
  MACRO_DIM_DOCALL(MACRO_SETARRAY0);
}
else if(rp.length() == 1) {
  MACRO_DIM_DOCALL(MACRO_SETARRAY1);
}
else stop(\"recycling not allowed\");

}

"


rcpp_code <- paste(c(header_for_sourcing, templatecode), sep = "", collapse = "\n\n\n")
cat(rcpp_code)

Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)




switchpiece <- "
  case TYPESXP:
  {
    rcpp_set_array_d_template<TYPESXP>(as<RCPPTYPEVector>(x), sub, dimcumprod, as<RCPPTYPEVector>(rp));
    break;
  }
"

switches <- character(6L)

for(i in 1:6) {
  switches[i] <- stringi::stri_replace_all_fixed(
    switchpiece, c("TYPESXP", "RCPPTYPE"),  c(SEXPTYPES[i], RCPPTYPES[i]),
    vectorize_all = FALSE
  )
}

switches <- stringi::stri_paste(switches, collapse = "\n")
cat(switches)


atomcode <- stri_paste("

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_d_atomic)]]
void rcpp_set_array_DTYPEd_atomic(
  SEXP x, SEXP sub, SEXP dimcumprod, const SEXP rp
) {
  
  
  switch(TYPEOF(x)){
    ",
                       
                       
                       switches,
                       
                       
                       "
  }
}

"
)

cat(atomcode)

set_array <- stri_c(
  templatecode,
  atomcode,
  collapse = "\n\n"
)



rcpp_code <- stri_c(header_for_sourcing, set_array, collapse = "\n\n\n")
cat(rcpp_code)
Rcpp::sourceCpp(code = rcpp_code)

rcpp_code <- stri_c(header_for_package, set_array, collapse = "\n\n\n")
setwd("..")
fileConn <- file("src/rcpp_set_array_d.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

