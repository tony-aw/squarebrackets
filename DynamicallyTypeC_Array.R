# set-up ====

library(stringi)



################################################################################
# C_sub2ind_16d_32 ====

all_args <- stri_c("const SEXP ind", 1:16)
getelements <- stri_c("SEXP ind", 1:16, " = VECTOR_ELT(sub, ", 0:15, ");")
setlengths <- paste("int len", 1:16, " = Rf_length(ind", 1:16, ");", sep= "")
make_pointers <- sprintf("const int *pind%d;\npind%d = INTEGER_RO(ind%d);\n", 1:16, 1:16, 1:16)
all_lengths <- paste("len", 1:16, sep = "")
all_for <- rev(
  sprintf("\t for(int iter%d = 0; iter%d < len%d; ++iter%d) {\n", 16:1, 16:1, 16:1, 16:1)
)

all_parts <- c(
  "pind1[iter1]",
  sprintf("pdim[%d] * (pind%d[iter%d] - 1)", 0:14, 2:16, 2:16)
)


templatecode <- "
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_sub2ind_16d_32(
  SEXP sub, SEXP dimcumprod
) {

<get_elements>

<setlengths>

<make_pointers>

R_xlen_t counter = 0;
int temp = 0;

const int *pdim;
pdim = INTEGER(dimcumprod);

int *pout;
SEXP out = PROTECT(Rf_allocVector(INTSXP, <setlength_mult>));
pout = INTEGER(out);
  
<startfor>
      
      temp = <main>;
      pout[counter] = temp;
      counter++;
      
<endfor>

UNPROTECT(1);
return out;

}


"

i = 16
current_args <- stri_c(all_args, collapse = ", ")
current_get_elements <- stri_c(getelements, collapse = "\n")
current_setlengths <- stri_c(setlengths, collapse = "\n")
current_makepointers <- stri_c(make_pointers, collapse = "\n")
current_setlength_mult <- stri_c(all_lengths, collapse = " * ")
current_for <- stri_c(all_for[i:1], collapse = "\n")
current_main <- stri_c(all_parts, collapse = " + ")
current_end <- stri_c(rep("\t }", i), collapse = "\n")

current_fixed <- c(
  "<args>",
  "<get_elements>",
  "<setlengths>",
  "<make_pointers>",
  "<setlength_mult>",
  "<startfor>",
  "<main>",
  "<endfor>"
)
current_replacement <- c(
  current_args,
  current_get_elements,
  current_setlengths,
  current_makepointers,
  current_setlength_mult,
  current_for,
  current_main,
  current_end
)

out <- stri_replace_all(
  templatecode,
  fixed = current_fixed,
  replacement = current_replacement,
  case_insensitive = FALSE,
  vectorize_all = FALSE
)

cat(out)

Rcpp::cppFunction(code = out)

fileConn <- file("src/C_sub2ind_16d_32.c")
writeLines(out, fileConn)
close(fileConn)





################################################################################
# C_sub2ind_16d_64 ====


templatecode <- "
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_sub2ind_16d_64(
  SEXP sub, SEXP dimcumprod
) {

<get_elements>

<setlengths>

<make_pointers>

R_xlen_t counter = 0;
double temp = 0;

const double *pdim;
pdim = REAL(dimcumprod);

double *pout;
SEXP out = PROTECT(Rf_allocVector(REALSXP, <setlength_mult>));
pout = REAL(out);
  
<startfor>
      
      temp = <main>;
      pout[counter] = temp;
      counter++;
      
<endfor>

UNPROTECT(1);
return out;

}


"

i = 16
current_args <- stri_c(all_args, collapse = ", ")
current_get_elements <- stri_c(getelements, collapse = "\n")
current_setlengths <- stri_c(setlengths, collapse = "\n")
current_makepointers <- stri_c(make_pointers, collapse = "\n")
current_setlength_mult <- stri_c(all_lengths, collapse = " * ")
current_for <- stri_c(all_for[i:1], collapse = "\n")
current_main <- stri_c(all_parts, collapse = " + ")
current_end <- stri_c(rep("\t }", i), collapse = "\n")

current_fixed <- c(
  "<args>",
  "<get_elements>",
  "<setlengths>",
  "<make_pointers>",
  "<setlength_mult>",
  "<startfor>",
  "<main>",
  "<endfor>"
)
current_replacement <- c(
  current_args,
  current_get_elements,
  current_setlengths,
  current_makepointers,
  current_setlength_mult,
  current_for,
  current_main,
  current_end
)

out <- stri_replace_all(
  templatecode,
  fixed = current_fixed,
  replacement = current_replacement,
  case_insensitive = FALSE,
  vectorize_all = FALSE
)

cat(out)

Rcpp::cppFunction(code = out)

fileConn <- file("src/C_sub2ind_16d_64.c")
writeLines(out, fileConn)
close(fileConn)

