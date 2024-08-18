# set-up ====

library(stringi)



################################################################################
# sub2ind dims ====

DTYPES <- 2:6
all_args <- stri_c("const SEXP ind", 1:6)
set_lengths <- c("int ni = Rf_length(ind1);",
                "int nj = Rf_length(ind2);",
                "int nk = Rf_length(ind3);",
                "int nl = Rf_length(ind4);",
                "int nm = Rf_length(ind5);",
                "int nn = Rf_length(ind6);")
all_lengths <- c("ni", "nj",  "nk", "nl", "nm", "nn")
all_for <- rev(c(
  "\t for(int n = 0; n < nn; ++n) {",
  "\t for(int m = 0; m < nm; ++m) {",
  "\t for(int l = 0; l < nl; ++l) {",
  "\t for(int k = 0; k < nk; ++k) {",
  "\t for(int j = 0; j < nj; ++j) {",
  "\t for(int i = 0; i < ni; ++i) {"
))

all_parts <- c(
  "pi[i]",
  "pdim1 * (pj[j] - 1)",
  "pdim2 * (pk[k] - 1)",
  "pdim3 * (pl[l] - 1)",
  "pdim4 * (pm[m] - 1)",
  "pdim5 * (pn[n] - 1)"
)

set_pointers <- sprintf("int *p%s; \n p%s = INTEGER(ind%d);", letters[9:14], letters[9:14], 1:6)
set_dimcumprod <- sprintf("double pdim%d = REAL(dimcumprod)[%d]; \n", 1:5, 0:4)

templatecode <- "

#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP C_sub2ind_DTYPEd(
  <args>, const SEXP dimcumprod
) {

<set_lengths>

R_xlen_t counter = 0;
double temp = 0.0;

<set_pointers>
<set_dimcumprod>


double *pout;
SEXP out = PROTECT(allocVector(REALSXP, <set_length_mult>));
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

C_scripts <- character(length(DTYPES))
names(C_scripts) <- DTYPES
for(i in DTYPES) {

  current_args <- stri_c(all_args[1:i], collapse = ", ")
  current_setlengths <- stri_c(set_lengths[1:i], collapse = "\n")
  current_setlength_mult <- stri_c(all_lengths[1:i], collapse = " * ")
  current_pointers <- stri_c(set_pointers[1:i], collapse = "\n")
  current_dimcumprod <- stri_c(set_dimcumprod[1:(i-1)], collapse = "\n")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main <- stri_c(all_parts[1:i], collapse = " + ")
  current_end <- stri_c(rep("\t }", i), collapse = "\n")

  current_fixed <- c(
    "DTYPE",
    "<args>",
    "<set_lengths>",
    "<set_length_mult>",
    "<set_pointers>",
    "<set_dimcumprod>",
    "<startfor>",
    "<main>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_args,
    current_setlengths,
    current_setlength_mult,
    current_pointers,
    current_dimcumprod,
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

  mypath <- sprintf("src/C_sub2ind_%dd.c", i)
  fileConn <- file(mypath)
  writeLines(out, fileConn)
  close(fileConn)

}


# wrappers:
wrapper_codes <- character(length(DTYPES))
names(wrapper_codes) <- DTYPES
args <- sprintf("ind%d", 1:6)
args_assigned <- sprintf("ind%d = as.integer(ind%d)", 1:6, 1:6)

templatecode <- "
#' @keywords Internal
#' @noRd
.C_sub2ind_DTYPEd <- function(
  <args>, dimcumprod
) {
  .Call(
    \"C_sub2ind_DTYPEd\",
    <args assigned>,
    dimcumprod = as.double(dimcumprod)
  )
}
"
for(i in DTYPES) {
  
  current_args <- stri_c(args[1:i], collapse = ", ")
  current_args_assigned <- stri_c(args_assigned[1:i], collapse = ", ")
  
  current_fixed <- c(
    "DTYPE",
    "<args>",
    "<args assigned>"
  )
  current_replacement <- c(
    i,
    current_args,
    current_args_assigned
  )
  
  out <- stri_replace_all(
    templatecode,
    fixed = current_fixed,
    replacement = current_replacement,
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
  
  wrapper_codes[i] <- out
}

cat(wrapper_codes)
