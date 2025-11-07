# set-up ====

library(stringi)

DTYPES <- 2:16


SXPTYPES <- c("LGLSXP", "INTSXP", "REALSXP", "CPLXSXP", "STRSXP", "RAWSXP")
RCPPTYPES <- c("Logical", "Integer", "Numeric", "Complex", "Character", "Raw")

header <- "

#include <Rcpp.h>

using namespace Rcpp;

"


################################################################################
# MACRO dims dtype ====
#

make_sub <- sprintf("SEXP ind%d = VECTOR_ELT(sub, %d);\t\\", 1:16, 0:15)
setlengths <- paste("int len", 1:16, " = Rf_length(ind", 1:16, ");\t\\", sep= "")
make_pointers <- sprintf("const int *pind%d = INTEGER_RO(ind%d);\t\\", 1:16, 1:16)
all_lengths <- paste("len", 1:16, sep = "")
all_parts_decl <- sprintf("i_parts%d", 1:16)


fortext <- "for(int iter%d = 0; iter%d < len%d; ++iter%d) {\t\\
\ti_parts%d = pdcp[%d] * (pind%d[iter%d] - 1) + i_parts%d;\t\\"

fortext1 <- "for(int iter1 = 0; iter1 < len1; ++iter1) {\t\\
\ti_parts1 = pind1[iter1] + i_parts2;\t\\"

all_for <- c(
  fortext1,
  sprintf(fortext, DTYPES, DTYPES, DTYPES, DTYPES,
          DTYPES, DTYPES - 2L, DTYPES, DTYPES, DTYPES + 1L)
)


templatecode <- "
#define MACRO_DIM_<dtype>(DOCODE) do {      \\
  <make_sub>
  <setlengths>
  <make_pointers>
  R_xlen_t <parts_decl>;  \\
  const double *pdcp = REAL_RO(dimcumprod); \\
  R_xlen_t flatind = 0;           \\
                              \\
  <startfor>
        flatind = i_parts1;     \\
        DOCODE;               \\
  <endfor>
} while(0)


"

rcpp_scripts <- character(length(DTYPES))
names(rcpp_scripts) <- DTYPES
counter <- 1
for(i in DTYPES) {
  
  current_make_sub <- stri_c(make_sub[1:i], collapse = "\n")
  current_setlengths <- stri_c(setlengths[1:i], collapse = "\n")
  current_makepointers <- stri_c(make_pointers[1:i], collapse = "\n")
  current_parts_decl <- stri_c(all_parts_decl[1:i], collapse = ", ")
  current_setlength_mult <- stri_c(all_lengths[1:i], collapse = " * ")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  find <- sprintf(c(" + i_parts%d"), i + 1)
  current_for <- stri_replace_all(
    current_for, c(""), fixed = find, vectorise_all = FALSE
  )
  current_end <- stri_c(rep("\t }\t\\", i), collapse = "\n")
  
  current_fixed <- c(
    "<dtype>",
    "<make_sub>",
    "<setlengths>",
    "<make_pointers>",
    "<parts_decl>",
    "<setlength_mult>",
    "<startfor>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_make_sub,
    current_setlengths,
    current_makepointers,
    current_parts_decl,
    current_setlength_mult,
    current_for,
    current_end
  )
  
  out <- stri_replace_all(
    templatecode,
    fixed = current_fixed,
    replacement = current_replacement,
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
  
  rcpp_scripts[[counter]] <- out
  counter <- counter + 1
}


macro_dim_d <- paste(rcpp_scripts, collapse = "\n\n\n")
cat(macro_dim_d)

Rcpp::sourceCpp(
  code = paste(header, macro_dim_d, collapse = "\n\n\n") # no errors, good
)




################################################################################
# MACRO dims docall ====
#


# cases:
case <-
  "case %d:                                       \\
{                                                 \\
  MACRO_DIM_%d(DOCODE);                           \\
  break;                                          \\
}                                                 \\
"
cases <- sprintf(case, DTYPES, DTYPES) |> stringi::stri_c(collapse = "")


cat(cases)

templatecode_docall <- "

#define MACRO_DIM_DOCALL(DOCODE) do {     \\
  int ndims = Rf_length(sub);         \\
                                          \\
  switch(ndims) {       \\
    <cases>       \\
  }       \\
} while(0)

"

templatecode_docall2 <- stringi::stri_replace_all(
  templatecode_docall,
  fixed = c("<cases>"),
  replacement = c(cases),
  vectorize_all = FALSE
)

macro_dim_docall <- templatecode_docall2
cat(macro_dim_docall)


rcpp_code <- paste(c(header, macro_dim_d, macro_dim_docall), collapse = "\n\n\n")
cat(rcpp_code)

Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)



################################################################################
# MACRO ACTIONS ====
#


macro_sub2ind <- "

#define MACRO_SUB2IND do {  \\
  pout[counter] = flatind;   \\
  counter++;              \\
} while(0)

"

macro_setarray0 <- "

#define MACRO_SETARRAY0 do {  \\
  x[flatind - 1] = rp[counter]; \\
      counter++;  \\
} while(0)

"

macro_setarray1 <- "

#define MACRO_SETARRAY1 do {  \\
  x[flatind - 1] = rp[0]; \\
} while(0)

"

macros_action <- stri_c(
  macro_sub2ind, macro_setarray0, macro_setarray1, collapse = "\n\n"
)

Rcpp::sourceCpp(code = macros_action)



################################################################################
# Write & test macros ====
#


macros <- stri_c(macro_dim_d, macro_dim_docall, macros_action, collapse = "\n\n\n")
readr::write_file(macros, "macros_arrays.txt")


testfun <- "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.test)]]
int test(int x, int y) {
  return(x + y);
}

"

Rcpp::sourceCpp(code = stri_c(macros, testfun, collapse = "\n"))


