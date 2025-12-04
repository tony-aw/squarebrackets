# set-up ====

library(stringi)


macros_arrays <- readr::read_file("macros_arrays.txt")
macros_slicev <- readr::read_file("macros_slicev.txt")

macro_set_atomic <- "

#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \\
  POINTER[INDEX] = REPLACEMENT; \\
} while(0)

"

macros <- stri_c(
  macro_set_atomic,
  macros_arrays,
  macros_slicev,
  collapse = "\n\n"
)
cat(macros)


testfun <- "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.test)]]
int test(int x, int y) {
  return(x + y);
}

"

Rcpp::sourceCpp(code = stri_c(macros, testfun, collapse = "\n"))


header_macro <- stri_c("

#ifndef SQUAREBRACKETS_H
#define SQUAREBRACKETS_H

",
                 
macros,

"

#endif
"
)



cat(header_macro)
Rcpp::sourceCpp(code = header_macro)

setwd("..")
readr::write_file(header_macro, "src/squarebrackets.h")

