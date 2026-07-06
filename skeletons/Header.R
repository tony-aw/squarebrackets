# set-up ====

library(stringi)



header <- "


#include <Rcpp.h>

using namespace Rcpp;


"



macros_arrays <- readr::read_file("macros_arrays.txt")
macros_stridev <- readr::read_file("macros_stridev.txt")
macros_slice <- readr::read_file("macros_slice.txt")

macro_set_atomic <- "

#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \\
  POINTER[INDEX] = REPLACEMENT; \\
} while(0)

"

macros <- stri_c(
  macro_set_atomic,
  macros_arrays,
  macros_stridev,
  macros_slice,
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

Rcpp::sourceCpp(code = stri_c(header, macros, testfun, collapse = "\n"))


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
Rcpp::sourceCpp(code = stri_c(header, header_macro))

setwd("..")
readr::write_file(header_macro, "src/squarebrackets.h")

