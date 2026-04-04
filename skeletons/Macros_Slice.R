library(stringi)

################################################################################
# general ====
#


header <- "


#include <Rcpp.h>
using namespace Rcpp;

"

macro_general <- "


#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \\
  POINTER[INDEX] = REPLACEMENT; \\
} while(0)



"




################################################################################
# sliceseq ====
#


macro_sliceseq <- "


#define MACRO_SLICE_SEQ_RANGE(STARTPOS, ENDPOS, DOCODE) do {  \\
  for(R_xlen_t i = STARTPOS; i <= ENDPOS; ++i) {  \\
    DOCODE; \\
  } \\
} while(0)


#define MACRO_SLICE_SEQ_FW(DOCODE) do {  \\
  for(R_xlen_t i = start; i <= end; i += by) { \\
    DOCODE; \\
  } \\
} while(0)


#define MACRO_SLICE_SEQ_BW(DOCODE) do {  \\
  for(R_xlen_t i = start; i >= end; i -= by) { \\
    DOCODE; \\
  } \\
} while(0)


#define MACRO_SLICE_SEQ_INV(DOCODE) do { \\
  if(start > 0) {  \\
    R_xlen_t startpos = 0;  \\
    R_xlen_t endpos = start - 1; \\
    MACRO_SLICE_SEQ_RANGE(  \\
      startpos, endpos, \\
      DOCODE  \\
    );  \\
  } \\
  for(R_xlen_t j = start; j < end; j += by) {  \\
    R_xlen_t startx = j + 1;  \\
    for(R_xlen_t i = startx; i < (startx + by - 1); ++i) { \\
      DOCODE; \\
    } \\
  } \\
  if(end < (Rf_xlength(x) - 1)) {  \\
    R_xlen_t startpos = end + 1; \\
    R_xlen_t endpos = Rf_xlength(x) - 1;  \\
    MACRO_SLICE_SEQ_RANGE(  \\
      startpos, endpos, \\
      DOCODE  \\
    );  \\
  } \\
} while(0)


#define MACRO_SLICE_SEQ(DOCODE) do {  \\
  const R_xlen_t start = rcpp_stride_get_Rxlent(stride, 0) - 1; \\
  const R_xlen_t end = rcpp_stride_get_Rxlent(stride, 1) - 1; \\
  const R_xlen_t by = rcpp_stride_get_Rxlent(stride, 2);  \\
  const R_xlen_t len = rcpp_stride_get_Rxlent(stride, 4); \\
  \\
  if(len == 0) {  \\
    break;  \\
  } \\
  else if(use < 0) { \\
    MACRO_SLICE_SEQ_INV(DOCODE);  \\
  } \\
  else if(start <= end) { \\
    MACRO_SLICE_SEQ_FW(DOCODE);  \\
  } \\
  else if(start > end) {  \\
    MACRO_SLICE_SEQ_BW(DOCODE); \\
  } \\
  else {  \\
    stop(\"unknown stride argument given\");  \\
  } \\
} while(0)
  

"




################################################################################
# slicep ====
#


macro_slicep <- "


#define MACRO_SLICE_PTRN_FW(DOCODE) do {  \\
  R_xlen_t end_verysafe = end - ppattern[pattern_len - 1]; \\
  R_xlen_t k = start; \\
  R_xlen_t i; \\
  for(; k <= end_verysafe; k += by) { \\
    for(R_xlen_t j = 0; j < pattern_len; ++j) { \\
      i = k + ppattern[j]; \\
      DOCODE; \\
    } \\
  } \\
  for (; k <= end; k += by) { \\
    for (R_xlen_t j = 0; j < pattern_len; ++j) {  \\
      i = k + ppattern[j]; \\
      if (i > end) break; \\
      DOCODE; \\
    } \\
  } \\
} while(0)


#define MACRO_SLICE_PTRN_BW(DOCODE) do {  \\
  R_xlen_t end_verysafe = end + ppattern[pattern_len - 1]; \\
  R_xlen_t k = start; \\
  R_xlen_t i; \\
  for(; k >= end_verysafe; k -= by) { \\
    for(R_xlen_t j = 0; j < pattern_len; ++j) { \\
      i = k - ppattern[j]; \\
      DOCODE; \\
    } \\
  } \\
  for (; k >= end; k -= by) { \\
    for (R_xlen_t j = 0; j < pattern_len; ++j) {  \\
      i = k - ppattern[j]; \\
      if (i < end) break; \\
      DOCODE; \\
    } \\
  } \\
} while(0)


#define MACRO_SLICE_PTRN_RANGE(STARTPOS, ENDPOS, DOCODE) do { \\
  for(R_xlen_t i = STARTPOS; i <= ENDPOS; ++i) {  \\
    DOCODE; \\
  } \\
} while(0)


#define MACRO_SLICE_PTRN_INV(DOCODE) do { \\
  if(start > 0) {  \\
    R_xlen_t startpos = 0;  \\
    R_xlen_t endpos = start - 1; \\
    MACRO_SLICE_PTRN_RANGE( \\
      startpos, endpos, \\
      DOCODE  \\
    );  \\
  } \\
    \\
  MACRO_SLICE_PTRN_FW(DOCODE);  \\
    \\
  if(end < (Rf_xlength(x) - 1)) {  \\
    R_xlen_t startpos = end + 1; \\
    R_xlen_t endpos = Rf_xlength(x) - 1;  \\
    MACRO_SLICE_PTRN_RANGE( \\
      startpos, endpos, \\
      DOCODE; \\
    );  \\
  } \\
} while(0)




#define MACRO_SLICE_PTRN(DOCODE) do {  \\
  const R_xlen_t start = rcpp_stride_get_Rxlent(stride, 0) - 1; \\
  const R_xlen_t end = rcpp_stride_get_Rxlent(stride, 1) - 1; \\
  const R_xlen_t by = rcpp_stride_get_Rxlent(stride, 2);  \\
  const R_xlen_t len = rcpp_stride_get_Rxlent(stride, 4);  \\
  const SEXP pattern = rcpp_stride_get_pattern(stride);  \\
  const int *ppattern = INTEGER_RO(pattern);  \\
  const R_xlen_t pattern_len = Rf_xlength(pattern); \\
  \\
  if(len == 0) {  \\
    break;  \\
  } \\
  else if(use < 0) { \\
    MACRO_SLICE_PTRN_INV(DOCODE);  \\
  } \\
  else if(start <= end) { \\
    MACRO_SLICE_PTRN_FW(DOCODE);  \\
  } \\
  else if(start > end) {  \\
    MACRO_SLICE_PTRN_BW(DOCODE); \\
  } \\
  else {  \\
    stop(\"unknown stride argument given\");  \\
  } \\
} while(0)
  



"




testfun <- "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.test)]]
int test(int x, int y) {
  return(x + y);
}

"

code <- stringi::stri_c(header, macro_general, macro_sliceseq, macro_slicep, testfun)

Rcpp::sourceCpp(code = code)

macros <- stringi::stri_c(
  macro_sliceseq, macro_slicep
)

readr::write_file(macros, "macros_slice.txt")

