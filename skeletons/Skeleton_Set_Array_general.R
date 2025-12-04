
library(stringi)
source("source.R")


header_for_source <- stri_c("

#include <Rcpp.h>

using namespace Rcpp;

#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \\
  POINTER[INDEX] = REPLACEMENT; \\
} while(0)

",
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

inline void rcpp_set_array_general_<RCPP_TYPE>(
  SEXP x,
  const SEXP s, const SEXP xdims,
  const SEXP rp
) {

  int k;
  
  const void *vmaxsave = vmaxget(); // Because I'm gonna use R_alloc()
  
 // s is a list
 
  k = Rf_length(xdims);

  // allocate vectors (kinda like a list) through R_alloc(); k = ndims(x)
  int **subs = (int**)R_alloc(k, sizeof(int*));
  int *indx = (int*)R_alloc(k, sizeof(int));
  int *bound = (int*)R_alloc(k, sizeof(int));
  R_xlen_t *offset = (R_xlen_t*)R_alloc(k, sizeof(R_xlen_t));
  
  
  // prep
  R_xlen_t n = 1;
  SEXP r;
  
  for (int i = 0; i < k; i++) {
    r = VECTOR_ELT(s, i); 
  	indx[i] = 0;
  	bound[i] = Rf_length(r);
    n *= bound[i];
  	subs[i] = INTEGER(r);
  }
  
  offset[0] = 1;
  for (int i = 1; i < k; i++) {
    offset[i] = offset[i - 1] * INTEGER(xdims)[i - 1];
  }
  
  <COMMENT> <scalar_type> *px = <FUN_TYPE>(x);
  const <scalar_type> *prp = <FUN_TYPE>_RO(rp);
  
  
  // main function
  if(Rf_xlength(rp) == n) {
    R_xlen_t counter = 0;

    for (R_xlen_t i = 0; i < n; i++) {
      R_xlen_t flatind = 0;
    	for (int j = 0; j < k; j++) {
  	    int jj = subs[j][indx[j]];
  	    flatind += (jj - 1) * offset[j];
    	}
    	
    	<SET_FUN>x, flatind, prp[counter]); // meat in the voodoo sandwich
    	counter++;
    	
    	if (n > 1) {
    	    int j = 0;
    	    while (++indx[j] >= bound[j]) {
        		indx[j] = 0;
        		j = (j + 1) % k;
    	    }
    	}
    }
  }
  else if(Rf_xlength(rp) == 1) {

    for (R_xlen_t i = 0; i < n; i++) {
      R_xlen_t flatind = 0;
    	for (int j = 0; j < k; j++) {
  	    int jj = subs[j][indx[j]];
  	    flatind += (jj - 1) * offset[j];
    	}
    	
    	<SET_FUN>x, flatind, prp[0]); // meat in the voodoo sandwich
    	
    	if (n > 1) {
    	    int j = 0;
    	    while (++indx[j] >= bound[j]) {
        		indx[j] = 0;
        		j = (j + 1) % k;
    	    }
    	}
    }
  }
  else {
    stop(\"recycling not allowed\");

  }
  

  // Free temporary memory
  vmaxset(vmaxsave);
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
  "x", "", "rcpp_set_array_general", "x, s, xdims, rp", SXP_TYPES, RCPP_TYPES
)
cat(switches)


atomic_code <- stri_c("

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_general_atomic)]]
void rcpp_set_array_general_atomic(
  SEXP x, SEXP s, SEXP xdims, SEXP rp
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


Rcpp::sourceCpp(
  code = code
) # no errors; good!


################################################################################
# write code ====
#


code <-  paste(c(header_for_package, templatecodes, atomic_code), collapse = "\n\n\n")

setwd("..")
fileConn <- file("src/dynamic_rcpp_set_array_general.cpp")
writeLines(code, fileConn)
close(fileConn)


