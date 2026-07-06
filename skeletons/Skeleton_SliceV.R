source("source.R")


library(stringi)

convert_macro <- function(x) {
  x <- stri_split(x, regex = "\\n")[[1L]]
  ind <- 2:(length(x) - 2L)
  x[ind] <- stringi::stri_c(x[ind], "\t\\")
  x <- stringi::stri_c(x, collapse = "\n")
  return(x)
}


header_for_source <- "

#include <Rcpp.h>
using namespace Rcpp;


#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \\
  POINTER[INDEX] = REPLACEMENT; \\
} while(0)


inline int rcpp_count_stringmatches(SEXP y, SEXP v) {
  int n = Rf_length(v);
  const SEXP *pv = STRING_PTR_RO(v);
  int count = 0;
  for(int i = 0; i < n; ++i) {
    if((int)R_compute_identical(y, pv[i], 0)) {
      count++;
    }
  }
  return count;
}




"


header_for_package <- "

#include <Rcpp.h>

#include \"squarebrackets.h\"

using namespace Rcpp;



inline int rcpp_count_stringmatches(SEXP y, SEXP v) {
  int n = Rf_length(v);
  const SEXP *pv = STRING_PTR_RO(v);
  int count = 0;
  for(int i = 0; i < n; ++i) {
    if((int)R_compute_identical(y, pv[i], 0)) {
      count++;
    }
  }
  return count;
}

#define MACRO_SET_ATOMIC(POINTER, INDEX, REPLACEMENT) do {  \\
  POINTER[INDEX] = REPLACEMENT; \\
} while(0)


"


################################################################################
# slicev_x ====
#

templatecode <- "
SEXP rcpp_slicev_x_<Rcpp_Type>(
  SEXP x, List preplist, NumericVector prepvector, SEXP pool
) {
  
  const <scalar_type> *px = <FUN_TYPE>_RO(x);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  const R_xlen_t indexform = prepvector[4];
  
  if(count_total == 0) {
    stop(\"no matches\");
  }
  
  SEXP out = PROTECT(Rf_allocVector(<SXP_TYPE>, count_total));
  <COMMENT> <scalar_type> *pout = <FUN_TYPE>(out);
  
  if(indexform == 0) {
    
    NumericVector first = preplist[0];
    NumericVector last = preplist[1];
    NumericVector count = preplist[2];
    NumericVector rnglen = preplist[3];
    const int n_chunks = Rf_length(first);
    
    R_xlen_t outcount = 0;
    
    for(int j = 0; j < n_chunks; ++j) {
      SEXP temp = VECTOR_ELT(pool, j);
      
      const R_xlen_t current_count = count[j];
      const R_xlen_t current_rnglen = rnglen[j];
      
      if(current_count == 0) {
        continue;
      }
      else if(current_count == 1) {
        const R_xlen_t first0 = first[j];
        <SET_FUN>out, outcount, px[first0]);
        outcount++;
      }
      else if(current_count == 2) {
        const R_xlen_t first0 = first[j];
        const R_xlen_t last0 = last[j];
        
        <SET_FUN>out, outcount, px[first0]);
        outcount++;
        
        <SET_FUN>out, outcount, px[last0]);
        outcount++;
      }
      else if(current_count == current_rnglen) {
        const R_xlen_t first0 = first[j];
        const R_xlen_t last0 = last[j];
        
        for(R_xlen_t i = first0; i <= last0; ++i) {
          <SET_FUN>out, outcount, px[i]);
          outcount++;
        }
      }
      else if(TYPEOF(temp) == RAWSXP) {
        const Rbyte *ptemp = RAW_RO(temp);
        R_xlen_t boolcount = 0;
        const R_xlen_t first0 = first[j];
        const R_xlen_t last0 = last[j];
        
        for(R_xlen_t i = first0; i <= last0; ++i) {
          if(ptemp[boolcount]) {
            <SET_FUN>out, outcount, px[i]);
            outcount++;
          }
          
          boolcount++;
        }
      }
    }
    
    
    
    UNPROTECT(1);
    return out;
    
  }
  else if(indexform == 1) {
  
    const double *ppool = REAL_RO(pool);
    const R_xlen_t n = Rf_xlength(pool);
    for(R_xlen_t i = 0; i < n; ++i) {
      <SET_FUN>out, i, px[(R_xlen_t)ppool[i]]);
    }
    
    UNPROTECT(1);
    return out;
    
  }
  else if(indexform == -1) {
    const R_xlen_t pool_len = Rf_xlength(pool);
    const R_xlen_t n = Rf_xlength(x);
    double* ppool = REAL(pool);
  
    R_xlen_t last_idx = 0;
    R_xlen_t counter = 0;
    
    for (R_xlen_t i = 0; i < pool_len; ++i) {
        
      R_xlen_t skip = ppool[i];
      
      for (R_xlen_t j = last_idx; j < skip; ++j) {
        <SET_FUN>out, counter, px[j]);
        counter++;
      }
      
      last_idx = skip + 1;
    }
    
    for (R_xlen_t j = last_idx; j < n; ++j) {
        <SET_FUN>out, counter, px[j]);
        counter++;
    }
    
    UNPROTECT(1);
    return out;
  }
  else {
    stop(\"unknown type of pool given\");
  }
}

"

templatecodes <- character(6L)

for(i in 1:6) {
  find <- c("<Rcpp_Type>", "<scalar_type>", "<FUN_TYPE>", "<SXP_TYPE>", "<COMMENT>",  "<SET_FUN>")
  replace <- c(RCPP_TYPES[i], scalar_types[i], FUN_TYPES[i], SXP_TYPES[i], COMMENTS[i], SET_FUNS[i])
  templatecodes[i] <- stri_replace_all(
    templatecode, replace, fixed = find, vectorize_all = FALSE
  )
}

templatecodes <- stri_c(templatecodes, collapse = "\n\n")

cat(templatecodes)


switches <- make_atomic_switches(
  "x", "return", "rcpp_slicev_x", "x, preplist, prepvector, pool", SXP_TYPES, RCPP_TYPES
)
cat(switches)


code_slicev_x <- stri_c(
  templatecodes,
  
  
  "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slicev_x_atomic)]]
SEXP rcpp_slicev_x_atomic(
  SEXP x, List preplist, NumericVector prepvector, SEXP pool
) {
",
  switches,
  "
  return R_NilValue;
}
"
)

cat(code_slicev_x)


code <- stri_paste(header_for_source, code_slicev_x)
cat(code)
Rcpp::sourceCpp(code = code) # no errors, good!




################################################################################
# slicev_set ====
#

templatecode <- "

void rcpp_slicev_set_<Rcpp_Type>(
  SEXP x, SEXP rp, List preplist, NumericVector prepvector, SEXP pool
) {
  
  <COMMENT> <scalar_type> *px = <FUN_TYPE>(x);
  const <scalar_type> *prp = <FUN_TYPE>_RO(rp);
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  const R_xlen_t indexform = prepvector[4];
  
  R_xlen_t rpcount = 0;
  
  int by_rp;
  if(Rf_xlength(rp) == count_total) {
    by_rp = 1;
  }
  else if(Rf_xlength(rp) == 1) {
    by_rp = 0;
  }
  else {
    stop(\"vector recycling not supported\");
  }
  
  if(count_total == 0) {
    stop(\"no matches\");
  }
  
  if(indexform == 0) {
    
    NumericVector first = preplist[0];
    NumericVector last = preplist[1];
    NumericVector count = preplist[2];
    NumericVector rnglen = preplist[3];
    const int n_chunks = Rf_length(first);
    
    R_xlen_t rpcount = 0;
    
    for(int j = 0; j < n_chunks; ++j) {
      SEXP temp = VECTOR_ELT(pool, j);
      
      const R_xlen_t current_count = count[j];
      const R_xlen_t current_rnglen = rnglen[j];
      
      
      if(current_count == 0) {
        continue;
      }
      else if(current_count == 1) {
        const R_xlen_t first0 = first[j];
        <SET_FUN>x, first0, prp[rpcount]);
        rpcount += by_rp;
      }
      else if(current_count == 2) {
        const R_xlen_t first0 = first[j];
        const R_xlen_t last0 = last[j];
        
        <SET_FUN>x, first0, prp[rpcount]);
        rpcount += by_rp;
        
        <SET_FUN>x, last0, prp[rpcount]);
        rpcount += by_rp;
      }
      else if(current_count == current_rnglen) {
        const R_xlen_t first0 = first[j];
        const R_xlen_t last0 = last[j];
        
        for(R_xlen_t i = first0; i <= last0; ++i) {
          <SET_FUN>x, i, prp[rpcount]);
          rpcount += by_rp;
        }
      }
      else if(TYPEOF(temp) == RAWSXP) {
        const Rbyte *ptemp = RAW_RO(temp);
        R_xlen_t boolcount = 0;
        const R_xlen_t first0 = first[j];
        const R_xlen_t last0 = last[j];
        
        for(R_xlen_t i = first0; i <= last0; ++i) {
          if(ptemp[boolcount]) {
            <SET_FUN>x, i, prp[rpcount]);
            rpcount += by_rp;
          }
          
          boolcount++;
        }
      }
    }
  }
  else if(indexform == 1) {
    
    const double *ppool = REAL_RO(pool);
    const R_xlen_t n = Rf_xlength(pool);
    for(R_xlen_t i = 0; i < n; ++i) {
      rpcount = i * by_rp;
      <SET_FUN>x, (R_xlen_t)ppool[i], prp[rpcount]);
    }
    
  }
  else if(indexform == -1) {
    const R_xlen_t count = Rf_xlength(pool);
    const R_xlen_t n = Rf_xlength(x);
    
    double* ppool = REAL(pool);
    
    R_xlen_t last_idx = 0;
    
    for (R_xlen_t i = 0; i < count; ++i) {
      R_xlen_t skip = ppool[i];
      
      for (R_xlen_t j = last_idx; j < skip; ++j) {
        <SET_FUN>x, j, prp[rpcount]);
        rpcount += by_rp;
      }
      
      last_idx = skip + 1;
    }
    
    for (R_xlen_t j = last_idx; j < n; ++j) {
      <SET_FUN>x, j, prp[rpcount]);
      rpcount += by_rp;
    }
  }
  else {
    stop(\"unknown type of pool given\");
  }
}


"


templatecodes <- character(6L)

for(i in 1:6) {
  find <- c("<Rcpp_Type>", "<scalar_type>", "<FUN_TYPE>", "<SXP_TYPE>", "<COMMENT>",  "<SET_FUN>")
  replace <- c(RCPP_TYPES[i], scalar_types[i], FUN_TYPES[i], SXP_TYPES[i], COMMENTS[i], SET_FUNS[i])
  templatecodes[i] <- stri_replace_all(
    templatecode, replace, fixed = find, vectorize_all = FALSE
  )
}

templatecodes <- stri_c(templatecodes, collapse = "\n\n")

cat(templatecodes)


switches <- make_atomic_switches(
  "x", "", "rcpp_slicev_set", "x, rp, preplist, prepvector, pool", SXP_TYPES, RCPP_TYPES
)
cat(switches)


code_slicev_set <- stri_c(
  templatecodes,
  
  
  "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slicev_set_atomic)]]
void rcpp_slicev_set_atomic(
  SEXP x, SEXP rp, List preplist, NumericVector prepvector, SEXP pool
) {
",
  switches,
  "
}
"
)

cat(code_slicev_set)


code <- stri_paste(header_for_source, code_slicev_set)
cat(code)
Rcpp::sourceCpp(code = code) # no errors, good!


################################################################################
# combining code ====
#

rcpp_code <- paste(c(header_for_source, code_slicev_x, code_slicev_set), collapse = "\n\n\n")
cat(rcpp_code)

Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)

code <-  paste(c(header_for_package, code_slicev_x, code_slicev_set), collapse = "\n\n\n")

setwd("..")
fileConn <- file("src/dynamic_rcpp_slicev.cpp")
writeLines(code, fileConn)
close(fileConn)

