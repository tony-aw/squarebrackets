
library(stringi)

convert_macro <- function(x) {
  x <- stri_split(x, regex = "\\n")[[1L]]
  ind <- 2:(length(x) - 2L)
  x[ind] <- stringi::stri_c(x[ind], "\t\\")
  x <- stringi::stri_c(x, collapse = "\n")
  return(x)
}



header <- "


#include <Rcpp.h>

using namespace Rcpp;


"


inlines <- "


inline int inline_count_stringmatches(SEXP y, SEXP v) {
  int n = Rf_length(v);
  const SEXP *pv = STRING_PTR_RO(v);
  for(int i = 0; i < n; ++i) {
    if((int)R_compute_identical(y, pv[i], 0)) {
      return 1;
    }
  }
  return 0;
}

"

# prep ====

macro_stridev_prep <- "
#define MACRO_STRIDEV_PREP(CONDITIONCODE) do {
  
  R_xlen_t n = Rf_xlength(y);
  
  const NumericVector startpos = VECTOR_ELT(chunks, 0);
  const NumericVector endpos = VECTOR_ELT(chunks, 1);
  const int n_chunks = Rf_length(startpos);
  
  NumericVector first(n_chunks);
  NumericVector last(n_chunks);
  NumericVector count(n_chunks);
  NumericVector rnglen(n_chunks);
  
  for(int j = 0; j < n_chunks; ++j) {
    
    R_xlen_t startpos0 = startpos[j];
    R_xlen_t endpos0 = endpos[j];
    R_xlen_t first0 = -1;
    R_xlen_t last0 = -1;
    R_xlen_t count0 = 0;
    R_xlen_t rnglen0 = 0;
    
    
    for(R_xlen_t i = startpos0; i <= endpos0; ++i) {
      if((CONDITIONCODE) == condition) {
        first0 = i;
        last0 = i;
        break;
      }
    }
    
    if(first0 == endpos0) {
      count0 = 1;
    }
    if(first0 != -1 && first0 < endpos0) {
      for(R_xlen_t i = first0; i <= endpos0; ++i) {
        if((CONDITIONCODE) == condition) {
          count0++;
          last0 = i;
        }
      }
    }
    
    if(first0 > -1 && last0 > -1) {
      rnglen0 = last0 - first0 + 1;
    }
    
    first[j] = first0;
    last[j] = last0;
    count[j] = count0;
    rnglen[j] = rnglen0;
    
  }
  
  List out(4);
  out[0] = first;
  out[1] = last;
  out[2] = count;
  out[3] = rnglen;
  return out;

} while(0)
"
macro_stridev_prep <- convert_macro(macro_stridev_prep)


# pool ====

macro_stridev_pool <- "
#define MACRO_STRIDEV_POOL(CONDITIONCODE) do {
  
  const R_xlen_t first_total = prepvector[0];
  const R_xlen_t last_total = prepvector[1];
  const R_xlen_t count_total = prepvector[2];
  const R_xlen_t rnglen_total = prepvector[3];
  const int indexform = prepvector[4];
  
  const R_xlen_t n = Rf_xlength(y);
  
  
  if(indexform == 0) {
    
    NumericVector first = preplist[0];
    NumericVector last = preplist[1];
    NumericVector count = preplist[2];
    NumericVector rnglen = preplist[3];
    const int n_chunks = Rf_length(first);
    List out(n_chunks);
    
    for(int j = 0; j < n_chunks; ++j) {
      const R_xlen_t current_count = count[j];
      const R_xlen_t current_rnglen = rnglen[j];
      
      if(current_count == current_rnglen) {
        out[j] = R_NilValue;
      }
      else if(current_count <= 2) {
        out[j] = R_NilValue;
      }
      else {
        RawVector temp(current_rnglen);
        Rbyte *ptemp = RAW(temp);
        R_xlen_t counter = 0;
        
        const R_xlen_t first0 = first[j];
        const R_xlen_t last0 = last[j];
        
        for(R_xlen_t i = first0; i <= last0; ++i) {
          ptemp[counter] = (CONDITIONCODE) == condition;
          counter++;
        }
        
        out[j] = temp;
        
      }
    }
    return out;
  }
  else if(indexform == 1) {
    SEXP out = PROTECT(Rf_allocVector(REALSXP, count_total));
    double *pout = REAL(out);
    R_xlen_t counter = 0;
    
    for(R_xlen_t i = first_total; i <= last_total; ++i) {
      if((CONDITIONCODE) == condition) {
        pout[counter] = i;
        counter++;
      }
    }
    
    UNPROTECT(1);
    return out;
  }
  else if(indexform == -1) {
    SEXP out = PROTECT(Rf_allocVector(REALSXP, n - count_total));
    double *pout = REAL(out);
    R_xlen_t counter = 0;
    
    for(R_xlen_t i = 0; i < n; ++i) {
      if((CONDITIONCODE) != condition) {
        pout[counter] = i;
        counter++;
      }
    }
    
    UNPROTECT(1);
    return out;
  }
  else {
    stop(\"unknown indexform given\");
  }

} while(0)
"

macro_stridev_pool <- convert_macro(macro_stridev_pool)



# atomic types ====

macro_stridev_raw <- "
#define MACRO_STRIDEV_RAW(MACROCODE) do { \\
  const Rbyte *py = RAW_RO(y);  \\
  const Rbyte pv = RAW_RO(v)[0];  \\
  if(LogicalVector::is_na(na[0])) { \\
    stop(\"NAs not defined for type `raw`\"); \\
  } \\
  else if(na[0]) {	\\
    MACROCODE(  \\
      (py[i] == pv) \\
    ); \\
  }	\\
  else if(!na[0]) {  \\
    MACROCODE(  \\
      (py[i] == pv)  \\
    );  \\
  }	\\
  else {	\\
    stop(\"improper combination of `v` and `na` given\");  \\
  }	\\
} while(0)
"

macro_stridev_logical <- "
#define MACRO_STRIDEV_LGL(MACROCODE) do { \\
  const int pv = LOGICAL_RO(v)[0]; \\
  const int *py = LOGICAL_RO(y);  \\
  if(LogicalVector::is_na(na[0])) { \\
    MACROCODE(  \\
      (py[i] == NA_LOGICAL)  \\
    );  \\
  } \\
  else if(na[0]) {	\\
    MACROCODE(  \\
      (py[i] == NA_LOGICAL || (py[i] == pv)) \\
    ); \\
  }	\\
  else if(!na[0]) {  \\
    MACROCODE(  \\
      (py[i] != NA_LOGICAL && (py[i] == pv))  \\
    );  \\
  }	\\
  else {	\\
    stop(\"improper combination of `v` and `na` given\");  \\
  }	\\
} while(0)
"


macro_stridev_integer <- "
#define MACRO_STRIDEV_INT(MACROCODE) do { \\
  const int *py = INTEGER_RO(y);  \\
  if(LogicalVector::is_na(na[0])) { \\
    MACROCODE(  \\
      (py[i] == NA_INTEGER)  \\
    );  \\
  } \\
  else if(na[0] && Rf_length(v) == 1) {	\\
    const double pv = REAL_RO(v)[0];  \\
    MACROCODE(  \\
      (py[i] == NA_INTEGER || (py[i] == pv)) \\
    ); \\
  }	\\
  else if(na[0] && Rf_length(v) == 2) {	\\
    const double *pv = REAL_RO(v);  \\
    MACROCODE(  \\
      (py[i] == NA_INTEGER || (py[i] >= pv[0] && py[i] <= pv[1]))  \\
    ); \\
  }	\\
  else if(!na[0] && Rf_length(v) == 1) {  \\
    const double pv = REAL_RO(v)[0];  \\
    MACROCODE(  \\
      (py[i] != NA_INTEGER && (py[i] == pv))  \\
    );  \\
  }	\\
  else if(!na[0] && Rf_length(v) == 2) { \\
    const double *pv = REAL_RO(v);  \\
    MACROCODE(  \\
      (py[i] != NA_INTEGER && (py[i] >= pv[0] && py[i] <= pv[1])) \\
    );  \\
  }	\\
  else {	\\
    stop(\"improper combination of `v` and `na` given\");  \\
  }	\\
} while(0)
"

macro_stridev_real <- "
#define MACRO_STRIDEV_REAL(MACROCODE) do {  \\
  const double *py = REAL_RO(y);  \\
  if(LogicalVector::is_na(na[0])) { \\
    MACROCODE(  \\
      (R_isnancpp(py[i])) \\
    );  \\
  } \\
  else if(na[0] && Rf_length(v) == 1) {	\\
    const double pv = REAL_RO(v)[0];  \\
    MACROCODE(  \\
      (R_isnancpp(py[i]) || (py[i] == pv))  \\
    ); \\
  }	\\
  else if(na[0] && Rf_length(v) == 2) {	\\
    const double *pv = REAL_RO(v);  \\
    MACROCODE(  \\
      (R_isnancpp(py[i]) || (py[i] >= pv[0] && py[i] <= pv[1])) \\
    ); \\
  }	\\
  else if(!na[0] && Rf_length(v) == 1) {  \\
    const double pv = REAL_RO(v)[0];  \\
    MACROCODE(  \\
      (!R_isnancpp(py[i]) && (py[i] == pv)) \\
    );  \\
  }	\\
  else if(!na[0] && Rf_length(v) == 2) { \\
    const double *pv = REAL_RO(v);  \\
    MACROCODE(  \\
      (!R_isnancpp(py[i]) && (py[i] >= pv[0] && py[i] <= pv[1]))  \\
    );  \\
  }	\\
  else {	\\
    stop(\"improper combination of `v` and `na` given\");  \\
  }	\\
} while(0)
"


macro_stridev_complex <- "
#define MACRO_STRIDEV_CPLX(MACROCODE) do {  \\
  const Rcomplex *py = COMPLEX_RO(y); \\
  if(LogicalVector::is_na(na[0])) { \\
    MACROCODE(  \\
      (R_isnancpp(py[i].r) || R_isnancpp(py[i].i))  \\
    );  \\
  } \\
  else if(na[0]) {	\\
    const Rcomplex pv = COMPLEX_RO(v)[0]; \\
    MACROCODE(  \\
      ((R_isnancpp(py[i].r) || R_isnancpp(py[i].i)) || (py[i].r == pv.r && py[i].i == pv.i)) \\
    ); \\
  }	\\
  else if(!na[0]) {  \\
    const Rcomplex pv = COMPLEX_RO(v)[0]; \\
    MACROCODE(  \\
      (!(R_isnancpp(py[i].r) || R_isnancpp(py[i].i)) && (py[i].r == pv.r && py[i].i == pv.i))  \\
    );  \\
  }	\\
  else {	\\
    stop(\"improper combination of `v` and `na` given\");  \\
  }	\\
} while(0)
"

macro_stridev_string <- "
#define MACRO_STRIDEV_STRING(MACROCODE) do {  \\
  const SEXP *py = STRING_PTR_RO(y);  \\
  const SEXP *pv = STRING_PTR_RO(v);  \\
  if(LogicalVector::is_na(na[0])) { \\
    MACROCODE(  \\
      (py[i] == NA_STRING)  \\
    );  \\
  } \\
  else if(na[0] && Rf_length(v) == 1) {	\\
    MACROCODE(  \\
      (py[i] == NA_STRING || (int)R_compute_identical(py[i], pv[0], 0)) \\
    ); \\
  }	\\
  else if(na[0] && Rf_length(v) > 1) {	\\
    MACROCODE(  \\
      (py[i] == NA_STRING || inline_count_stringmatches(py[i], v))  \\
    ); \\
  }	\\
  else if(!na[0] && Rf_length(v) == 1) {  \\
    MACROCODE(  \\
      (py[i] != NA_STRING && (int)R_compute_identical(py[i], pv[0], 0))  \\
    );  \\
  }	\\
  else if(!na[0] && Rf_length(v) > 1) { \\
    MACROCODE(  \\
      (py[i] != NA_STRING && inline_count_stringmatches(py[i], v)) \\
    );  \\
  }	\\
  else {	\\
    stop(\"improper combination of `v` and `na` given\");  \\
  }	\\
} while(0)
"

# typeswitch ====

macro_stridev_typeswitch <- "
#define MACRO_STRIDEV_TYPESWITCH(MACROCODE) do {
  switch(TYPEOF(y)) {
    case RAWSXP:
    {
      MACRO_STRIDEV_RAW(MACROCODE);
      break;
    }
    case LGLSXP:
    {
      MACRO_STRIDEV_LGL(MACROCODE);
      break;
    }
    case INTSXP:
    {
      MACRO_STRIDEV_INT(MACROCODE);
      break;
    }
    case REALSXP:
    {
      MACRO_STRIDEV_REAL(MACROCODE);
      break;
    }
    case CPLXSXP:
    {
      MACRO_STRIDEV_CPLX(MACROCODE);
      break;
    }
    case STRSXP:
    {
      MACRO_STRIDEV_STRING(MACROCODE);
      break;
    }
    default:
    {
      stop(\"Unsupported type \");
    }
  }
} while(0)
"
macro_stridev_typeswitch <- convert_macro(macro_stridev_typeswitch)



# combine ====

macros <- stringi::stri_c(
  inlines,
  macro_stridev_prep,
  macro_stridev_pool,
  macro_stridev_raw,
  macro_stridev_logical,
  macro_stridev_integer,
  macro_stridev_real,
  macro_stridev_complex,
  macro_stridev_string,
  macro_stridev_typeswitch,
  collapse = "\n"
  
)

Rcpp::sourceCpp(code = stri_c(header, macros))


readr::write_file(macros, "macros_stridev.txt")

