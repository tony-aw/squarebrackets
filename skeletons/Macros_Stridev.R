
library(stringi)

convert_macro <- function(x) {
  x <- stri_split(x, regex = "\\n")[[1L]]
  ind <- 2:(length(x) - 2L)
  x[ind] <- stringi::stri_c(x[ind], "\t\\")
  x <- stringi::stri_c(x, collapse = "\n")
  return(x)
}


n <- 1e8
count <- ceiling(n/16)
rawsize <- n
count <- n - count
numsize <- (n - count) * 8
rawsize; numsize


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

# bits ====

macro_write_bits <- "
#define MACRO_STRIDEV_BITS_WRITE(CONDITIONCODE) do {
    R_xlen_t _strv_n = endpos - startpos + 1; 
    R_len_t _strv_num_ints = _strv_n / 32;
    
    IntegerVector b32(_strv_num_ints + 1); 
    unsigned int* _strv_pb32 = (unsigned int*)INTEGER(b32); 
    
    R_xlen_t i = startpos; 
    
    /* MAIN LOOP */ 
    for (R_xlen_t _strv_int_idx = 0; _strv_int_idx < _strv_num_ints; ++_strv_int_idx) { 
        
        
        unsigned int _strv_reg = 0; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 0);  i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 1);  i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 2);  i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 3);  i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 4);  i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 5);  i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 6);  i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 7);  i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 8);  i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 9);  i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 10); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 11); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 12); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 13); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 14); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 15); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 16); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 17); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 18); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 19); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 20); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 21); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 22); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 23); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 24); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 25); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 26); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 27); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 28); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 29); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 30); i++; 
        _strv_reg |= ((unsigned int)((CONDITIONCODE)==condition) << 31); i++; 
        
        
        _strv_pb32[_strv_int_idx] = _strv_reg; 
    } 
    
    /* TAIL HANDLER */ 
    R_xlen_t _strv_rem = _strv_n % 32; 
    if (_strv_rem > 0) { 
        unsigned int _strv_reg = 0; 
        for (int _strv_b = 0; _strv_b < _strv_rem; ++_strv_b) { 
            if (((CONDITIONCODE)==condition) == 1) { 
                _strv_reg |= ((unsigned int)1 << _strv_b); 
            } 
            i++; 
        } 
        _strv_pb32[_strv_num_ints] = _strv_reg; 
    } 
    return b32; 
} while(0)
"
macro_write_bits <- convert_macro(macro_write_bits)

macro_read_bits <- "
#define MACRO_STRIDEV_BITS_TRANSFER(DOCODE, STARTPOS, ENDPOS) do { 
    R_xlen_t _strv_n = (ENDPOS) - (STARTPOS) + 1; 
    int* _strv_native_ptr = INTEGER(b32); 
    unsigned int* _strv_pb32 = (unsigned int*)_strv_native_ptr; 
    
    R_xlen_t _strv_num_ints = _strv_n / 32; 
    R_xlen_t i = (STARTPOS); 
    
    /* MAIN LOOP */ 
    for (R_xlen_t _strv_int_idx = 0; _strv_int_idx < _strv_num_ints; ++_strv_int_idx) { 
        unsigned int _strv_current_int = _strv_pb32[_strv_int_idx]; 
        int _strv_bval; 
        
        _strv_bval = (_strv_current_int >> 0)  & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 1)  & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 2)  & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 3)  & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 4)  & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 5)  & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 6)  & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 7)  & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 8)  & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 9)  & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 10) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 11) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 12) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 13) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 14) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 15) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 16) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 17) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 18) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 19) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 20) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 21) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 22) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 23) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 24) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 25) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 26) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 27) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 28) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 29) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 30) & 1; if (_strv_bval) { DOCODE; } i++; 
        _strv_bval = (_strv_current_int >> 31) & 1; if (_strv_bval) { DOCODE; } i++; 
    } 
    
    /* TAIL HANDLER */ 
    R_xlen_t _strv_rem = _strv_n % 32; 
    if (_strv_rem > 0) { 
        unsigned int _strv_current_int = _strv_pb32[_strv_num_ints]; 
        int _strv_bval; 
        for (int _strv_b = 0; _strv_b < _strv_rem; ++_strv_b) { 
            _strv_bval = (_strv_current_int >> _strv_b) & 1; 
            if (_strv_bval) { 
                { DOCODE; } 
            } 
            i++;
        } 
    } 
} while(0)
"

macro_read_bits <- convert_macro(macro_read_bits)


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
      out[j] = rcpp_stridev_bits_write(y, v, condition, na, startpos, endpos);
    }
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
  macro_write_bits,
  macro_read_bits,
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
