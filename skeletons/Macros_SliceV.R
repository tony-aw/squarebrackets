# set-up ====

source("source.R")

library(stringi)



header <- "

#include <Rcpp.h>
using namespace Rcpp;


inline int rcpp_count_stringmatches(String y, SEXP v) {
  int n = Rf_length(v);
  const SEXP *pv = STRING_PTR_RO(v);
  int count = 0;
  for(int i = 0; i < n; ++i) {
    if(y == pv[i]) {
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
# MACRO DO NA REMOVE ====
#

macro_slicev_do_narm <- "
#define MACRO_SLICEV_DO_NARM(DOCODE) do {
  
  switch(TYPEOF(y)) {
    
    case LGLSXP:
    {
      bool condition = !invert[0];
      const int *py = LOGICAL(y);
      const int pv = LOGICAL(v)[0];
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] != NA_LOGICAL && (py[i] == pv) == condition) {
          DOCODE;
        }
      }
      break;
    }
    case CPLXSXP:
    {
      bool condition = !invert[0];
      const Rcomplex *py = COMPLEX(y);
      const Rcomplex pv = COMPLEX(v)[0];
      bool checkNA;
      for(R_xlen_t i = start; i != (end + by); i += by) {
        checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);
        if(!checkNA && (py[i].r == pv.r && py[i].i == pv.i) == condition) {
          DOCODE;
        }
      }
      break;
    }
    case RAWSXP :
    {
      bool condition = !invert[0];
      const Rbyte *py = RAW(y);
      const Rbyte pv = RAW(v)[0];
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if((py[i] == pv) == condition) {
          DOCODE;
        }
      }
      break;
    }
    case INTSXP:
    {
      bool condition = !invert[0];
      const int *py = INTEGER(y);
      if(Rf_xlength(v) == 1) {
        int pv;
        pv = Rf_asInteger(v);
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_INTEGER && (py[i] == pv) == condition) {
            DOCODE;
          }
        }
      }
      else if(Rf_xlength(v) == 2) {
        double *pv = REAL(v);
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_INTEGER && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
            DOCODE;
          }
        }
      }
      else {
        stop(\"improper length for `v`\");
      }
      break;
    }
    case REALSXP:
    {
      bool condition = !invert[0];
      const double *py = REAL(y);
      if(Rf_xlength(v) == 1) {
        double pv;
        pv = Rf_asReal(v);
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(!R_isnancpp(py[i]) && (py[i] == pv) == condition) {
            DOCODE;
          }
        }
      }
      else if(Rf_xlength(v) == 2) {
        double *pv = REAL(v);
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(!R_isnancpp(py[i]) && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
            DOCODE;
          }
        }
      }
      else {
        stop(\"improper length for `v`\");
      }
      
      break;
    }
    case STRSXP:
    {
      const SEXP *py = STRING_PTR_RO(y);
      if(Rf_length(v) == 1) {
        const SEXP *pv = STRING_PTR_RO(v);
        if(invert[0]) {
          for(R_xlen_t i = start; i != (end + by); i += by) {
            if(py[i] != NA_STRING && py[i] != pv[0]) {
              DOCODE;
            }
          }
        }
        else {
          for(R_xlen_t i = start; i != (end + by); i += by) {
            if(py[i] == pv[0]) {
              DOCODE;
            }
          }
        }
      }
      if(Rf_length(v) > 1) {
        if(invert[0]) {
          for(R_xlen_t i = start; i != (end + by); i += by) {
            if(py[i] != NA_STRING) {
              if(rcpp_count_stringmatches(py[i], v) == 0) {
                DOCODE;
              }
              
            }
          }
        }
        else {
          for(R_xlen_t i = start; i != (end + by); i += by) {
            if(rcpp_count_stringmatches(py[i], v) > 0) {
              DOCODE;
            }
          }
        }
      }
      
      break;
    }
    default: stop(\"Unsupported type \");
  }
} while(0)
"
macro_slicev_do_narm <- convert_macro(macro_slicev_do_narm)
cat(macro_slicev_do_narm)




################################################################################
# MACRO DO NA KEEP ====
#


macro_slicev_do_nakeep <- "
#define MACRO_SLICEV_DO_NAKEEP(DOCODE) do {
  switch(TYPEOF(y)) {
    
    case LGLSXP:
    {
      bool condition = !invert[0];
      const int *py = LOGICAL(y);
      const int pv = LOGICAL(v)[0];
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] == NA_LOGICAL || (py[i] == pv) == condition) {
          DOCODE;
          
        }
      }
      break;
    }
    case CPLXSXP:
    {
      bool condition = !invert[0];
      const Rcomplex *py = COMPLEX(y);
      const Rcomplex pv = COMPLEX(v)[0];
      bool checkNA;
      for(R_xlen_t i = start; i != (end + by); i += by) {
        checkNA = R_isnancpp(py[i].r) || R_isnancpp(py[i].i);
        if(checkNA || (py[i].r == pv.r && py[i].i == pv.i) == condition) {
          DOCODE;
          
        }
      }
      break;
    }
    case RAWSXP :
    {
      bool condition = !invert[0];
      const Rbyte *py = RAW(y);
      const Rbyte pv = RAW(v)[0];
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if((py[i] == pv) == condition) {
          DOCODE;
          
        }
      }
      break;
    }
    case INTSXP:
    {
      bool condition = !invert[0];
      const int *py = INTEGER(y);
      if(Rf_xlength(v) == 1) {
        int pv;
        pv = Rf_asInteger(v);
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_INTEGER || (py[i] == pv) == condition) {
            DOCODE;
            
          }
        }
      }
      else if(Rf_xlength(v) == 2) {
        double *pv = REAL(v);
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_INTEGER || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
            DOCODE;
            
          }
        }
      }
      else {
        stop(\"improper length for `v`\");
      }
      break;
    }
    case REALSXP:
    {
      bool condition = !invert[0];
      const double *py = REAL(y);
      if(Rf_xlength(v) == 1) {
        double pv;
        pv = Rf_asReal(v);
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(R_isnancpp(py[i]) || (py[i] == pv) == condition) {
            DOCODE;
            
          }
        }
      }
      else if(Rf_xlength(v) == 2) {
        double *pv = REAL(v);
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(R_isnancpp(py[i]) || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
            DOCODE;
            
          }
        }
      }
      else {
        stop(\"improper length for `v`\");
      }
      
      break;
    }
    case STRSXP:
    {
      const SEXP *py = STRING_PTR_RO(y);
      if(Rf_length(v) == 1) {
        const SEXP *pv = STRING_PTR_RO(v);
        if(invert[0]) {
          for(R_xlen_t i = start; i != (end + by); i += by) {
            if(py[i] == NA_STRING || py[i] != pv[0]) {
              DOCODE;
              
            }
          }
        }
        else {
          for(R_xlen_t i = start; i != (end + by); i += by) {
            if(py[i] == NA_STRING || py[i] == pv[0]) {
              DOCODE;
              
            }
          }
        }
      }
      if(Rf_length(v) > 1) {
        if(invert[0]) {
          for(R_xlen_t i = start; i != (end + by); i += by) {
            if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) == 0) {
              DOCODE;
              
            }
          }
        }
        else {
          for(R_xlen_t i = start; i != (end + by); i += by) {
            if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) > 0) {
              DOCODE;
              
            }
          }
        }
      }
      
      break;
    }
    default: stop(\"Unsupported type \");
  }
} while(0)
"
macro_slicev_do_nakeep <- convert_macro(macro_slicev_do_nakeep)
cat(macro_slicev_do_nakeep)



################################################################################
# MACRO DO NA ONLY ====
#

macro_slicev_do_naonly <- "
#define MACRO_SLICEV_DO_NAONLY(DOCODE) do {
  switch(TYPEOF(y)) {
    
    case LGLSXP:
    {
      bool condition = !invert[0];
      const int *py = LOGICAL(y);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if((py[i] == NA_LOGICAL) == condition) {
          DOCODE;
          
        }
      }
      break;
    }
    case INTSXP:
    {
      bool condition = !invert[0];
      const int *py = INTEGER(y);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if((py[i] == NA_INTEGER) == condition) {
          DOCODE;
          
        }
      }
      break;
    }
    case REALSXP:
    {
      bool condition = !invert[0];
      const double *py = REAL(y);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(R_isnancpp(py[i]) == condition) {
          DOCODE;
          
        }
      }
      break;
    }
    case CPLXSXP:
    {
      const Rcomplex *py = COMPLEX(y);
      bool condition = !invert[0];
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if((R_isnancpp(py[i].r) || R_isnancpp(py[i].i)) == condition){
          DOCODE;
          
        }
      }
      break;
    }
    case STRSXP:
    {
      const SEXP *py = STRING_PTR_RO(y);
      bool condition = !invert[0];
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if((py[i] == NA_STRING) == condition) {
          DOCODE;
          
        }
      }
      break;
    }
    case RAWSXP :
    {
      stop(\"NAs not defined for type `raw`\");
    }
    default: stop(\"Unsupported type \");
  }
} while(0)
"
macro_slicev_do_naonly <- convert_macro(macro_slicev_do_naonly)
cat(macro_slicev_do_naonly)



################################################################################
# MACRO DO CALL ====
#

macro_slicev_do <- "
#define MACRO_SLICEV_DO(DOCODE) do {
  if(LogicalVector::is_na(na[0])) {
    MACRO_SLICEV_DO_NAONLY(DOCODE);
  }
  else if(na[0]) {
    MACRO_SLICEV_DO_NAKEEP(DOCODE);
  }
  else if(!na[0]) {
    MACRO_SLICEV_DO_NARM(DOCODE);
  }
  else {
    stop(\"unknow value for `na` given\");
  }
} while(0)
"
macro_slicev_do <- convert_macro(macro_slicev_do)
cat(macro_slicev_do)




################################################################################
# Write & test macros ====
#

macros <- stri_c(
  macro_slicev_do_narm,
  "\n",
  macro_slicev_do_nakeep,
  "\n",
  macro_slicev_do_naonly,
  "\n",
  macro_slicev_do,
  "\n",
  collapse = "\n"
)
cat(macros)


readr::write_file(macros, "macros_slicev.txt")


testfun <- "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.test)]]
int test(int x, int y) {
  return(x + y);
}

"

Rcpp::sourceCpp(code = stri_c(macros, testfun, collapse = "\n"))


