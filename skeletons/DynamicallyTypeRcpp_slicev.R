# set-up ====

library(stringi)
SXPTYPES <- c("LGLSXP", "INTSXP", "REALSXP", "CPLXSXP", "STRSXP", "RAWSXP")
RCPPTYPES <- c("Logical", "Integer", "Numeric", "Complex", "Character", "Raw")

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


"


code_do_something_narm <- "

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    const int pv = LOGICAL(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(py[i] != NA_LOGICAL && (py[i] == pv) == condition) {
        <do_something1>
        <do_something2>
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
        <do_something1>
        <do_something2>
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
        <do_something1>
        <do_something2>
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
          <do_something1>
          <do_something2>
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] != NA_INTEGER && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          <do_something1>
          <do_something2>
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
          <do_something1>
          <do_something2>
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(!R_isnancpp(py[i]) && (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          <do_something1>
          <do_something2>
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
            <do_something1>
            <do_something2>
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == pv[0]) {
            <do_something1>
            <do_something2>
          }
        }
      }
    }
    if(Rf_length(v) > 1) {
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] != NA_STRING) {
            if(rcpp_count_stringmatches(py[i], v) == 0) {
              <do_something1>
              <do_something2>
            }
            
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(rcpp_count_stringmatches(py[i], v) > 0) {
            <do_something1>
            <do_something2>
          }
        }
      }
    }
    
    break;
  }
  default: stop(\"Unsupported type \");
}

"
cat(code_do_something_narm)



code_do_something_nakeep <- "

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    const int pv = LOGICAL(v)[0];
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if(py[i] == NA_LOGICAL || (py[i] == pv) == condition) {
        <do_something1>
        <do_something2>
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
        <do_something1>
        <do_something2>
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
        <do_something1>
        <do_something2>
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
          <do_something1>
          <do_something2>
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(py[i] == NA_INTEGER || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          <do_something1>
          <do_something2>
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
          <do_something1>
          <do_something2>
        }
      }
    }
    else if(Rf_xlength(v) == 2) {
      double *pv = REAL(v);
      for(R_xlen_t i = start; i != (end + by); i += by) {
        if(R_isnancpp(py[i]) || (py[i] >= pv[0] && py[i] <= pv[1]) == condition) {
          <do_something1>
          <do_something2>
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
            <do_something1>
            <do_something2>
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || py[i] == pv[0]) {
            <do_something1>
            <do_something2>
          }
        }
      }
    }
    if(Rf_length(v) > 1) {
      if(invert[0]) {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) == 0) {
            <do_something1>
            <do_something2>
          }
        }
      }
      else {
        for(R_xlen_t i = start; i != (end + by); i += by) {
          if(py[i] == NA_STRING || rcpp_count_stringmatches(py[i], v) > 0) {
            <do_something1>
            <do_something2>
          }
        }
      }
    }
    
    break;
  }
  default: stop(\"Unsupported type \");
}

"
cat(code_do_something_nakeep)




code_do_something_naonly <- "

switch(TYPEOF(y)) {
  
  case LGLSXP:
  {
    bool condition = !invert[0];
    const int *py = LOGICAL(y);
    for(R_xlen_t i = start; i != (end + by); i += by) {
      if((py[i] == NA_LOGICAL) == condition) {
        <do_something1>
        <do_something2>
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
        <do_something1>
        <do_something2>
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
        <do_something1>
        <do_something2>
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
        <do_something1>
        <do_something2>
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
        <do_something1>
        <do_something2>
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

"
cat(code_do_something_naonly)


make_do_na_code <- function(do_something1, do_something2) {
  code_do_something_narm <- stringi::stri_replace_all_fixed(
    code_do_something_narm,
    c("<do_something1>", "<do_something2>"),
    c(do_something1, do_something2),
    vectorize_all = FALSE
  )
  
  code_do_something_nakeep <- stringi::stri_replace_all_fixed(
    code_do_something_nakeep,
    c("<do_something1>", "<do_something2>"),
    c(do_something1, do_something2),
    vectorize_all = FALSE
  )
  
  code_do_something_naonly <- stringi::stri_replace_all_fixed(
    code_do_something_naonly,
    c("<do_something1>", "<do_something2>"),
    c(do_something1, do_something2),
    vectorize_all = FALSE
  )
  
  code <- stringi::stri_paste("
  if(LogicalVector::is_na(na[0])) {
    ",
    code_do_something_naonly,
    
    "
  }
  
  else if(na[0]) {
    
    ",
  code_do_something_nakeep,
  
  "
  }
  else if(!na[0]) {
    ",
  
  code_do_something_narm,
  
  "
    
  }
  "
  )
  return(code)
  
}


################################################################################
# countv & whichv ====

code_do_count <- make_do_na_code("count++;", "")
cat(code_do_count)

code_countv <- stringi::stri_paste(
  
  "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_countv)]]
R_xlen_t rcpp_countv(
    SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    if(len == 0) {
      return count;
    }
    
    ",
    code_do_count,
    
  
  "
  
    return count;
  }
  "
)

cat(code_countv)

code <- paste0(header, "\n", code_countv)
Rcpp::sourceCpp(code = code)


code_do_whichv <- make_do_na_code("pout[count] = i + 1;", "count++;")
cat(code_do_whichv)

code_whichv <- stringi::stri_paste(
"
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_whichv_32)]]
IntegerVector rcpp_whichv_32(
    SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    
   
    R_xlen_t amount = rcpp_countv(y, v, na, invert, start, end, by, len);
    int *pout;
    SEXP out = PROTECT(Rf_allocVector(INTSXP, amount));
    pout = INTEGER(out);
    
    if(amount == 0) {
      UNPROTECT(1);
      return out;
    }
    
    ",
  code_do_whichv,

"


  
  UNPROTECT(1);
    return out;
  }
  
  "
  
)

cat(code_whichv)

code <- paste0(header, code_countv, code_whichv, collapse = "\n\n")
cat(code)

Rcpp::sourceCpp(code = code)


################################################################################
# slicev_x ====

code_do_x <- make_do_na_code("out[count] = x[i];", "count++;")
cat(code_do_x)

templatecode <- stringi::stri_paste(
  "

template<int RTYPE> Vector<RTYPE> rcpp_slicev_x_template(
    Vector<RTYPE> x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
  ) {
    R_xlen_t count = 0;
    
    if(Rf_xlength(x) != Rf_xlength(y)) {
      stop(\"`x` and `y` must have equal lengths\");
    }
    
    R_xlen_t size = rcpp_countv(y, v, na, invert, start, end, by, len);
    Vector<RTYPE> out(size);
    
    if(size == 0) {
      return out;
    }
    
  ",
  code_do_x,
  
  "
  
    return out;
  }
  
  "
  
)

cat(templatecode)




switchpiece <- "
  case TYPESXP:
  {
    return rcpp_slicev_x_template<TYPESXP>(as<RCPPTYPEVector>(x), y, v, na, invert, start, end, by, len);
  }
"

switches <- character(6L)

for(i in 1:6) {
  switches[i] <- stringi::stri_replace_all_fixed(
    switchpiece, c("TYPESXP", "RCPPTYPE"),  c(SXPTYPES[i], RCPPTYPES[i]),
    vectorize_all = FALSE
  )
}

switches <- stringi::stri_paste(switches, collapse = "\n")
cat(switches)





code_slicev_x <- stri_c(
  
  templatecode,
  
  
  "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slicev_x_atomic)]]
SEXP rcpp_slicev_x_atomic(
  SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len
) {



switch(TYPEOF(x)){
",
  
  
  switches,
  
  
  "
  default: stop(\"unsupported type\");
}
  return R_NilValue;
}
"
)

cat(code_slicev_x)


code <- stri_paste(header, code_countv, code_slicev_x)
cat(code)
Rcpp::sourceCpp(code = code)




################################################################################
# slicev_set ====

code_do_set1 <- make_do_na_code("x[i] = rp[0];", "")
cat(code_do_set1)
code_do_set0 <- make_do_na_code("x[i] = rp[count];", "count++;")
cat(code_do_set0)

templatecode <- stringi::stri_paste(
  "

template<int RTYPE> void rcpp_slicev_set_template(
    Vector<RTYPE> x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len, Vector<RTYPE> rp
  ) {
  
  if(Rf_xlength(x) != Rf_xlength(y)) {
      stop(\"`x` and `y` must have equal lengths\");
    }
  
  if(Rf_xlength(rp) == 1 && len > 0) {
    ",
  code_do_set1,
  "
  }
  else if(Rf_xlength(rp) > 1 && len > 0) {
    R_xlen_t count = 0;
    ",
  code_do_set0,
  "
  }
   
  ",
  
  
  "
  }
  
  "
  
)

cat(templatecode)




switchpiece <- "
  case TYPESXP:
  {
    rcpp_slicev_set_template<TYPESXP>(as<RCPPTYPEVector>(x), y, v, na, invert, start, end, by, len, as<RCPPTYPEVector>(rp));
    break;
  }
"

switches <- character(6L)

for(i in 1:6) {
  switches[i] <- stringi::stri_replace_all_fixed(
    switchpiece, c("TYPESXP", "RCPPTYPE"),  c(SXPTYPES[i], RCPPTYPES[i]),
    vectorize_all = FALSE
  )
}

switches <- stringi::stri_paste(switches, collapse = "\n")
cat(switches)





code_slicev_set <- stri_c(
  
  templatecode,
  
  
  "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slicev_set_atomic)]]
void rcpp_slicev_set_atomic(
  SEXP x, SEXP y, SEXP v, LogicalVector na, LogicalVector invert, R_xlen_t start, R_xlen_t end, R_xlen_t by, R_xlen_t len, SEXP rp
) {



switch(TYPEOF(x)){
",
  
  
  switches,
  
  
  "
  default: stop(\"unsupported type\");
}
}
"
)

cat(code_slicev_set)


code <- stri_paste(header, code_countv, code_slicev_set)
cat(code)
Rcpp::sourceCpp(code = code)



################################################################################
# checks ====


code <- stri_paste(
  
  header,
  code_countv,
  code_whichv,
  code_slicev_x,
  code_slicev_set,
  collapse = "\n\n"
  
)
cat(code)

Rcpp::sourceCpp(code = code)



################################################################################
# write script ====


fileConn <- file("src/dynamic_rcpp_slicev.cpp")
writeLines(code, fileConn)
close(fileConn)



