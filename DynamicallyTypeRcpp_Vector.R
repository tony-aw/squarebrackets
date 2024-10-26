# set-up ====

library(stringi)

SXPTYPES <- c("LGLSXP", "INTSXP", "REALSXP", "CPLXSXP", "STRSXP", "RAWSXP")
RCPPTYPES <- c("Logical", "Integer", "Numeric", "Complex", "Character", "Raw")

header <- "

#include <Rcpp.h>

using namespace Rcpp;

"


################################################################################
# set all ====



templatecode <- "

template<int RTYPE> void rcpp_set_all_template(Vector<RTYPE> x, Vector<RTYPE> rp) {
  R_xlen_t n = x.length();

  if(rp.length() == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[i] = rp[i];
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i){
      x[i] = rp[0];
    }
  }
  else stop(\"recycling not allowed\");

}


"


switchpiece <- "
  case TYPESXP:
  {
    rcpp_set_all_template<TYPESXP>(as<RCPPTYPEVector>(x), as<RCPPTYPEVector>(rp));
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



code <- stri_c(
  header,
  
  templatecode,
  
  
  "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_all_atomic)]]
void rcpp_set_all_atomic(
  SEXP x, const SEXP rp
) {


switch(TYPEOF(x)){
",
  
  
  switches,
  
  
  "
}
}
"
)

cat(code)



Rcpp::sourceCpp(code = code)


################################################################################

# set vind 32 bit ====

templatecode <- "

template<int RTYPE> void rcpp_set_vind_32_template(
  Vector<RTYPE> x, const SEXP ind, const Vector<RTYPE> rp
) {
  R_xlen_t n = Rf_xlength(ind);
  
  const int *pind = INTEGER(ind);
  
  if(rp.length() == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[pind[i]] = rp[i];
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[pind[i]] = rp[0];
    }
  }
  else stop(\"recycling not allowed\");
}


"


switchpiece <- "
  case TYPESXP:
  {
    rcpp_set_vind_32_template<TYPESXP>(as<RCPPTYPEVector>(x), ind, as<RCPPTYPEVector>(rp));
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



code <- stri_c(
  header,
  
  templatecode,
  
  
  "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_32_atomic)]]
void rcpp_set_vind_32_atomic(
  SEXP x, const SEXP ind, const SEXP rp
) {


switch(TYPEOF(x)){
",
  
  
  switches,
  
  
  "
}
}
"
)

cat(code)



Rcpp::sourceCpp(code = code)



################################################################################

# set vind 64 bit ====


templatecode <- "

template<int RTYPE> void rcpp_set_vind_64_template(
  Vector<RTYPE> x, const SEXP ind, const Vector<RTYPE> rp
) {
  R_xlen_t n = Rf_xlength(ind);
  
  const double *pind = REAL(ind);
  
  if(rp.length() == n) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[pind[i]] = rp[i];
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[pind[i]] = rp[0];
    }
  }
  else stop(\"recycling not allowed\");
}


"


switchpiece <- "
  case TYPESXP:
  {
    rcpp_set_vind_64_template<TYPESXP>(as<RCPPTYPEVector>(x), ind, as<RCPPTYPEVector>(rp));
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


code <- stri_c(
  header,
  
  templatecode,
  
  
  "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_64_atomic)]]
void rcpp_set_vind_64_atomic(
  SEXP x, const SEXP ind, const SEXP rp
) {


switch(TYPEOF(x)){
",
  
  
  switches,
  
  
  "
}
}
"
)

cat(code)



Rcpp::sourceCpp(code = code)



################################################################################

# setrv ====

rtypes <- c("Logical", "Integer", "Numeric", "Character", "Raw")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_safe_RTYPE)]]
void rcpp_setrv_safe_RTYPE(RTYPEVector x, RTYPEVector v, RTYPEVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!RTYPEVector::is_na(x[i])) {
        if(x[i] == v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!RTYPEVector::is_na(x[i])) {
        if(x[i] != v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_fast_RTYPE)]]
void rcpp_setrv_fast_RTYPE(RTYPEVector x, RTYPEVector v, RTYPEVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] == v[0]) {
        x[i] = rp[0];
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] != v[0]) {
          x[i] = rp[0];
      }
    }
  }
  
}

"


rcpp_scripts <- character(length(rtypes))

for(i in seq_along(rtypes)) {
  rcpp_scripts[i] <- stri_replace_all(
    templatecode,
    fixed = "RTYPE",
    replacement = rtypes[i],
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
}


headers <- "

#include <Rcpp.h>

using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_safe_Complex)]]
void rcpp_setrv_safe_Complex(ComplexVector x, ComplexVector v, ComplexVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!ComplexVector::is_na(x[i])) {
        if(x[i] == v[0]) {
          x[i] = rp[0];
        }
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!ComplexVector::is_na(x[i])) {
        if(!(x[i] == v[0])) {
          x[i] = rp[0];
        }
      }
    }
  }
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setrv_fast_Complex)]]
void rcpp_setrv_fast_Complex(ComplexVector x, ComplexVector v, ComplexVector rp, bool invert) {
  R_xlen_t n = x.length();
  
  if(!invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(x[i] == v[0]) {
        x[i] = rp[0];
      }
    }
  }
  if(invert) {
    for(R_xlen_t i = 0; i < n; ++i) {
      if(!(x[i] == v[0])) {
          x[i] = rp[0];
      }
    }
  }
  
}



"
rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)


Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)


fileConn <- file("src/dynamic_rcpp_setrv.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)


