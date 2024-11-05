# set-up ====

library(stringi)


SXPTYPES <- c("LGLSXP", "INTSXP", "REALSXP", "CPLXSXP", "STRSXP", "RAWSXP")
RCPPTYPES <- c("Logical", "Integer", "Numeric", "Complex", "Character", "Raw")

header <- "

#include <Rcpp.h>

using namespace Rcpp;

"

# templates ====

templatecode <- "

template<int RTYPE> Vector<RTYPE> rcpp_slice_x_template(
    const Vector<RTYPE> x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  Vector<RTYPE> out(len);
  
  if(len == 1) {
    out[0] = x[start];
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      out[counter] = x[i];
      counter++;
    }
  }
  return out;
}
  


template<int RTYPE> void rcpp_slice_set_template(
    Vector<RTYPE> x, const Vector<RTYPE> rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  if(len == 1) {
    x[start] = rp[0];
  }
  else if(rp.length() == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i <= end; i += by) {
      x[i] = rp[counter];
      counter++;
    }
  }
  else if(rp.length() == 1) {
    // Comment
    for(R_xlen_t i = start; i <= end; i += by) {
      x[i] = rp[0];
    }
  }
  else {
    stop(\"recycling not allowed\");
  }
}


template<int RTYPE> Vector<RTYPE> rcpp_slice_xrev_template(
    const Vector<RTYPE> x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  Vector<RTYPE> out(len);
  
  if(len == 1) {
    out[0] = x[start];
    return out;
  }
  else {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      out[counter] = x[i];
      counter++;
    }
  }
  return out;
}


template<int RTYPE> void rcpp_slice_setrev_template(
    Vector<RTYPE> x, const Vector<RTYPE> rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  if(len == 1) {
    x[start] = rp[0];
  }
  else if(rp.length() == len) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = start; i >= end; i -= by) {
      x[i] = rp[counter];
      counter++;
    }
  }
  else if(rp.length() == 1) {
    // Comment
    for(R_xlen_t i = start; i >= end; i -= by) {
      x[i] = rp[0];
    }
  }
  else {
    stop(\"recylcing not allowed\");
  }
}


template<int RTYPE> Vector<RTYPE> rcpp_slice_rm_template(
    const Vector<RTYPE> x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  Vector<RTYPE> out(len);
  
  R_xlen_t counter = 0;
  if(start > 0) {
    // Comment
    for(R_xlen_t i = 0; i < start; ++i) {
      out[i] = x[i];
    }
    counter = start;
  }
  
  if(by == 2) {
    for(R_xlen_t i = start; i < end; i+=2) {
      out[counter] = x[i+1];
      counter++;
    }
  }
  
  if(by > 2) {
    for(R_xlen_t i = start; i < end; i += by) {
      R_xlen_t startx = i + 1;
      for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
        out[counter] = x[j];
        counter++;
      }
    }
  }
  
  if(end < (x.length() - 1)) {
    // Comment
    for(R_xlen_t i = end + 1; i < x.length(); ++i) {
      out[i - (end + 1) + counter] = x[i];
    }
  }
  return out;
}



template<int RTYPE> void rcpp_slice_setinv_template(
    Vector<RTYPE> x, const Vector<RTYPE> rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  
  
  if(rp.length() == len) {
    R_xlen_t counter = 0;
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        x[i] = rp[i];
      }
      counter = start;
    }
    
    if(by == 2) {
      for(R_xlen_t i = start; i < end; i+=2) {
        x[i+1] = rp[counter];
        counter++;
      }
    }
    
    if(by > 2) {
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          x[j] = rp[counter];
          counter++;
        }
      }
    }
    
    if(end < (x.length() - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < x.length(); ++i) {
        x[i] = rp[i - (end + 1) + counter];
      }
    }
  }
  else if(rp.length() == 1) {
    if(start > 0) {
      // Comment
      for(R_xlen_t i = 0; i < start; ++i) {
        x[i] = rp[0];
      }
    }
    
    if(by == 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i+=2) {
        x[i+1] = rp[0];
      }
    }
    
    if(by > 2) {
      // Comment
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          x[j] = rp[0];
        }
      }
    }
    
    if(end < (x.length() - 1)) {
      // Comment
      for(R_xlen_t i = end + 1; i < x.length(); ++i) {
        x[i] = rp[0];
      }
    }
  }
  else {
    stop(\"recycling not allowed\");
  }
}

"
cat(templatecode)


################################################################################
# atomic switches ====




make_switches <- function(template_return, template_name, template_inputs) {
  
  switchpiece <- "
  case TYPESXP:
  {
    <return> template_name<TYPESXP>(template_inputs);
    break;
  }
"
  switchpiece <- stri_replace_all_fixed(
    switchpiece, c("<return>", "template_name", "template_inputs"),
    c(template_return, template_name, template_inputs), vectorize_all = FALSE
  )
  
  switches <- character(6L)
  
  for(i in 1:6) {
    switches[i] <- stringi::stri_replace_all_fixed(
      switchpiece, c("TYPESXP", "RCPPTYPE"),  c(SXPTYPES[i], RCPPTYPES[i]),
      vectorize_all = FALSE
    )
  }
  
  switches <- stringi::stri_paste(switches, collapse = "\n")
  return(switches)
}

template_names <- c(
  "rcpp_slice_x_template",
  "rcpp_slice_set_template",
  "rcpp_slice_xrev_template",
  "rcpp_slice_setrev_template",
  "rcpp_slice_rm_template",
  "rcpp_slice_setinv_template"
)


template_inputs <- rep(c(
  "as<RCPPTYPEVector>(x), start, end, by, len",
  "as<RCPPTYPEVector>(x), as<RCPPTYPEVector>(rp), start, end, by, len"
), 3L)

template_returns <- rep(c(
  "return", ""
), 3L)

atomic_fun_args <- rep(c(
  "const SEXP x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len",
  "SEXP x, const SEXP rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len"
), 3L)


atomic_fun_names <- stri_replace_all_fixed(
  template_names, "_template", "_atomic"
)

atomic_fun_returntypes <- rep(c(
  "SEXP", "void"
), 3L)

atomic_fun_returnoutes <- rep(c(
  "return R_NilValue;", ""
), 3L)

atomic_codes <- character(6)

for(i in 1:6) {
  
  temp <- stri_c(
    
    "
//' @keywords internal
//' @noRd
// [[Rcpp::export(.<ATOMIC_FUN_NAME>)]]
<returntype> <ATOMIC_FUN_NAME>(
  <ATOMIC_FUN_ARGS>
) {


switch(TYPEOF(x)){
",
    
    
    make_switches(template_returns[i], template_names[i], template_inputs[i]),
    
    
    "
  default: stop(\"unsupported type given\");
}

<returnout>

}
"
  )
  
  temp <- stri_replace_all_fixed(
    temp,
    c("<returntype>", "<ATOMIC_FUN_NAME>", "<ATOMIC_FUN_ARGS>", "<returnout>"),
    c(atomic_fun_returntypes[i], atomic_fun_names[i], atomic_fun_args[i], atomic_fun_returnoutes[i]),
    vectorise_all = FALSE
  )
  
  atomic_codes[i] <- temp
  
}

atomic_code <- stri_paste(atomic_codes, collapse = "\n \n")

cat(atomic_code)


################################################################################
# combining code ====


final_code <- stri_paste(
  templatecode,
  atomic_code,
  collapse = "\n \n"
)
cat(final_code)



headers <- "

#include <Rcpp.h>

using namespace Rcpp;



"
rcpp_code <- paste(c(headers, final_code), collapse = "\n\n\n")
cat(rcpp_code)

Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)

fileConn <- file("src/dynamic_rcpp_slice.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)

