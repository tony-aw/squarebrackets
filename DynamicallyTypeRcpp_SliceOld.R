# set-up ====

library(stringi)


# slice ====

RTYPES <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_x_RTYPE)]]
RTYPEVector rcpp_slice_x_RTYPE(
    const RTYPEVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  R_xlen_t counter = 0;
  RTYPEVector out(len);
  
  if(len == 1) {
    out[0] = x[start];
    return out;
  }
  else {
    for(R_xlen_t i = start; i <= end; i += by) {
      out[counter] = x[i];
      counter++;
    }
  }
  return out;
}
  

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_set_RTYPE)]]
void rcpp_slice_set_RTYPE(
    RTYPEVector x, const RTYPEVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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
    for(R_xlen_t i = start; i <= end; i += by) {
      x[i] = rp[0];
    }
  }
  else {
    stop(\"recycling not allowed\");
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_xrev_RTYPE)]]
RTYPEVector rcpp_slice_xrev_RTYPE(
    const RTYPEVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  R_xlen_t counter = 0;
  RTYPEVector out(len);
  
  if(len == 1) {
    out[0] = x[start];
    return out;
  }
  else {
    for(R_xlen_t i = start; i >= end; i -= by) {
      out[counter] = x[i];
      counter++;
    }
  }
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_setrev_RTYPE)]]
void rcpp_range_slice_setrev_RTYPE(
    RTYPEVector x, const RTYPEVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
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
    for(R_xlen_t i = start; i >= end; i -= by) {
      x[i] = rp[0];
    }
  }
  else {
    stop(\"recylcing not allowed\");
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_rm_RTYPE)]]
RTYPEVector rcpp_slice_rm_RTYPE(
    const RTYPEVector x, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  R_xlen_t counter = 0;
  RTYPEVector out(len);
  
  if(start > 0) {
    for(R_xlen_t i = 0; i < start; ++i) {
      out[counter] = x[i];
      counter++;
    }
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
    for(R_xlen_t i = end + 1; i < x.length(); ++i) {
      out[counter] = x[i];
      counter++;
    }
  }
  return out;
}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_slice_setinv_RTYPE)]]
void rcpp_slice_setinv_RTYPE(
    RTYPEVector x, const RTYPEVector rp, const R_xlen_t start, const R_xlen_t end, const R_xlen_t by, const R_xlen_t len
  ) {
  
  if(rp.length() == len) {
    R_xlen_t counter = 0;
    if(start > 0) {
      for(R_xlen_t i = 0; i < start; ++i) {
        x[i] = rp[counter];
        counter++;
      }
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
      for(R_xlen_t i = end + 1; i < x.length(); ++i) {
        x[i] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    if(start > 0) {
      for(R_xlen_t i = 0; i < start; ++i) {
        x[i] = rp[0];
      }
    }
    
    if(by == 2) {
      for(R_xlen_t i = start; i < end; i+=2) {
        x[i+1] = rp[0];
      }
    }
    
    if(by > 2) {
      for(R_xlen_t i = start; i < end; i += by) {
        R_xlen_t startx = i + 1;
        for(R_xlen_t j = startx; j < (startx + by - 1); ++j) {
          x[j] = rp[0];
        }
      }
    }
    
    if(end < (x.length() - 1)) {
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


rcpp_scripts <- character(length(RTYPES))
names(rcpp_scripts) <- RTYPES
for(i in seq_along(RTYPES)) {
  rcpp_scripts[[i]] <- stri_replace_all(
    templatecode,
    fixed = c("RTYPE"),
    replacement = c(RTYPES[i]),
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
}


headers <- "

#include <Rcpp.h>

using namespace Rcpp;




"
rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)

Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)

fileConn <- file("src/dynamic_rcpp_slice.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)

