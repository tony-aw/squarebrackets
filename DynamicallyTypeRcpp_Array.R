# set-up ====

library(stringi)



################################################################################
# rcpp_sub2ind_d_32 ====


DTYPES <- 2:7
all_args <- stri_c("const SEXP ind", 1:7)
setlengths <- paste("int len", 1:7, " = Rf_length(ind", 1:7, ");", sep= "")
make_pointers <- sprintf("const int *pind%d;\npind%d = INTEGER(ind%d);\n", 1:7, 1:7, 1:7)
all_lengths <- paste("len", 1:7, sep = "")
all_for <- rev(
  sprintf("\t for(int iter%d = 0; iter%d < len%d; ++iter%d) {\n", 7:1, 7:1, 7:1, 7:1)
)

all_parts <- c(
  "pind1[iter1]",
  "pdim[0] * (pind2[iter2] - 1)",
  "pdim[1] * (pind3[iter3] - 1)",
  "pdim[2] * (pind4[iter4] - 1)",
  "pdim[3] * (pind5[iter5] - 1)",
  "pdim[4] * (pind6[iter6] - 1)",
  "pdim[5] * (pind7[iter7] - 1)"
)


templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_sub2ind_DTYPEd_32)]]
SEXP C_sub2ind_DTYPEd_32(
  <args>, SEXP dimcumprod
) {

<setlengths>
<make_pointers>

R_xlen_t counter = 0;
int temp = 0;

int *pdim;
pdim = INTEGER(dimcumprod);

int *pout;
SEXP out = PROTECT(Rf_allocVector(INTSXP, <setlength_mult>));
pout = INTEGER(out);
  
<startfor>
      
      temp = <main>;
      pout[counter] = temp;
      counter++;
      
<endfor>

UNPROTECT(1);
return out;

}


"

rcpp_scripts <- character(length(DTYPES))
names(rcpp_scripts) <- DTYPES
for(i in DTYPES) {
  
  current_args <- stri_c(all_args[1:i], collapse = ", ")
  current_setlengths <- stri_c(setlengths[1:i], collapse = "\n")
  current_makepointers <- stri_c(make_pointers[1:i], collapse = "\n")
  current_setlength_mult <- stri_c(all_lengths[1:i], collapse = " * ")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main <- stri_c(all_parts[1:i], collapse = " + ")
  current_end <- stri_c(rep("\t }", i), collapse = "\n")
  
  current_fixed <- c(
    "DTYPE",
    "<args>",
    "<setlengths>",
    "<make_pointers>",
    "<setlength_mult>",
    "<startfor>",
    "<main>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_args,
    current_setlengths,
    current_makepointers,
    current_setlength_mult,
    current_for,
    current_main,
    current_end
  )
  
  out <- stri_replace_all(
    templatecode,
    fixed = current_fixed,
    replacement = current_replacement,
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
  
  rcpp_scripts[[i]] <- out
}



templatecode2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_2d_7d_32)]]
IntegerVector rcpp_sub2ind_2d_7d_32(
  List sub, SEXP dimcumprod
) {
  int n = sub.length();
  
  IntegerVector ind1 = sub[0];
  IntegerVector ind2 = sub[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;
  IntegerVector ind7;
  IntegerVector out;

  if(n > 2) {
    ind3 = sub[2];
    if(n > 3) {
      ind4 = sub[3];
      if(n > 4) {
        ind5 = sub[4];
        if(n > 5) {
          ind6 = sub[5];
          if(n > 6) {
            ind7 = sub[6];
          }
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      out = C_sub2ind_2d_32(
        <args2>,
        dimcumprod
      );
      break;
    case 3:
      out = C_sub2ind_3d_32(
        <args3>,
        dimcumprod
      );
      break;
    case 4:
      out = C_sub2ind_4d_32(
        <args4>,
        dimcumprod
      );
      break;
    case 5:
      out = C_sub2ind_5d_32(
        <args5>,
        dimcumprod
      );
      break;
    case 6:
      out = C_sub2ind_6d_32(
        <args6>,
        dimcumprod
      );
      break;
    case 7:
      out = C_sub2ind_7d_32(
        <args7>,
        dimcumprod
      );
      break;
  }
  
  return out;
}



"

args <- sprintf("ind%d", 1:7)
rp <- sapply(2:7, \(i) stri_c(args[1:i], collapse = ", "))

templatecode2 <- stri_replace_all_fixed(
  templatecode2,
  sprintf("<args%d>", 2:7),
  rp,
  vectorize_all = FALSE
)


headers <- "

#include <Rcpp.h>

using namespace Rcpp;


"




rcpp_code <- paste(c(headers, rcpp_scripts, templatecode2), collapse = "\n\n\n")
cat(rcpp_code)

Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)


fileConn <- file("src/dynamic_rcpp_sub2ind_d_32.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)





################################################################################
# rcpp_sub2ind_d_64 ====


DTYPES <- 2:7
all_args <- stri_c("const SEXP ind", 1:7)
setlengths <- paste("int len", 1:7, " = Rf_length(ind", 1:7, ");", sep= "")
make_pointers <- sprintf("const int *pind%d;\npind%d = INTEGER(ind%d);\n", 1:7, 1:7, 1:7)
all_lengths <- paste("len", 1:7, sep = "")
all_for <- rev(
  sprintf("\t for(int iter%d = 0; iter%d < len%d; ++iter%d) {\n", 7:1, 7:1, 7:1, 7:1)
)

all_parts <- c(
  "pind1[iter1]",
  "pdim[0] * (pind2[iter2] - 1)",
  "pdim[1] * (pind3[iter3] - 1)",
  "pdim[2] * (pind4[iter4] - 1)",
  "pdim[3] * (pind5[iter5] - 1)",
  "pdim[4] * (pind6[iter6] - 1)",
  "pdim[5] * (pind7[iter7] - 1)"
)


templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.C_sub2ind_DTYPEd_64)]]
SEXP C_sub2ind_DTYPEd_64(
  <args>, SEXP dimcumprod
) {

<setlengths>
<make_pointers>

R_xlen_t counter = 0;
double temp = 0.0;

double *pdim;
pdim = REAL(dimcumprod);

double *pout;
SEXP out = PROTECT(Rf_allocVector(REALSXP, <setlength_mult>));
pout = REAL(out);
  
<startfor>
      
      temp = <main>;
      pout[counter] = temp;
      counter++;
      
<endfor>

UNPROTECT(1);
return out;

}


"

rcpp_scripts <- character(length(DTYPES))
names(rcpp_scripts) <- DTYPES
for(i in DTYPES) {
  
  current_args <- stri_c(all_args[1:i], collapse = ", ")
  current_setlengths <- stri_c(setlengths[1:i], collapse = "\n")
  current_makepointers <- stri_c(make_pointers[1:i], collapse = "\n")
  current_setlength_mult <- stri_c(all_lengths[1:i], collapse = " * ")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main <- stri_c(all_parts[1:i], collapse = " + ")
  current_end <- stri_c(rep("\t }", i), collapse = "\n")
  
  current_fixed <- c(
    "DTYPE",
    "<args>",
    "<setlengths>",
    "<make_pointers>",
    "<setlength_mult>",
    "<startfor>",
    "<main>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_args,
    current_setlengths,
    current_makepointers,
    current_setlength_mult,
    current_for,
    current_main,
    current_end
  )
  
  out <- stri_replace_all(
    templatecode,
    fixed = current_fixed,
    replacement = current_replacement,
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )
  
  rcpp_scripts[[i]] <- out
}



templatecode2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_2d_7d_64)]]
NumericVector rcpp_sub2ind_2d_7d_64(
  List sub, SEXP dimcumprod
) {
  int n = sub.length();
  
  IntegerVector ind1 = sub[0];
  IntegerVector ind2 = sub[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;
  IntegerVector ind7;
  NumericVector out;

  if(n > 2) {
    ind3 = sub[2];
    if(n > 3) {
      ind4 = sub[3];
      if(n > 4) {
        ind5 = sub[4];
        if(n > 5) {
          ind6 = sub[5];
          if(n > 6) {
            ind7 = sub[6];
          }
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      out = C_sub2ind_2d_64(
        <args2>,
        dimcumprod
      );
      break;
    case 3:
      out = C_sub2ind_3d_64(
        <args3>,
        dimcumprod
      );
      break;
    case 4:
      out = C_sub2ind_4d_64(
        <args4>,
        dimcumprod
      );
      break;
    case 5:
      out = C_sub2ind_5d_64(
        <args5>,
        dimcumprod
      );
      break;
    case 6:
      out = C_sub2ind_6d_64(
        <args6>,
        dimcumprod
      );
      break;
    case 7:
      out = C_sub2ind_7d_64(
        <args7>,
        dimcumprod
      );
      break;
  }
  
  return out;
}



"

args <- sprintf("ind%d", 1:7)
rp <- sapply(2:7, \(i) stri_c(args[1:i], collapse = ", "))

templatecode2 <- stri_replace_all_fixed(
  templatecode2,
  sprintf("<args%d>", 2:7),
  rp,
  vectorize_all = FALSE
)



headers <- "

#include <Rcpp.h>

using namespace Rcpp;


"


rcpp_code <- paste(c(headers, rcpp_scripts, templatecode2), collapse = "\n\n\n")
cat(rcpp_code)

Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)


fileConn <- file("src/dynamic_rcpp_sub2ind_d_64.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)




################################################################################
# rcpp_set_array_d ====


RTYPES <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_DTYPEd_RTYPE)]]
void rcpp_set_array_DTYPEd_RTYPE(
  RTYPEVector x, <args>, SEXP dimcumprod, RTYPEVector rp
) {

<setlengths>
<make_pointers>

double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (<setlength_mult>)) {
R_xlen_t counter = 0;
<startfor>
      flatind = <main>;
      x[flatind - 1] = rp[counter];
      counter++;
<endfor>
}
else if(rp.length() == 1) {
<startfor>
      flatind = <main>;
      x[flatind - 1] = rp[0];
<endfor>
}
else stop(\"recycling not allowed\");

}

"


rcpp_scripts1 <- character(length(DTYPES) + length(RTYPES))

counter <- 1

for(j in RTYPES) {
  for(i in DTYPES) {
    
    current_args <- stri_c(all_args[1:i], collapse = ", ")
    current_setlengths <- stri_c(setlengths[1:i], collapse = "\n")
    current_makepointers <- stri_c(make_pointers[1:i], collapse = "\n")
    current_setlength_mult <- stri_c(all_lengths[1:i], collapse = " * ")
    current_for <- stri_c(all_for[i:1], collapse = "\n")
    current_main <- stri_c(all_parts[1:i], collapse = " + ")
    current_end <- stri_c(rep("\t }", i), collapse = "\n")
    
    current_fixed <- c(
      "RTYPE",
      "DTYPE",
      "<args>",
      "<setlengths>",
      "<make_pointers>",
      "<setlength_mult>",
      "<startfor>",
      "<main>",
      "<endfor>"
    )
    current_replacement <- c(
      j,
      i,
      current_args,
      current_setlengths,
      current_makepointers,
      current_setlength_mult,
      current_for,
      current_main,
      current_end
    )
    
    out <- stri_replace_all(
      templatecode1,
      fixed = current_fixed,
      replacement = current_replacement,
      case_insensitive = FALSE,
      vectorize_all = FALSE
    )
    
    rcpp_scripts1[[counter]] <- out
    counter <- counter + 1
    
  }
  
}




templatecode2 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_6d_RTYPE)]]
void rcpp_set_array_2d_6d_RTYPE(
  RTYPEVector x, List out, NumericVector dimcumprod, RTYPEVector rp
) {
  int n = out.length();
  
  IntegerVector ind1 = out[0];
  IntegerVector ind2 = out[1];
  IntegerVector ind3;
  IntegerVector ind4;
  IntegerVector ind5;
  IntegerVector ind6;
  IntegerVector ind7;

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
          if(n > 6) {
            ind7 = out[6];
          }
        }
      }
    }
  }
  
  switch(n) {
    case 2:
      rcpp_set_array_2d_RTYPE(
        x,
        <args2>,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_RTYPE(
        x,
        <args3>,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_RTYPE(
        x,
        <args4>,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_RTYPE(
        x,
        <args5>,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_RTYPE(
        x,
        <args6>,
        dimcumprod,
        rp
      );
      break;
    case 7:
      rcpp_set_array_7d_RTYPE(
        x,
        <args7>,
        dimcumprod,
        rp
      );
      break;
  }
}

"


args <- sprintf("ind%d", 1:7)
rp <- sapply(2:7, \(i) stri_c(args[1:i], collapse = ", "))

templatecode2 <- stri_replace_all_fixed(
  templatecode2,
  sprintf("<args%d>", 2:7),
  rp,
  vectorize_all = FALSE
)


rcpp_scripts2 <- character(length(RTYPES))


for(i in seq_along(RTYPES)) {
  rcpp_scripts2[[i]] <- stri_replace_all(
    templatecode2,
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




rcpp_code <- paste(c(headers, rcpp_scripts1, rcpp_scripts2), sep = "", collapse = "\n\n\n")
cat(rcpp_code)

Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)


fileConn <- file("src/dynamic_rcpp_set_array_d.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)



sub2ind <- function(sub, x.dim, checks = TRUE) {
  
  n <- length(x.dim)
  
  if(checks) {
    if(n == 0) {
      stop("`length(x.dim) == 0`")
    }
    
    if(length(sub) != n) {
      stop("`length(sub) != length(x.dim)`")
    }
  }
  
  if(n == 1) {
    return(sub[[1]])
  }
  else if(n == 2) {
    dimcumprod <- as.double(cumprod(x.dim))
    return(.rcpp_sub2ind_2d(
      as.integer(sub[[1]]), as.integer(sub[[2]]), dimcumprod
    ))
  }
  else if(n == 3) {
    dimcumprod <- as.double(cumprod(x.dim))
    return(.rcpp_sub2ind_3d(
      as.integer(sub[[1]]), as.integer(sub[[2]]), as.integer(sub[[3]]), dimcumprod
    ))
  }
  else if(n == 4) {
    dimcumprod <- as.double(cumprod(x.dim))
    return(.rcpp_sub2ind_4d(
      as.integer(sub[[1]]),
      as.integer(sub[[2]]),
      as.integer(sub[[3]]),
      as.integer(sub[[4]]),
      dimcumprod
    ))
  }
  else if(n == 5) {
    dimcumprod <- as.double(cumprod(x.dim))
    return(.rcpp_sub2ind_5d(
      as.integer(sub[[1]]),
      as.integer(sub[[2]]),
      as.integer(sub[[3]]),
      as.integer(sub[[4]]),
      as.integer(sub[[5]]),
      dimcumprod
    ))
  }
  else if(n == 6) {
    dimcumprod <- as.double(cumprod(x.dim))
    return(.rcpp_sub2ind_6d(
      as.integer(sub[[1]]),
      as.integer(sub[[2]]),
      as.integer(sub[[3]]),
      as.integer(sub[[4]]),
      as.integer(sub[[5]]),
      as.integer(sub[[6]]),
      dimcumprod
    ))
  }
  
  return(.sub2ind_general(sub, x.dim))
  
}

sub2ind2 <- function(sub, x.dim) {
  n <- length(sub)
  if(n == 1) {
    return(sub[[1]])
  }
  else if(n <=6 ) {
    dimcumprod <- as.double(cumprod(x.dim))
    return(.rcpp_sub2ind_2d_6d(
      sub, dimcumprod
    ))
  }
  return(.sub2ind_general(sub, x.dim))
}




x.dim <- c(100, 100, 100)
x.len <- prod(x.dim)
x <- array(1:x.len, x.dim)
sub <- lapply(1:3, \(i)sample(1:99))

foo <- bench::mark(
  sub2ind(sub, x.dim, checks = FALSE),
  sub2ind2(sub, x.dim),
  min_iterations = 1000
)
foo
ggplot2::autoplot(foo)



################################################################################

# seq_rec ====


inops_nms <- c("plus", "min", "x", "div")
inops_sym <- c("+", "-", "*", "/")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_seq_rec2_<INOP>)]]
NumericVector rcpp_seq_rec2_<INOP>(
  NumericVector inits, int n, NumericVector s, NumericVector m, int form, bool rev
) {

  if(inits.length() != 2 || s.length() != 2 || m.length() != 2) {
    stop(\"`inits`, `s`, `m` must each be of length 2\");
  }
  
  NumericVector x(n);
  Range idx(0, 1);
  x[sub] = inits;
  
  int prev1;
  int prev2;
  
  if(!rev) {
    prev1 = 1;
    prev2 = 2;
  }
  if(rev) {
    prev1 = 2;
    prev2 = 1;
  }
  
  if(form == 1) {
    for (int i = 2; i < n; i++){
      x[i] = (s[0] + m[0] * x[i-prev1]) %INOP% (s[1] + m[1] * x[i-prev2]);
    }
  }
  if(form == 2) {
    for (int i = 2; i < n; i++){
       x[i] =(m[0] * (x[i-prev1] + s[0])) %INOP% (m[1] * (x[i-prev2] + s[1]));
    }
  }
  
  return x;
}

"

rcpp_scripts <- character(length(inops_sym))

for(i in seq_along(inops_sym)) {
  rcpp_scripts[i] <- stri_replace_all(
    templatecode,
    fixed = c("%INOP%", "<INOP>"),
    replacement = c(inops_sym[i], inops_nms[i]),
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



fileConn <- file("src/dynamic_rcpp_seq_rec2.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)


