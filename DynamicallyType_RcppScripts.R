# set-up ====

library(stringi)


# set all ====

RTYPES <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_all_RTYPE)]]
void rcpp_set_all_RTYPE(RTYPEVector x, RTYPEVector rp) {
  R_xlen_t n = x.length();

  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[i] = rp[counter];
      counter += 1;
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

fileConn <- file("src/dynamic_rcpp_set_all.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)


################################################################################

# set vind ====

Rcpp::cppFunction(
  "
  bool rcpp_check_len(
    NumericVector rp
  ) {
    R_xlen_t n_rp = rp.length();
    bool out = n_rp == 1;
    return out;
  }
  "
)

rcpp_check_len(rp)


RTYPES <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_64_RTYPE)]]
void rcpp_set_vind_64_RTYPE(RTYPEVector x, const NumericVector ind, const RTYPEVector rp) {
  R_xlen_t n = ind.length();
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop(\"recycling not allowed\");
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_vind_32_RTYPE)]]
void rcpp_set_vind_32_RTYPE(RTYPEVector x, const IntegerVector ind, const RTYPEVector rp) {
  R_xlen_t n = ind.length();
  
  if(rp.length() == n) {
    R_xlen_t counter = 0;
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[counter];
      counter += 1;
    }
  }
  else if(rp.length() == 1) {
    for(R_xlen_t i = 0; i < n; ++i) {
      x[ind[i]] = rp[0];
    }
  }
  else stop(\"recycling not allowed\");
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

fileConn <- file("src/dynamic_rcpp_set_vind.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)


################################################################################



# set matrix scripts ====

RTYPES <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode <- list()


templatecode[["set_matrix_rowcol"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_rowcol_RTYPE)]]
void rcpp_set_matrix_rowcol_RTYPE(
  RTYPEMatrix x, IntegerVector rowind, IntegerVector colind, RTYPEVector rp
) {
  int ni = rowind.length();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      RTYPEMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      RTYPEMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
         col[rowind[i]] = rp[0];
      }
    }
  }
  else stop(\"recycling not allowed\");
  
}

"


templatecode[["set_matrix_row"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_row_RTYPE)]]
void rcpp_set_matrix_row_RTYPE(RTYPEMatrix x, IntegerVector rowind, RTYPEVector rp) {
  int ni = rowind.length();
  int nj = x.ncol();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j) {
      RTYPEMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      RTYPEMatrix::Column col = x(_, j);
      for(int i = 0; i < ni; ++i) {
        col[rowind[i]] = rp[0];
      }
    }
  }
  else stop(\"recycling not allowed\");
  
}

"

templatecode[["set_matrix_col"]] <- "
  
//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_col_RTYPE)]]
void rcpp_set_matrix_col_RTYPE(RTYPEMatrix x, IntegerVector colind, RTYPEVector rp) {
  int ni = x.nrow();
  int nj = colind.length();
  
  if(rp.length() == (ni * nj)) {
    R_xlen_t counter = 0;
    for(int j = 0; j < nj; ++j){
      RTYPEMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[counter];
        counter++;
      }
    }
  }
  else if(rp.length() == 1) {
    for(int j = 0; j < nj; ++j){
      RTYPEMatrix::Column col = x(_, colind[j]);
      for(int i = 0; i < ni; ++i) {
        col[i] = rp[0];
      }
    }
  }
  else stop(\"recycling not allowed\");
  
}

"



templatecode[["set_matrix"]] <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_matrix_RTYPE)]]
void rcpp_set_matrix_RTYPE(
  RTYPEMatrix x, IntegerVector rowind, IntegerVector colind, RTYPEVector rp
) {
  if(colind.length() == 1 && colind[0] == -1) {
    rcpp_set_matrix_row_RTYPE(x, rowind, rp);
  }
  else if(rowind.length() == 1 && rowind[0] == -1) {
    rcpp_set_matrix_col_RTYPE(x, colind, rp);
  }
  else {
    rcpp_set_matrix_rowcol_RTYPE(x, rowind, colind, rp);
  }
}


"


templatecode <- do.call(paste, templatecode)

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

fileConn <- file("src/dynamic_rcpp_set_matrix.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)



################################################################################
# rcpp_sub2ind_d (for testing rcpp_set_array) ====


DTYPES <- 2:6
all_args <- stri_c("IntegerVector ind", 1:6)
setlengths <- c("int ni = ind1.length();",
                "int nj = ind2.length();",
                "int nk = ind3.length();",
                "int nl = ind4.length();",
                "int nm = ind5.length();",
                "int nn = ind6.length();")
all_lengths <- c("ni", "nj",  "nk", "nl", "nm", "nn")
all_for <- rev(c(
  "\t for(int n = 0; n < nn; ++n) {",
  "\t for(int m = 0; m < nm; ++m) {",
  "\t for(int l = 0; l < nl; ++l) {",
  "\t for(int k = 0; k < nk; ++k) {",
  "\t for(int j = 0; j < nj; ++j) {",
  "\t for(int i = 0; i < ni; ++i) {"
))

all_parts <- c(
  "ind1[i]",
  "dimcumprod[0] * (ind2[j] - 1)",
  "dimcumprod[1] * (ind3[k] - 1)",
  "dimcumprod[2] * (ind4[l] - 1)",
  "dimcumprod[3] * (ind5[m] - 1)",
  "dimcumprod[4] * (ind6[n] - 1)"
)


templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2ind_DTYPEd)]]
NumericVector rcpp_sub2ind_DTYPEd(
  <args>, NumericVector dimcumprod
) {

<setlengths>
R_xlen_t counter = 0;
NumericVector flatind(<setlength_mult>);
  
<startfor>
  
      flatind[counter] = <main>;
      counter++;
    
<endfor>

return flatind;

}


"

rcpp_scripts <- character(length(DTYPES))
names(rcpp_scripts) <- DTYPES
for(i in DTYPES) {
  
  current_args <- stri_c(all_args[1:i], collapse = ", ")
  current_setlengths <- stri_c(setlengths[1:i], collapse = "\n")
  current_setlength_mult <- stri_c(all_lengths[1:i], collapse = " * ")
  current_for <- stri_c(all_for[i:1], collapse = "\n")
  current_main <- stri_c(all_parts[1:i], collapse = " + ")
  current_end <- stri_c(rep("\t }", i), collapse = "\n")
  
  current_fixed <- c(
    "DTYPE",
    "<args>",
    "<setlengths>",
    "<setlength_mult>",
    "<startfor>",
    "<main>",
    "<endfor>"
  )
  current_replacement <- c(
    i,
    current_args,
    current_setlengths,
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


headers <- "

#include <Rcpp.h>

using namespace Rcpp;


"




rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)

Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)


fileConn <- file("src/dynamic_rcpp_sub2ind_d_testing.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)



################################################################################
# rcpp_set_array_d ====

# NOTE: USING SAME COMPONENTS AS rcpp_sub2ind, so that I can perform partial testing

RTYPES <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode1 <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_DTYPEd_RTYPE)]]
void rcpp_set_array_DTYPEd_RTYPE(
  RTYPEVector x, <args>, NumericVector dimcumprod, RTYPEVector rp
) {

<setlengths>
double flatind = 0;
  
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
    current_setlength_mult <- stri_c(all_lengths[1:i], collapse = " * ")
    current_for <- stri_c(all_for[i:1], collapse = "\n")
    current_main <- stri_c(all_parts[1:i], collapse = " + ")
    current_end <- stri_c(rep("\t }", i), collapse = "\n")
    
    current_fixed <- c(
      "RTYPE",
      "DTYPE",
      "<args>",
      "<setlengths>",
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

  if(n > 2) {
    ind3 = out[2];
    if(n > 3) {
      ind4 = out[3];
      if(n > 4) {
        ind5 = out[4];
        if(n > 5) {
          ind6 = out[5];
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
  }
}

"


args <- sprintf("ind%d", 1:6)
rp <- sapply(2:6, \(i) stri_c(args[1:i], collapse = ", "))

templatecode2 <- stri_replace_all_fixed(
  templatecode2,
  sprintf("<args%d>", 2:6),
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

# test_sub2ind ====




################################################################################

# setapply ====

rtypes <- c("Logical", "Integer", "Numeric", "Character", "Complex", "Raw")

templatecode <- "

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_row_RTYPE)]]
void rcpp_setapply_col_RTYPE(RTYPEMatrix x, Function f) {
  int nvec = x.nrow();
  int lenvec = x.ncol();
  RTYPEVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    RTYPEMatrix::Row row = x(j, _);
    subset1 = f(x(j, _));
    row = subset1;
  }
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_setapply_col_RTYPE)]]
void rcpp_setapply_row_RTYPE(RTYPEMatrix x, Function f) {
  int nvec = x.ncol();
  int lenvec = x.nrow();
  RTYPEVector subset1(lenvec);
  for(int j = 0; j < nvec; ++j) {
    RTYPEMatrix::Column col = x(_, j);
    subset1 = f(x(_, j));
    col = subset1;
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


"
rcpp_code <- paste(c(headers, rcpp_scripts), collapse = "\n\n\n")
cat(rcpp_code)


Rcpp::sourceCpp(
  code = rcpp_code # no errors, good
)


fileConn <- file("src/dynamic_rcpp_setapply.cpp")
writeLines(rcpp_code, fileConn)
close(fileConn)





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


