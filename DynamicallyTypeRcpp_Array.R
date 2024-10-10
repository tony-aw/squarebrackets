# set-up ====

library(stringi)



################################################################################
# rcpp_sub2ind_d_32 ====


DTYPES <- 2:8
all_args <- stri_c("const SEXP ind", 1:8)
setlengths <- paste("int len", 1:8, " = Rf_length(ind", 1:8, ");", sep= "")
make_pointers <- sprintf("const int *pind%d;\npind%d = INTEGER(ind%d);\n", 1:8, 1:8, 1:8)
all_lengths <- paste("len", 1:8, sep = "")
all_for <- rev(
  sprintf("\t for(int iter%d = 0; iter%d < len%d; ++iter%d) {\n", 8:1, 8:1, 8:1, 8:1)
)

all_parts <- c(
  "pind1[iter1]",
  "pdim[0] * (pind2[iter2] - 1)",
  "pdim[1] * (pind3[iter3] - 1)",
  "pdim[2] * (pind4[iter4] - 1)",
  "pdim[3] * (pind5[iter5] - 1)",
  "pdim[4] * (pind6[iter6] - 1)",
  "pdim[5] * (pind7[iter7] - 1)",
  "pdim[6] * (pind8[iter8] - 1)"
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
// [[Rcpp::export(.rcpp_sub2ind_2d_8d_32)]]
IntegerVector rcpp_sub2ind_2d_8d_32(
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
  IntegerVector ind8;
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
            if(n > 7) {
              ind8 = sub[7];
            }
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
    case 8:
      out = C_sub2ind_8d_32(
        <args8>,
        dimcumprod
      );
      break;
  }
  
  return out;
}



"

args <- sprintf("ind%d", 1:8)
rp <- sapply(2:8, \(i) stri_c(args[1:i], collapse = ", "))

templatecode2 <- stri_replace_all_fixed(
  templatecode2,
  sprintf("<args%d>", 2:8),
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
// [[Rcpp::export(.rcpp_sub2ind_2d_8d_64)]]
NumericVector rcpp_sub2ind_2d_8d_64(
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
  IntegerVector ind8;
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
            if(n > 7) {
              ind8 = sub[7];
            }
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
    case 8:
      out = C_sub2ind_8d_64(
        <args8>,
        dimcumprod
      );
      break;
  }
  
  return out;
}



"

args <- sprintf("ind%d", 1:8)
rp <- sapply(2:8, \(i) stri_c(args[1:i], collapse = ", "))

templatecode2 <- stri_replace_all_fixed(
  templatecode2,
  sprintf("<args%d>", 2:8),
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
// [[Rcpp::export(.rcpp_set_array_2d_8d_RTYPE)]]
void rcpp_set_array_2d_8d_RTYPE(
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
  IntegerVector ind8;

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
            if(n > 7) {
              ind8 = out[7];
            }
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
    case 8:
      rcpp_set_array_8d_RTYPE(
        x,
        <args8>,
        dimcumprod,
        rp
      );
      break;
  }
}

"


args <- sprintf("ind%d", 1:8)
rp <- sapply(2:8, \(i) stri_c(args[1:i], collapse = ", "))

templatecode2 <- stri_replace_all_fixed(
  templatecode2,
  sprintf("<args%d>", 2:8),
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




