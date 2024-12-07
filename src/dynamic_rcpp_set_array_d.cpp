

#include <Rcpp.h>

using namespace Rcpp;






template<int RTYPE> void rcpp_set_array_2d_template(
  Vector<RTYPE> x, const SEXP ind1, const SEXP ind2, SEXP dimcumprod, Vector<RTYPE> rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2)) {
R_xlen_t counter = 0;
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
}
else stop("recycling not allowed");

}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_atomic)]]
void rcpp_set_array_2d_atomic(
  SEXP x, const SEXP ind1, const SEXP ind2, SEXP dimcumprod, const SEXP rp
) {
  
  
  switch(TYPEOF(x)){
    
  case LGLSXP:
  {
    rcpp_set_array_2d_template<LGLSXP>(as<LogicalVector>(x), ind1, ind2, dimcumprod, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_array_2d_template<INTSXP>(as<IntegerVector>(x), ind1, ind2, dimcumprod, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_array_2d_template<REALSXP>(as<NumericVector>(x), ind1, ind2, dimcumprod, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_array_2d_template<CPLXSXP>(as<ComplexVector>(x), ind1, ind2, dimcumprod, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_array_2d_template<STRSXP>(as<CharacterVector>(x), ind1, ind2, dimcumprod, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_array_2d_template<RAWSXP>(as<RawVector>(x), ind1, ind2, dimcumprod, as<RawVector>(rp));
    break;
  }

  }
}






template<int RTYPE> void rcpp_set_array_3d_template(
  Vector<RTYPE> x, const SEXP ind1, const SEXP ind2, const SEXP ind3, SEXP dimcumprod, Vector<RTYPE> rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3)) {
R_xlen_t counter = 0;
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
}
else stop("recycling not allowed");

}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_3d_atomic)]]
void rcpp_set_array_3d_atomic(
  SEXP x, const SEXP ind1, const SEXP ind2, const SEXP ind3, SEXP dimcumprod, const SEXP rp
) {
  
  
  switch(TYPEOF(x)){
    
  case LGLSXP:
  {
    rcpp_set_array_3d_template<LGLSXP>(as<LogicalVector>(x), ind1, ind2, ind3, dimcumprod, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_array_3d_template<INTSXP>(as<IntegerVector>(x), ind1, ind2, ind3, dimcumprod, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_array_3d_template<REALSXP>(as<NumericVector>(x), ind1, ind2, ind3, dimcumprod, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_array_3d_template<CPLXSXP>(as<ComplexVector>(x), ind1, ind2, ind3, dimcumprod, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_array_3d_template<STRSXP>(as<CharacterVector>(x), ind1, ind2, ind3, dimcumprod, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_array_3d_template<RAWSXP>(as<RawVector>(x), ind1, ind2, ind3, dimcumprod, as<RawVector>(rp));
    break;
  }

  }
}






template<int RTYPE> void rcpp_set_array_4d_template(
  Vector<RTYPE> x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, SEXP dimcumprod, Vector<RTYPE> rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4)) {
R_xlen_t counter = 0;
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_4d_atomic)]]
void rcpp_set_array_4d_atomic(
  SEXP x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, SEXP dimcumprod, const SEXP rp
) {
  
  
  switch(TYPEOF(x)){
    
  case LGLSXP:
  {
    rcpp_set_array_4d_template<LGLSXP>(as<LogicalVector>(x), ind1, ind2, ind3, ind4, dimcumprod, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_array_4d_template<INTSXP>(as<IntegerVector>(x), ind1, ind2, ind3, ind4, dimcumprod, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_array_4d_template<REALSXP>(as<NumericVector>(x), ind1, ind2, ind3, ind4, dimcumprod, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_array_4d_template<CPLXSXP>(as<ComplexVector>(x), ind1, ind2, ind3, ind4, dimcumprod, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_array_4d_template<STRSXP>(as<CharacterVector>(x), ind1, ind2, ind3, ind4, dimcumprod, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_array_4d_template<RAWSXP>(as<RawVector>(x), ind1, ind2, ind3, ind4, dimcumprod, as<RawVector>(rp));
    break;
  }

  }
}






template<int RTYPE> void rcpp_set_array_5d_template(
  Vector<RTYPE> x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, SEXP dimcumprod, Vector<RTYPE> rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5)) {
R_xlen_t counter = 0;
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_5d_atomic)]]
void rcpp_set_array_5d_atomic(
  SEXP x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, SEXP dimcumprod, const SEXP rp
) {
  
  
  switch(TYPEOF(x)){
    
  case LGLSXP:
  {
    rcpp_set_array_5d_template<LGLSXP>(as<LogicalVector>(x), ind1, ind2, ind3, ind4, ind5, dimcumprod, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_array_5d_template<INTSXP>(as<IntegerVector>(x), ind1, ind2, ind3, ind4, ind5, dimcumprod, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_array_5d_template<REALSXP>(as<NumericVector>(x), ind1, ind2, ind3, ind4, ind5, dimcumprod, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_array_5d_template<CPLXSXP>(as<ComplexVector>(x), ind1, ind2, ind3, ind4, ind5, dimcumprod, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_array_5d_template<STRSXP>(as<CharacterVector>(x), ind1, ind2, ind3, ind4, ind5, dimcumprod, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_array_5d_template<RAWSXP>(as<RawVector>(x), ind1, ind2, ind3, ind4, ind5, dimcumprod, as<RawVector>(rp));
    break;
  }

  }
}






template<int RTYPE> void rcpp_set_array_6d_template(
  Vector<RTYPE> x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, SEXP dimcumprod, Vector<RTYPE> rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6)) {
R_xlen_t counter = 0;
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_6d_atomic)]]
void rcpp_set_array_6d_atomic(
  SEXP x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, SEXP dimcumprod, const SEXP rp
) {
  
  
  switch(TYPEOF(x)){
    
  case LGLSXP:
  {
    rcpp_set_array_6d_template<LGLSXP>(as<LogicalVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, dimcumprod, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_array_6d_template<INTSXP>(as<IntegerVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, dimcumprod, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_array_6d_template<REALSXP>(as<NumericVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, dimcumprod, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_array_6d_template<CPLXSXP>(as<ComplexVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, dimcumprod, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_array_6d_template<STRSXP>(as<CharacterVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, dimcumprod, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_array_6d_template<RAWSXP>(as<RawVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, dimcumprod, as<RawVector>(rp));
    break;
  }

  }
}






template<int RTYPE> void rcpp_set_array_7d_template(
  Vector<RTYPE> x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, SEXP dimcumprod, Vector<RTYPE> rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
int len7 = Rf_length(ind7);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);

const int *pind7;
pind7 = INTEGER(ind7);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6 * len7)) {
R_xlen_t counter = 0;
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_7d_atomic)]]
void rcpp_set_array_7d_atomic(
  SEXP x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, SEXP dimcumprod, const SEXP rp
) {
  
  
  switch(TYPEOF(x)){
    
  case LGLSXP:
  {
    rcpp_set_array_7d_template<LGLSXP>(as<LogicalVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, dimcumprod, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_array_7d_template<INTSXP>(as<IntegerVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, dimcumprod, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_array_7d_template<REALSXP>(as<NumericVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, dimcumprod, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_array_7d_template<CPLXSXP>(as<ComplexVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, dimcumprod, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_array_7d_template<STRSXP>(as<CharacterVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, dimcumprod, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_array_7d_template<RAWSXP>(as<RawVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, dimcumprod, as<RawVector>(rp));
    break;
  }

  }
}






template<int RTYPE> void rcpp_set_array_8d_template(
  Vector<RTYPE> x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, const SEXP ind8, SEXP dimcumprod, Vector<RTYPE> rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
int len7 = Rf_length(ind7);
int len8 = Rf_length(ind8);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);

const int *pind7;
pind7 = INTEGER(ind7);

const int *pind8;
pind8 = INTEGER(ind8);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6 * len7 * len8)) {
R_xlen_t counter = 0;
	 for(int iter8 = 0; iter8 < len8; ++iter8) {

	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1) + pdim[6] * (pind8[iter8] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter8 = 0; iter8 < len8; ++iter8) {

	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1) + pdim[6] * (pind8[iter8] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_8d_atomic)]]
void rcpp_set_array_8d_atomic(
  SEXP x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, const SEXP ind8, SEXP dimcumprod, const SEXP rp
) {
  
  
  switch(TYPEOF(x)){
    
  case LGLSXP:
  {
    rcpp_set_array_8d_template<LGLSXP>(as<LogicalVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, dimcumprod, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_array_8d_template<INTSXP>(as<IntegerVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, dimcumprod, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_array_8d_template<REALSXP>(as<NumericVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, dimcumprod, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_array_8d_template<CPLXSXP>(as<ComplexVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, dimcumprod, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_array_8d_template<STRSXP>(as<CharacterVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, dimcumprod, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_array_8d_template<RAWSXP>(as<RawVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, dimcumprod, as<RawVector>(rp));
    break;
  }

  }
}






template<int RTYPE> void rcpp_set_array_16d_template(
  Vector<RTYPE> x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, const SEXP ind8, const SEXP ind9, const SEXP ind10, const SEXP ind11, const SEXP ind12, const SEXP ind13, const SEXP ind14, const SEXP ind15, const SEXP ind16, SEXP dimcumprod, Vector<RTYPE> rp
) {

int len1 = Rf_length(ind1);
int len2 = Rf_length(ind2);
int len3 = Rf_length(ind3);
int len4 = Rf_length(ind4);
int len5 = Rf_length(ind5);
int len6 = Rf_length(ind6);
int len7 = Rf_length(ind7);
int len8 = Rf_length(ind8);
int len9 = Rf_length(ind9);
int len10 = Rf_length(ind10);
int len11 = Rf_length(ind11);
int len12 = Rf_length(ind12);
int len13 = Rf_length(ind13);
int len14 = Rf_length(ind14);
int len15 = Rf_length(ind15);
int len16 = Rf_length(ind16);
const int *pind1;
pind1 = INTEGER(ind1);

const int *pind2;
pind2 = INTEGER(ind2);

const int *pind3;
pind3 = INTEGER(ind3);

const int *pind4;
pind4 = INTEGER(ind4);

const int *pind5;
pind5 = INTEGER(ind5);

const int *pind6;
pind6 = INTEGER(ind6);

const int *pind7;
pind7 = INTEGER(ind7);

const int *pind8;
pind8 = INTEGER(ind8);

const int *pind9;
pind9 = INTEGER(ind9);

const int *pind10;
pind10 = INTEGER(ind10);

const int *pind11;
pind11 = INTEGER(ind11);

const int *pind12;
pind12 = INTEGER(ind12);

const int *pind13;
pind13 = INTEGER(ind13);

const int *pind14;
pind14 = INTEGER(ind14);

const int *pind15;
pind15 = INTEGER(ind15);

const int *pind16;
pind16 = INTEGER(ind16);


double *pdim;
pdim = REAL(dimcumprod);

R_xlen_t flatind = 0;
  
if(rp.length() == (len1 * len2 * len3 * len4 * len5 * len6 * len7 * len8 * len9 * len10 * len11 * len12 * len13 * len14 * len15 * len16)) {
R_xlen_t counter = 0;
	 for(int iter16 = 0; iter16 < len16; ++iter16) {

	 for(int iter15 = 0; iter15 < len15; ++iter15) {

	 for(int iter14 = 0; iter14 < len14; ++iter14) {

	 for(int iter13 = 0; iter13 < len13; ++iter13) {

	 for(int iter12 = 0; iter12 < len12; ++iter12) {

	 for(int iter11 = 0; iter11 < len11; ++iter11) {

	 for(int iter10 = 0; iter10 < len10; ++iter10) {

	 for(int iter9 = 0; iter9 < len9; ++iter9) {

	 for(int iter8 = 0; iter8 < len8; ++iter8) {

	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1) + pdim[6] * (pind8[iter8] - 1) + pdim[7] * (pind9[iter9] - 1) + pdim[8] * (pind10[iter10] - 1) + pdim[9] * (pind11[iter11] - 1) + pdim[10] * (pind12[iter12] - 1) + pdim[11] * (pind13[iter13] - 1) + pdim[12] * (pind14[iter14] - 1) + pdim[13] * (pind15[iter15] - 1) + pdim[14] * (pind16[iter16] - 1);
      x[flatind - 1] = rp[counter];
      counter++;
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else if(rp.length() == 1) {
	 for(int iter16 = 0; iter16 < len16; ++iter16) {

	 for(int iter15 = 0; iter15 < len15; ++iter15) {

	 for(int iter14 = 0; iter14 < len14; ++iter14) {

	 for(int iter13 = 0; iter13 < len13; ++iter13) {

	 for(int iter12 = 0; iter12 < len12; ++iter12) {

	 for(int iter11 = 0; iter11 < len11; ++iter11) {

	 for(int iter10 = 0; iter10 < len10; ++iter10) {

	 for(int iter9 = 0; iter9 < len9; ++iter9) {

	 for(int iter8 = 0; iter8 < len8; ++iter8) {

	 for(int iter7 = 0; iter7 < len7; ++iter7) {

	 for(int iter6 = 0; iter6 < len6; ++iter6) {

	 for(int iter5 = 0; iter5 < len5; ++iter5) {

	 for(int iter4 = 0; iter4 < len4; ++iter4) {

	 for(int iter3 = 0; iter3 < len3; ++iter3) {

	 for(int iter2 = 0; iter2 < len2; ++iter2) {

	 for(int iter1 = 0; iter1 < len1; ++iter1) {

      flatind = pind1[iter1] + pdim[0] * (pind2[iter2] - 1) + pdim[1] * (pind3[iter3] - 1) + pdim[2] * (pind4[iter4] - 1) + pdim[3] * (pind5[iter5] - 1) + pdim[4] * (pind6[iter6] - 1) + pdim[5] * (pind7[iter7] - 1) + pdim[6] * (pind8[iter8] - 1) + pdim[7] * (pind9[iter9] - 1) + pdim[8] * (pind10[iter10] - 1) + pdim[9] * (pind11[iter11] - 1) + pdim[10] * (pind12[iter12] - 1) + pdim[11] * (pind13[iter13] - 1) + pdim[12] * (pind14[iter14] - 1) + pdim[13] * (pind15[iter15] - 1) + pdim[14] * (pind16[iter16] - 1);
      x[flatind - 1] = rp[0];
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
	 }
}
else stop("recycling not allowed");

}



//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_16d_atomic)]]
void rcpp_set_array_16d_atomic(
  SEXP x, const SEXP ind1, const SEXP ind2, const SEXP ind3, const SEXP ind4, const SEXP ind5, const SEXP ind6, const SEXP ind7, const SEXP ind8, const SEXP ind9, const SEXP ind10, const SEXP ind11, const SEXP ind12, const SEXP ind13, const SEXP ind14, const SEXP ind15, const SEXP ind16, SEXP dimcumprod, const SEXP rp
) {
  
  
  switch(TYPEOF(x)){
    
  case LGLSXP:
  {
    rcpp_set_array_16d_template<LGLSXP>(as<LogicalVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, ind9, ind10, ind11, ind12, ind13, ind14, ind15, ind16, dimcumprod, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_array_16d_template<INTSXP>(as<IntegerVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, ind9, ind10, ind11, ind12, ind13, ind14, ind15, ind16, dimcumprod, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_array_16d_template<REALSXP>(as<NumericVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, ind9, ind10, ind11, ind12, ind13, ind14, ind15, ind16, dimcumprod, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_array_16d_template<CPLXSXP>(as<ComplexVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, ind9, ind10, ind11, ind12, ind13, ind14, ind15, ind16, dimcumprod, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_array_16d_template<STRSXP>(as<CharacterVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, ind9, ind10, ind11, ind12, ind13, ind14, ind15, ind16, dimcumprod, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_array_16d_template<RAWSXP>(as<RawVector>(x), ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, ind9, ind10, ind11, ind12, ind13, ind14, ind15, ind16, dimcumprod, as<RawVector>(rp));
    break;
  }

  }
}






//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_2d_8d_atomic)]]
void rcpp_set_array_2d_8d_atomic(
  SEXP x, List out, NumericVector dimcumprod, SEXP rp
) {
  int n = out.length();
  
  SEXP ind1 = out[0];
  SEXP ind2 = out[1];
  SEXP ind3;
  SEXP ind4;
  SEXP ind5;
  SEXP ind6;
  SEXP ind7;
  SEXP ind8;

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
      rcpp_set_array_2d_atomic(
        x,
        ind1, ind2,
        dimcumprod,
        rp
      );
      break;
    case 3:
      rcpp_set_array_3d_atomic(
        x,
        ind1, ind2, ind3,
        dimcumprod,
        rp
      );
      break;
    case 4:
      rcpp_set_array_4d_atomic(
        x,
        ind1, ind2, ind3, ind4,
        dimcumprod,
        rp
      );
      break;
    case 5:
      rcpp_set_array_5d_atomic(
        x,
        ind1, ind2, ind3, ind4, ind5,
        dimcumprod,
        rp
      );
      break;
    case 6:
      rcpp_set_array_6d_atomic(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6,
        dimcumprod,
        rp
      );
      break;
    case 7:
      rcpp_set_array_7d_atomic(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6, ind7,
        dimcumprod,
        rp
      );
      break;
    case 8:
      rcpp_set_array_8d_atomic(
        x,
        ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8,
        dimcumprod,
        rp
      );
      break;
  }
}


