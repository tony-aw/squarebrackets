

#include <Rcpp.h>

#include "squarebrackets.h"

using namespace Rcpp;


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_stridev_chunks)]]
 List rcpp_stridev_chunks(
     SEXP y
 ) {
   R_xlen_t n = Rf_xlength(y);
   
   int n_chunks = 1;
   if(n >= std::pow(2, 16)) {
     n_chunks = std::ceil(std::pow( (double)n, 0.15));
   }
   const R_xlen_t chunksize = std::floor(n / n_chunks);
   
   NumericVector startpos(n_chunks);
   NumericVector endpos(n_chunks);
   
   R_xlen_t tempstart = 0;
   for(int j = 0; j < n_chunks; ++j) {
     startpos[j] = tempstart;
     endpos[j] = tempstart + chunksize;
     tempstart = endpos[j] + 1;
   }
   endpos[n_chunks - 1] = n - 1;
   
   List out(2);
   out[0] = startpos;
   out[1] = endpos;
   
   return out;
   
 }


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_stridev_preplist)]]
 List rcpp_stridev_preplist(
     SEXP y, SEXP v, List chunks, bool condition, LogicalVector na
 ) {
   
   MACRO_STRIDEV_TYPESWITCH(MACRO_STRIDEV_PREP);
 }




//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_stridev_prepvector)]]
 NumericVector rcpp_stridev_prepvector(
     SEXP y, List prep
 ) {
   
   const R_xlen_t n = Rf_length(y);
   
   NumericVector first = prep[0];
   NumericVector last = prep[1];
   NumericVector count = prep[2];
   NumericVector rnglen = prep[3];
   
   const double *pfirst = REAL_RO(first);
   const double *plast = REAL_RO(last);
   const double *pcount = REAL_RO(count);
   const double *prnglen = REAL_RO(rnglen);
   
   const int n_chunks = Rf_length(first);
   
   R_xlen_t first_total = -1;
   for(int j = 0; j < n_chunks; ++j) {
     if(pfirst[j] != -1) {
       first_total = pfirst[j];
       break;
     }
   }
   R_xlen_t last_total = -1;
   for(int j = (n_chunks - 1); j >= 0; --j) {
     if(plast[j] != - 1) {
       last_total = plast[j];
       break;
     }
   }
   
   R_xlen_t count_total = 0;
   for(int j = 0; j < n_chunks; ++j) {
     count_total += pcount[j];
   }
   
   R_xlen_t allocsize = 0;
   for(int j = 0; j < n_chunks; ++j) {
     if(pcount[j] > 2 && pcount[j] < prnglen[j]) {
       allocsize += prnglen[j];
     }
   }
   
   R_xlen_t rnglen_total = last_total - first_total + 1;
   
   
   NumericVector out(4);
   out[0] = first_total;
   out[1] = last_total;
   out[2] = count_total;
   out[3] = rnglen_total;
   
   return(out);
   
 }


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_stridev_transfer_bits)]]
 void rcpp_stridev_transfer_bits(IntegerVector b32, LogicalVector out, R_xlen_t startpos, R_xlen_t endpos) {
   
   int *pout = LOGICAL(out);
   R_xlen_t n = Rf_xlength(out);
   for(R_xlen_t i = 0; i < n; ++i ) {
     pout[i] = 0;
   }
   
   R_xlen_t i = startpos;
   MACRO_STRIDEV_BITS_TRANSFER(
     pout[i++] = 1,
     startpos,
     endpos
   );
 }

//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_stridev_write_bits)]]
 IntegerVector rcpp_stridev_write_bits(SEXP y, SEXP v, bool condition, LogicalVector na, R_xlen_t startpos, R_xlen_t endpos) {
   
   MACRO_STRIDEV_TYPESWITCH(MACRO_STRIDEV_BITS_WRITE);
   
 }


//' @keywords internal
 //' @noRd
 // [[Rcpp::export(.rcpp_stridev_pool)]]
 List rcpp_stridev_pool(
     SEXP y, SEXP v, List preplist, NumericVector prepvector, bool condition, LogicalVector na
 ) {
   
   const R_xlen_t first_total = prepvector[0];
   const R_xlen_t last_total = prepvector[1];
   const R_xlen_t count_total = prepvector[2];
   const R_xlen_t rnglen_total = prepvector[3];
   
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
     const R_xlen_t startpos = first[j];
     const R_xlen_t endpos = last[j];
     
     if(current_count == current_rnglen) {
       out[j] = R_NilValue;
     }
     else if(current_count <= 2) {
       out[j] = R_NilValue;
     }
     else {
       IntegerVector temp = rcpp_stridev_write_bits(y, v, condition, na, startpos, endpos);
       out[j] = temp;
     }
   }
   
   return out;
   
 }