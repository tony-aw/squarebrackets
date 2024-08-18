#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2coord)]]
IntegerMatrix rcpp_sub2coord(
  const List lst, const double nrow, const int ncol, const NumericVector lens, const NumericVector reps_each, const NumericVector reps_whole
) {
  Rcpp::IntegerMatrix out(nrow, ncol);
  int counter;
  for(int i = 0; i < ncol; ++i) {
    IntegerVector temp = lst[i];
    IntegerMatrix::Column col = out(_, i);
    counter = 0;
    for(R_xlen_t j = 0; j < reps_whole[i]; ++j) {
      for(R_xlen_t k = 0; k < lens[i]; ++k) {
        for(R_xlen_t l = 0; l < reps_each[i]; ++l) {
          col[counter] = temp[k];
          counter++;
        }
      }
    }
  }
  return(out);
}