#include <Rcpp.h>
using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_sub2coord)]]
IntegerMatrix rcpp_sub2coord(
  const List lst, const int nrow, const int ncol, const IntegerVector lens, const IntegerVector reps_each, const IntegerVector reps_whole
) {
  Rcpp::IntegerMatrix out(nrow, ncol);
  int counter;
  for(int i = 0; i < ncol; ++i) {
    IntegerVector temp = lst[i];
    IntegerMatrix::Column col = out(_, i);
    counter = 0;
    for(int j = 0; j < reps_whole[i]; ++j) {
      for(int k = 0; k < lens[i]; ++k) {
        for(int l = 0; l < reps_each[i]; ++l) {
          col[counter] = temp[k];
          counter++;
        }
      }
    }
  }
  return(out);
}