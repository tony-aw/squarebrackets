#include <Rcpp.h>

using namespace Rcpp;


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_check_conform_dims)]]
void rcpp_check_conform_dims(
  IntegerVector conform_dim, IntegerMatrix arg_dim, int n, int along
) {
  int m = arg_dim.nrow();
  for(int i = 0; i < n; ++i) {
    for(int j = 0; j < m; ++j) {
      if(j != (along - 1)) {
        if(conform_dim[j] != arg_dim(j, i)) {
          stop("non-conformable dimensions");
        }
      }
    }
  }
}