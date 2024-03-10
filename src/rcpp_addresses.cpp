#include <Rcpp.h>

using namespace Rcpp;

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_address)]]
String rcpp_address(SEXP x) {
  // based on 'R'-source `do_tracemem()`, but doing the 'C++' equivalent, and only getting the address (no explicit tracing involved):
  char buffer[20];
  std::snprintf(buffer, 20, "<%p>", (void *) x);
  std::string buffer2 = buffer;
  return buffer2;
  
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_list_bindings)]]
CharacterVector rcpp_list_bindings(String ref_address, Environment env, CharacterVector ls) {
  
  int n = ls.length();
  int counter = 0;
  CharacterVector out(n);
  String temp;
  for(int i = 0; i < n; ++i) {
    temp = ls[i];
    if(rcpp_address(env[temp]) == ref_address) {
      out[counter] = temp;
      counter++;
    }
  }
  out = out[Rcpp::Range(0, counter - 1)];
  return out;
}


//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_all_addresses)]]
CharacterVector rcpp_all_addresses(Environment env, CharacterVector ls) {
  
  int n = ls.length();
  String temp;
  CharacterVector out(n);
  for(int i = 0; i < n; ++i) {
    temp = ls[i];
    out[i] = rcpp_address(env[temp]);
  }
  return out;
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_address_in_env)]]
bool rcpp_address_in_env(String ref_address, Environment env, CharacterVector ls) {
  
  int n = ls.length();
  String temp;
  for(int i = 0; i < n; ++i) {
    temp = ls[i];
    if(rcpp_address(env[temp]) == ref_address) {
      return true;
    }
  }
  return false;
}