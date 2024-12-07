#include <Rcpp.h>

using namespace Rcpp;


#include <Rcpp.h>

using namespace Rcpp;


template<int RTYPE> void rcpp_set_array_general_template(
  Vector<RTYPE> x,
  const SEXP s, const SEXP xdims,
  Vector<RTYPE> rp
) {
  int k;
  
  const void *vmaxsave = vmaxget(); // Because I'm gonna use R_alloc()
  
 // s is a list
 
  k = Rf_length(xdims);

  // allocate vectors (kinda like a list) through R_alloc(); k = ndims(x)
  int **subs = (int**)R_alloc(k, sizeof(int*));
  int *indx = (int*)R_alloc(k, sizeof(int));
  int *bound = (int*)R_alloc(k, sizeof(int));
  R_xlen_t *offset = (R_xlen_t*)R_alloc(k, sizeof(R_xlen_t));
  
  R_xlen_t n = 1;
  SEXP r;
  
  for (int i = 0; i < k; i++) {
    r = VECTOR_ELT(s, i); 
  	indx[i] = 0;
  	bound[i] = Rf_length(r);
    n *= bound[i];
  	subs[i] = INTEGER(r);
  }
  
  
  offset[0] = 1;
  for (int i = 1; i < k; i++) {
    offset[i] = offset[i - 1] * INTEGER(xdims)[i - 1];
  }
  
  
  if(rp.length() == n) {
    R_xlen_t counter = 0;

    for (R_xlen_t i = 0; i < n; i++) {
      R_xlen_t flatind = 0;
    	for (int j = 0; j < k; j++) {
  	    int jj = subs[j][indx[j]];
  	    flatind += (jj - 1) * offset[j];
    	}
    	
    	x[flatind] = rp[counter]; // meat in the voodoo sandwich
    	counter++;
    	
    	if (n > 1) {
    	    int j = 0;
    	    while (++indx[j] >= bound[j]) {
        		indx[j] = 0;
        		j = (j + 1) % k;
    	    }
    	}
    }
  }
  else if(rp.length() == 1) {

    for (R_xlen_t i = 0; i < n; i++) {
      R_xlen_t flatind = 0;
    	for (int j = 0; j < k; j++) {
  	    int jj = subs[j][indx[j]];
  	    flatind += (jj - 1) * offset[j];
    	}
    	
    	x[flatind] = rp[0]; // meat in the voodoo sandwich
    	
    	if (n > 1) {
    	    int j = 0;
    	    while (++indx[j] >= bound[j]) {
        		indx[j] = 0;
        		j = (j + 1) % k;
    	    }
    	}
    }
  }
  else {
    stop("recycling not allowed");

  }
  

  // Free temporary memory
  vmaxset(vmaxsave);
  
}

//' @keywords internal
//' @noRd
// [[Rcpp::export(.rcpp_set_array_general_atomic)]]
void rcpp_set_array_general_atomic(
  SEXP x,
  const SEXP s, const SEXP xdims,
  const SEXP rp
) {
  
  
  switch(TYPEOF(x)){
    
  case LGLSXP:
  {
    rcpp_set_array_general_template<LGLSXP>(as<LogicalVector>(x), s, xdims, as<LogicalVector>(rp));
    break;
  }


  case INTSXP:
  {
    rcpp_set_array_general_template<INTSXP>(as<IntegerVector>(x), s, xdims, as<IntegerVector>(rp));
    break;
  }


  case REALSXP:
  {
    rcpp_set_array_general_template<REALSXP>(as<NumericVector>(x), s, xdims, as<NumericVector>(rp));
    break;
  }


  case CPLXSXP:
  {
    rcpp_set_array_general_template<CPLXSXP>(as<ComplexVector>(x), s, xdims, as<ComplexVector>(rp));
    break;
  }


  case STRSXP:
  {
    rcpp_set_array_general_template<STRSXP>(as<CharacterVector>(x), s, xdims, as<CharacterVector>(rp));
    break;
  }


  case RAWSXP:
  {
    rcpp_set_array_general_template<RAWSXP>(as<RawVector>(x), s, xdims, as<RawVector>(rp));
    break;
  }

  }
}
