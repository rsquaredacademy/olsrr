#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List gvar(NumericVector ln, NumericVector ly) {
  
    int n = ly.size();
    List out(n);
    
    for(int i = 0; i < n; i++) {
        out[i] = rep(i, ln[i]);
    }

    return out;
    
}

