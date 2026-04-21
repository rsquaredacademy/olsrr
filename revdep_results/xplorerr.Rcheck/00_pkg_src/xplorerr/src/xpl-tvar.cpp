#include <Rcpp.h>
using namespace Rcpp;

//' Repeat data
//'
//' @param ln A list
//' @param ly A list
//' @export
// [[Rcpp::export]]
List xpl_gvar(NumericVector ln, NumericVector ly) {
  
    int n = ly.size();
    List out(n);
    
    for(int i = 0; i < n; i++) {
        out[i] = rep(i, ln[i]);
    }

    return out;
    
}

