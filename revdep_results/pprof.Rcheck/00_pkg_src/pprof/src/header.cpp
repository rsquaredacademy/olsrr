#include <string>
#include <sstream>
#include <Rcpp.h>

Rcpp::StringVector modString(Rcpp::StringVector myStringV)
{
  if(myStringV.size() > 1){
    myStringV[1] = "Rcpp";
  }
  return myStringV;
}
