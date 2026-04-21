#ifndef HEADER_H
#define HEADER_H

#include <RcppArmadillo.h>

arma::vec rep(arma::vec &x, arma::vec &each);

double Loglkd(const arma::vec &Y,
              const arma::vec &Z_beta,
              const arma::vec &gamma_obs);

Rcpp::StringVector modString(Rcpp::StringVector myStringV);

#endif // HEADER_H
