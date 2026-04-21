//#define ARMA_NO_DEBUG
#define STRICT_R_HEADERS // needed on Windows, not on macOS
#include <RcppParallel.h>
// [[Rcpp::depends(RcppParallel)]]
#define ARMA_DONT_USE_OPENMP
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <cmath>
#include "header.h"
#include "myomp.h"
#include <iostream>
#include <chrono>

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(openmp)]]
using namespace RcppParallel;
using namespace Rcpp;
using namespace std;
using namespace arma;

//` @importFrom RcppParallel RcppParallelLibs


// [[Rcpp::export]]
double logdet_info(const arma::vec& info_gamma,             // m-vector (diag D)
                   const arma::mat& schur)
{
  // log det(A) with a tiny guard
  const double eps = 1e-12;
  const double logdet_A = arma::sum(arma::log(arma::clamp(info_gamma, eps, DBL_MAX)));

  // (optional) enforce symmetry to reduce round-off issues
  arma::mat S = 0.5 * (schur + schur.t());

  // Cholesky (upper by default): S = R.t() * R
  arma::mat R;
  if (!arma::chol(R, S)) {
    // try a small ridge
    const double ridge = 1e-8;
    if (!arma::chol(R, S + ridge * arma::eye(S.n_rows, S.n_cols))) {
      Rcpp::stop("Cholesky failed: Schur complement not (numerically) PD.");
    }
  }

  const double logdet_S = 2.0 * arma::sum(arma::log(R.diag()));
  return logdet_A + logdet_S;
}

// [[Rcpp::export]]
double Loglkd_firth(arma::vec &Y, arma::mat &Z, arma::vec &n_prov,
                    arma::vec &gamma, arma::vec &beta) {
  int m = gamma.n_elem;
  int beta_size = beta.n_elem;
  arma::vec score_gamma(m), info_gamma(m);
  arma::mat info_betagamma(beta_size, m, arma::fill::zeros);
  arma::mat schur(beta_size, beta_size, arma::fill::zeros);  //I^*(\beta)

  arma::vec Z_beta = Z * beta;
  arma::vec gamma_obs = rep(gamma, n_prov);
  arma::vec p = 1 / (1 + exp(-gamma_obs-Z_beta));
  arma::vec pq = p % (1-p);  // pq under full model
  if (any(pq == 0)) {
    pq.replace(0, 1e-10);
  }

  int ind = 0;
  for (int i = 0; i < m; ++i) {
    info_gamma(i) = sum(pq.subvec(ind, ind + n_prov(i) - 1));
    info_betagamma.col(i) = sum(Z.rows(ind, ind + n_prov(i) - 1).each_col() % pq.subvec(ind, ind + n_prov(i) - 1)).t();
    ind += n_prov(i);
  }
  arma::mat info_beta = Z.t() * (Z.each_col()%pq);
  arma::vec info_gamma_inv = 1.0 / info_gamma;
  arma::mat mat_tmp1 = info_betagamma.each_row() % info_gamma_inv.t();
  schur = info_beta - mat_tmp1 * info_betagamma.t();

  double loglik = Loglkd(Y, Z_beta, gamma_obs);
  double penalty = 0.5 * logdet_info(info_gamma, schur);
  return loglik + penalty;
}


// [[Rcpp::export]]
List logis_firth_prov(arma::vec &Y, arma::mat &Z, arma::vec &n_prov, arma::vec gamma, arma::vec beta,
                      int n_obs, int m, const int &threads, double tol = 1e-5, int max_iter = 100,
                      double bound = 10.0, bool message = true, const std::string &stop = "beta",
                      bool need_trace = false) {
  if (message == true) {
    Rcpp::Rcout << "Implementing firth-corrected fixed provider effects model (Rcpp) ..." << endl;
  }

  // make "provider" as the basic unit of parallelization (so ideally maximum number of threads = number of providers)
  arma::ivec indices(m + 1);
  int ind = 0;
  for (int i = 0; i < m; i++) {
    indices(i) = ind;
    ind += n_prov(i);
  }
  indices(m) = ind;

  int chunk_id; //each thread will process a chunk of data

  int beta_size = Z.n_cols;

  arma::vec score_gamma(m), info_gamma_inv(m);
  arma::mat J(beta_size, m);
  arma::mat J2(beta_size, m);  //I^*(\beta)^{-1} * J; where "I^*(\beta)^{-1}" is "schur_inv".
  arma::mat schur(beta_size, beta_size);  //I^*(\beta)
  arma::mat schur_inv(beta_size, beta_size);   //I^*(\beta)^{-1}
  arma::mat info_betagamma(beta_size, m, arma::fill::zeros);

  arma::vec p(n_obs);
  arma::vec Yp(n_obs);
  arma::vec pq(n_obs);
  arma::vec H(beta_size);

  // matrices that will be *reused* next iteration  // <<< modified
  arma::vec info_gamma_inv_prev;
  arma::mat info_betagamma_prev, schur_prev;
  bool first_iter = true;

  omp_set_num_threads(threads);
  int iter = 0;
  double crit = 1e9;

  arma::vec loglik_trace;                 // grows as needed
  double loglik_pen = NA_REAL;
  double old_pen = NA_REAL;        // previous penalised ℓ*
  double pen_init = NA_REAL;

  if (message) Rcpp::Rcout << "Algorithm ("<<threads<<" cores) ...\n";

  #pragma omp parallel shared(indices, iter, crit)
  {
    arma::mat local_schur(beta_size, beta_size, arma::fill::zeros);
    arma::vec local_H(beta_size, arma::fill::zeros);

    while (iter < max_iter && crit > tol) {
        if (first_iter) {
            #pragma omp single
            {
                schur.zeros();
                chunk_id=0;
            }

            while(true){
                int my_chunk;
                #pragma omp critical (get_chunk_id_1)
                {
                    my_chunk = (chunk_id < m) ? chunk_id++ : -1;
                }
                if (my_chunk == -1) {
                    break;
                }
                const int start = indices(my_chunk);
                const int stop  = indices(my_chunk + 1) - 1;

                const arma::mat &TDS_Z = Z.rows(start, stop);
                const arma::vec &TDS_Y = Y.subvec(start, stop);

                arma::vec TDS_gamma_obs = rep(gamma(my_chunk), n_prov(my_chunk)); // get the chunk of gamma
                arma::vec TDS_Z_beta = TDS_Z * beta;
                arma::vec TDS_p = 1.0 / (1.0 + exp(-TDS_gamma_obs-TDS_Z_beta));
                arma::vec TDS_pq = TDS_p % (1.0 - TDS_p);
                if (arma::any(TDS_pq == 0)) {
                    TDS_pq.replace(0, 1e-10);
                }

                p.subvec(start, stop) = TDS_p;
                Yp.subvec(start, stop) = TDS_Y - TDS_p;
                pq.subvec(start, stop) = TDS_pq;

                // 1. compute I(gamma, gamma) & I(beta, gamma) & I(beta, beta)
                double TDS_info_gamma_inv = 1 / sum(TDS_pq);
                info_gamma_inv(my_chunk) = TDS_info_gamma_inv;

                arma::vec TDS_info_betagamma = sum(TDS_Z.each_col() % TDS_pq).t();  //p*1 ("B_i" in notes)
                info_betagamma.col(my_chunk) = TDS_info_betagamma;

                J.col(my_chunk) = TDS_info_betagamma * TDS_info_gamma_inv;

                arma::mat TDS_info_beta = TDS_Z.t() * (TDS_Z.each_col() % TDS_pq);  //p*p ("C_i" in notes)
                local_schur += TDS_info_beta - J.col(my_chunk) * TDS_info_betagamma.t();
            }
            #pragma omp critical (update_schur)
            {
                schur += local_schur;
            }
            #pragma omp barrier
            local_schur.zeros();

            #pragma omp single
            {
                schur_inv = inv_sympd(schur);  //S^-1

                double loglkd0 = Loglkd(Y, Z*beta, rep(gamma,n_prov));
                pen_init = loglkd0 + 0.5 * logdet_info(1.0/info_gamma_inv, schur);
                old_pen  = pen_init;
                first_iter = false;
            }
        }

        #pragma omp single
        {
            chunk_id = 0;
            H.zeros();
        }
        #pragma omp barrier

        while (true){
            int my_chunk;
            #pragma omp critical (get_chunk_id_2)
            {
                my_chunk = (chunk_id < m) ? chunk_id++ : -1;
            }
            if (my_chunk == -1) {
                break;
            }

            J2.col(my_chunk) = schur_inv * J.col(my_chunk); //p*1 (J2_i)

            const int start = indices(my_chunk);
            const int stop  = indices(my_chunk + 1) - 1;

            const arma::vec &TDS_Yp = Yp.subvec(start, stop); //n_i*1
            const arma::mat &TDS_Z = Z.rows(start, stop);  //n_i*p
            const arma::vec &TDS_p = p.subvec(start, stop);
            const arma::vec &TDS_pq = pq.subvec(start, stop);

            double TDS_prod = arma::as_scalar(J.col(my_chunk).t() * J2.col(my_chunk)); // 1*1
            double TDS_diag_prod = info_gamma_inv(my_chunk) + TDS_prod;
            arma::vec TDS_c1 = rep(TDS_diag_prod, n_prov(my_chunk)); // n_i*1

            arma::vec TDS_c2 = -TDS_Z * J2.col(my_chunk); // n_i*1

            int n_i = n_prov(my_chunk);
            arma::vec TDS_c3(n_i);
            for (int i = 0; i < n_i; i++) {
                TDS_c3(i) = arma::as_scalar(TDS_Z.row(i) * schur_inv * TDS_Z.row(i).t());
            }
            arma::vec TDS_YpA = TDS_Yp + TDS_pq % (TDS_c1 + TDS_c2 + TDS_c2 + TDS_c3) % (0.5 - TDS_p); // n_i*1

            score_gamma(my_chunk) = sum(TDS_YpA);
            local_H += J.col(my_chunk) * score_gamma(my_chunk) - TDS_Z.t() * TDS_YpA; //H_i
        }
        #pragma omp critical (update_H)
        {
            H += local_H; //sum over all threads to get the global H
        }
        #pragma omp barrier
        local_H.zeros();

        arma::vec d_beta;
        #pragma omp single
        {
          d_beta = - schur_inv * H;
          beta += d_beta;
          chunk_id = 0;
          iter++;
        }
        #pragma omp barrier

        // update gamma
        while (true){
            int my_chunk;
            #pragma omp critical (get_chunk_id_3)
            {
                my_chunk = (chunk_id < m) ? chunk_id++ : -1;
            }
            if (my_chunk == -1) {
                break;
            }

            gamma(my_chunk) += info_gamma_inv(my_chunk) * score_gamma(my_chunk) + arma::as_scalar(J2.col(my_chunk).t() * H);
        }
        #pragma omp barrier

        #pragma omp single
        {
            gamma = clamp(gamma, median(gamma) - bound, median(gamma) + bound);
        }

        // compute the current penalized log-likelihood, which also computes the updated schur, info_betagamma, and info_gamma_inv
        #pragma omp single
        {
            schur.zeros();
            chunk_id=0;
        }

        while(true){
            int my_chunk;
            #pragma omp critical (get_chunk_id_X)
            {
                my_chunk = (chunk_id < m) ? chunk_id++ : -1;
            }
            if (my_chunk == -1) {
                break;
            }
            const int start = indices(my_chunk);
            const int stop  = indices(my_chunk + 1) - 1;

            const arma::mat &TDS_Z = Z.rows(start, stop);
            const arma::vec &TDS_Y = Y.subvec(start, stop);

            arma::vec TDS_gamma_obs = rep(gamma(my_chunk), n_prov(my_chunk));
            arma::vec TDS_Z_beta = TDS_Z * beta;
            arma::vec TDS_p = 1.0 / (1.0 + exp(-TDS_gamma_obs-TDS_Z_beta));
            arma::vec TDS_pq = TDS_p % (1.0 - TDS_p);
            if (arma::any(TDS_pq == 0)) {
                TDS_pq.replace(0, 1e-10);
            }

            p.subvec(start, stop) = TDS_p;
            Yp.subvec(start, stop) = TDS_Y - TDS_p;
            pq.subvec(start, stop) = TDS_pq;

            double TDS_info_gamma_inv = 1 / sum(TDS_pq);
            info_gamma_inv(my_chunk) = TDS_info_gamma_inv;

            arma::vec TDS_info_betagamma = sum(TDS_Z.each_col() % TDS_pq).t();
            info_betagamma.col(my_chunk) = TDS_info_betagamma;

            J.col(my_chunk) = TDS_info_betagamma * TDS_info_gamma_inv;

            arma::mat TDS_info_beta = TDS_Z.t() * (TDS_Z.each_col() % TDS_pq);
            local_schur += TDS_info_beta - J.col(my_chunk) * TDS_info_betagamma.t();
        }
        #pragma omp critical (update_schur)
        {
            schur += local_schur;
        }
        #pragma omp barrier
        local_schur.zeros();

        #pragma omp single
        {
            schur_inv = inv_sympd(schur);

            // compute the penalized log-likelihood
            double loglkd = Loglkd(Y, Z*beta, rep(gamma,n_prov));
            double pen = 0.5 * logdet_info(1.0/info_gamma_inv, schur);
            loglik_pen = loglkd + pen;


            // Rcpp::Rcout << "Iter " << iter
            //     << ": loglkd = " << std::setprecision(3) << loglkd
            //     << ", pen = " << std::setprecision(3) << pen
            //     << ", loglik_pen = " << std::setprecision(3) << loglik_pen
            //     << ";" << std::endl;

            if (need_trace) {
                loglik_trace.insert_rows(loglik_trace.n_rows, arma::vec({loglik_pen}));
            }

            if (stop == "beta"){
                crit = norm(d_beta, "inf");
                if (message == true) {
                  Rcpp::Rcout << "Iter " << iter << ": Inf norm of running diff in est reg parm is " << setprecision(3) << scientific << crit << ";" << endl;
                }
            } else {
                double d_loglkd = loglik_pen - old_pen;
                old_pen = loglik_pen;

                if (stop == "relch") {
                    crit = abs(d_loglkd/loglik_pen);
                    if (message == true) {
                      Rcpp::Rcout << "Iter " << iter << ": Relative change in est log likelihood is " << setprecision(3) << scientific << crit << ";" << endl;
                    }
                } else if (stop == "ratch") {
                    crit = abs(d_loglkd/(loglik_pen-pen_init));
                    if (message == true) {
                      Rcpp::Rcout << "Iter " << iter << ": Adjusted relative change in est log likelihood is " << setprecision(3) << scientific << crit << ";" << endl;
                    }
                } else if (stop == "all") {
                    arma::vec crits(3);
                    crits(0) = norm(d_beta, "inf");
                    crits(1) = abs(d_loglkd/loglik_pen);
                    crits(2) = crit = abs(d_loglkd/(loglik_pen-pen_init));
                    crit = crits.max();
                    if (message == true) {
                      Rcpp::Rcout << "Iter " << iter << ": Maximum criterion across all checks is " << setprecision(3) << scientific << crit << ";" << endl;
                    }
                } else if (stop == "or") {
                    arma::vec crits(3);
                    crits(0) = norm(d_beta, "inf");
                    crits(1) = abs(d_loglkd/loglik_pen);
                    crits(2) = crit = abs(d_loglkd/(loglik_pen-pen_init));
                    crit = crits.min();
                    if (message == true) {
                      Rcpp::Rcout << "Iter " << iter << ": Minimum criterion across all checks is " << setprecision(3) << scientific << crit << ";" << endl;
                    }
                } else {
                    Rcpp::stop("Argument 'stop' NOT as required!");
                }
            }
        }
        #pragma omp barrier
    }
  }

  if (!need_trace) {
    loglik_trace.set_size(1);
    loglik_trace(0) = loglik_pen;
  }

  if (message == true) {
    Rcpp::Rcout << "Algorithm with " << threads <<  " cores converged after " << iter << " iterations." << endl;
  }

  return List::create(_["gamma"]=gamma,
                      _["beta"] =beta,
                      _["loglik"]=loglik_pen,
                      _["history"]=loglik_trace,
                      _["iter"]=iter,
                      _["crit"]=crit);
}






