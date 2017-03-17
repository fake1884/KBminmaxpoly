////[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

//// [[Rcpp::depends(RcppParallel)]]
//#include <RcppParallel.h>


using namespace Rcpp;
using namespace arma;

//////////////////////////////////////////////////////////////////////////////////////////////////
// Support-Funktionen

// Multivariate Normalverteilung in Rcpparmadillo
// [[Rcpp::export]]
arma::mat mvrnormArma(int n, arma::vec mu, arma::mat sigma) {
  int ncols = sigma.n_cols;
  arma::mat Y = arma::randn(n, ncols);
  return arma::repmat(mu, 1, n).t() + Y * arma::chol(sigma);
}

// Die tilde{x} Funktion in Rcpparmadillo
// [[Rcpp::export]]
arma::mat x_fun_cpp(double x, int grad){
  arma::mat vec(grad+1,1);
  for(int i = 0; i < (grad+1); ++i)
  {
    vec(i,0)=pow(x,i);
  }

  return vec;
}

// Eine Rcpparmadillo rep(x, anzahl) Funktion
// [[Rcpp::export]]
arma::mat rep_fun(int x, int anzahl){
  NumericMatrix erg(anzahl, 1);

  for(int i = 0; i < (anzahl); ++i)
  {
    erg(i,0)=x;
  }

  arma::mat ergfin = as<arma::mat>(erg);
  return(ergfin);
}

// Simulation einer chi-squre verteilten ZV
// [[Rcpp::export]]
arma::vec chi_square_fun(int n, arma::mat sigma){
  arma::mat erg = 0;

  int ncols = sigma.n_cols;
  arma::mat Y = arma::randn(n, ncols);

  for(int i = 0; i < (n+1); ++i)
  {
    erg = erg + 1;
  }

  return( Y );
}


////////////////////////////////////////////////////////////////////////////////////////////////////
// Hauptfunktion dieser Datei
// [[Rcpp::export]]
arma::mat S_fun_cpp(double x, int grad, double sigmahat, arma::mat Xinv){
  // Berechnung von tilde{x}
  arma::mat z = x_fun_cpp(x, grad);

  // Simulation von T
  arma::mat mu = rep_fun(0, grad+1);
  arma::mat N = mvrnormArma(1, mu, Xinv);

  arma::mat T = N / sigmahat;

  // Berechnung von T * tilde{x} / sqrt(t(tilde{x}) * Xinttraf * tilde{x})
  arma::mat erg = T * z;
  arma::mat erg2 = arma::sqrt( z.t() * Xinv * z );
  arma::mat ergfin = erg/erg2;

  return(ergfin);
}




/////////////////////////////////////////////////////////////////////////////////////////////////////////
// Functions that are exclusively used in KB.minmax.poly and not in KB.minmax.poly.fast
// [[Rcpp::export]]
arma::mat g_fun_cpp(double x, int grad, arma::mat T, arma::mat Xinv){
  // Berechnung von tilde{x}
  arma::mat z = x_fun_cpp(x, grad);

  // Berechnung von T * tilde{x} / sqrt(t(tilde{x}) * Xinttraf * tilde{x})
  arma::mat erg = T * z;
  arma::mat erg2 = arma::sqrt( z.t() * Xinv * z );
  arma::mat ergfin = erg/erg2;

  return(ergfin);
}


// [[Rcpp::export]]
NumericVector x_fun_prime_cpp(double x, int grad){
  NumericVector vec(grad+1);
  vec[0]=0;
  for(int i = 0; i < grad; ++i)
  {
    vec[i+1]=(i+1)* pow(x, i);
  }

  return vec;
}


