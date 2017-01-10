#include <Rcpp.h>


//// [[Rcpp::depends(RcppParallel)]]
//#include <RcppParallel.h>

////[[Rcpp::depends(RcppArmadillo)]]
//#include <RcppArmadillo.h>

using namespace Rcpp;


// [[Rcpp::export]]
NumericVector x_fun_cpp(double x, int grad){
  NumericVector vec(grad+1);
  for(int i = 0; i < (grad+1); ++i)
  {
    vec[i]=pow(x,i);
  }

  return vec;
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

// [[Rcpp::export]]
int g_fun_cpp(double x, NumericVector T_vec, int grad, NumericMatrix inv_X, Function x_fun){
  int erg;

  erg=0;

//  erg = (t(x_fun(x, grad))%*%t(T.vec))/(sqrt(t(x_fun(x, grad))%*%inv.X%*%x_fun(x, grad)))

  return erg;
}


