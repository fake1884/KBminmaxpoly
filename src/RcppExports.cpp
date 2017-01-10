// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// x_fun_cpp
NumericVector x_fun_cpp(double x, int grad);
RcppExport SEXP KBminmaxpoly_x_fun_cpp(SEXP xSEXP, SEXP gradSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type grad(gradSEXP);
    rcpp_result_gen = Rcpp::wrap(x_fun_cpp(x, grad));
    return rcpp_result_gen;
END_RCPP
}
// x_fun_prime_cpp
NumericVector x_fun_prime_cpp(double x, int grad);
RcppExport SEXP KBminmaxpoly_x_fun_prime_cpp(SEXP xSEXP, SEXP gradSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type grad(gradSEXP);
    rcpp_result_gen = Rcpp::wrap(x_fun_prime_cpp(x, grad));
    return rcpp_result_gen;
END_RCPP
}
// g_fun_cpp
int g_fun_cpp(double x, NumericVector T_vec, int grad, NumericMatrix inv_X, Function x_fun);
RcppExport SEXP KBminmaxpoly_g_fun_cpp(SEXP xSEXP, SEXP T_vecSEXP, SEXP gradSEXP, SEXP inv_XSEXP, SEXP x_funSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type T_vec(T_vecSEXP);
    Rcpp::traits::input_parameter< int >::type grad(gradSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type inv_X(inv_XSEXP);
    Rcpp::traits::input_parameter< Function >::type x_fun(x_funSEXP);
    rcpp_result_gen = Rcpp::wrap(g_fun_cpp(x, T_vec, grad, inv_X, x_fun));
    return rcpp_result_gen;
END_RCPP
}
// timesTwo
NumericVector timesTwo(NumericVector x);
RcppExport SEXP KBminmaxpoly_timesTwo(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(timesTwo(x));
    return rcpp_result_gen;
END_RCPP
}
