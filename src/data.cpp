// Copyright (c) Konrad Grzanek
// Created 2015-10-08
//
#include <Rcpp.h>
using namespace Rcpp;

#define ARMA_DONT_USE_WRAPPER
#include <armadillo>

//' Returns the skewness of the vector. No NAs assumed.
//'
//' @param x the vector
//' @export
// [[Rcpp::export]]
double skewnessRcpp(NumericVector x) {
  const double mean = Rcpp::mean(x);
  const int n       = x.size();
  double squaresSum = 0;
  double cubesSum   = 0;

  for(int i = 0; i < n; i++) {
    const double r = x[i] - mean;
    const double s = r * r;
    squaresSum    += s;
    cubesSum      += (s * r);
  }
  const double v = squaresSum / (n - 1);
  return cubesSum / ((n - 1) * sqrt(v * v * v));
}

//' Some test for Armadillo
//' @export
// [[Rcpp::export]]
double armaTest() {
  using namespace arma;
  const mat A = randu<mat>(4000, 4000);
  const mat B = randu<mat>(4000, 4000);
  const mat C = A * B;
  return 0;
}
