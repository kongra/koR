// Copyright (c) Konrad Grzanek
// Created 2015-10-08
//
#include <Rcpp.h>
using namespace Rcpp;

//' Returns the skewness of the vector. No NAs assumed.
//'
//' @param x the vector
//' @export
// [[Rcpp::export]]
double skewnessRcpp(NumericVector x) {
  double mean = Rcpp::mean(x);
  int n       = x.size();

  double squaresSum = 0;
  double cubesSum   = 0;

  for(int i = 0; i < n; i++) {
    double r   = x[i] - mean;
    double s   = r * r;
    squaresSum += s;
    cubesSum   += (s * r);
  }
  double v = squaresSum / (n - 1);
  return cubesSum / ((n - 1) * sqrt(v * v * v));
}
