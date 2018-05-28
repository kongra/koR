// Copyright (c) Konrad Grzanek
// Created 2017-08-12
//
#include <atomic>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

//' @export
// [[Rcpp::export]]
LogicalVector boolsAndInterveawe(LogicalVector b, LogicalVector other) {
  const std::size_t n = b.size();
  LogicalVector result(n);

  for (std::size_t i = 0, j = 0; i < n; i++)
    if (b[i] == TRUE)
      result[i] = other[j++];

  return result;
}

template<typename V>
static inline V setClass(V x, StringVector cls) {
  x.attr("class") = cls;
  return x;
}
