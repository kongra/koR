// Copyright (c) Konrad Grzanek
// Created 2017-08-12
//
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

template<typename T>
static inline void arraycopy(T src, size_t srcPos, T dest, size_t destPos, size_t length) {
  for (size_t i = 0; i < length; i++) dest[i + destPos] = src[i + srcPos];
}

//' Works like System.arraycopy(...) in Java for integer vectors.
//' @export
// [[Rcpp::export]]
void copyInts(IntegerVector src, size_t srcPos, IntegerVector dest, size_t destPos, size_t length) {
  arraycopy(src, srcPos, dest, destPos, length);
}

//' Works like System.arraycopy(...) in Java for double vectors.
//' @export
// [[Rcpp::export]]
void copyDoubles(DoubleVector src, size_t srcPos, DoubleVector dest, size_t destPos, size_t length) {
  arraycopy(src, srcPos, dest, destPos, length);
}
