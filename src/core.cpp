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
static inline void arraycopy(const T src,
                             const std::size_t srcPos,
                             T dest,
                             const std::size_t destPos,
                             const std::size_t length) {
  for (size_t i = 0; i < length; i++) dest[i + destPos] = src[i + srcPos];
}

//' Works like System.arraycopy(...) in Java for integer vectors.
//' @export
// [[Rcpp::export]]
void copyInts(const IntegerVector src,
              const std::size_t srcPos,
              IntegerVector dest,
              const std::size_t destPos,
              const std::size_t length) {
  arraycopy(src, srcPos, dest, destPos, length);
}

//' Works like System.arraycopy(...) in Java for double vectors.
//' @export
// [[Rcpp::export]]
void copyDoubles(const DoubleVector src,
                 const std::size_t srcPos,
                 DoubleVector dest,
                 const std::size_t destPos,
                 const std::size_t length) {
  arraycopy(src, srcPos, dest, destPos, length);
}

//' Computes number of work days (1-5/Mon-Fri) fot the total number of days and
//' starting with the initial day of week (1-7/Mon-Sun).
//' @export
// [[Rcpp::export]]
std::size_t countWorkdays(const std::size_t dayOfWeek, const std::size_t daysCount) {
  std::size_t count = 0;
  std::size_t day = dayOfWeek;
  for (std::size_t i = 0; i < daysCount; i++) {
    if (day < 6) count++;
    if (day == 7)
      day = 1;
    else
      day++;
  }
  return count;
}
