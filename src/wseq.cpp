#include <Rcpp.h>
using namespace Rcpp;

std::vector<int> wseq_int(int max, int i, int size, bool clamped) {
  int half = ceil(size/2);
  int n = half * 2 + 1;
  i -= half;
  if (!clamped) {
    if (i < 0) i = 0;
    if (i > max - n) i = max - n;
  }
  std::vector<int> idx(n);
  for (int k = 0; k < n; k++) idx[k] = (max + (k + i) % max) % max;
  return(idx);
}

template <typename T>
std::vector<T> wseq_vec(const std::vector<T> &x, int i, int size, bool clamped) {
  std::vector<int> idx = wseq_int(x.size(), i, size, clamped);
  std::vector<T> ret(idx.size());
  for(size_t j = 0; j < idx.size(); j++) ret[j] = x[idx[j]];
  return(ret);
}

template <int RTYPE>
Vector<RTYPE> wseq_imp(const Vector<RTYPE>& x, int i, int size, bool clamped) {
  std::vector<int> idx = wseq_int(x.size(), i, size, clamped);
  IntegerVector id(idx.begin(), idx.end());
  return(x[id]);
}

// [[Rcpp::export]]
SEXP wseq_cpp(const SEXP& x, int i, int size, bool clamped = true) {
  RCPP_RETURN_VECTOR(wseq_imp, x, i, size, clamped);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/
