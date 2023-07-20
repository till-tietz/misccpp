#include <omp.h>
#include <RcppArmadillo.h>
#include <unistd.h>
#include <vector>
#include <iostream>
#include <chrono>
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
int add_par(int n, int ncores) {
  int res = 0;
  omp_set_num_threads(ncores);
  #pragma omp parallel reduction(+: res)
  {
    #pragma omp for
    for(int i = 0; i < n; i++) {
      sleep(1);
      res += 1;
    }
  }
  return res;
}


// [[Rcpp::export]]
int add_seq(int n){
  int res = 0;
  
  for(int i = 0; i < n; i++) {
    sleep(1);
    res += 1;
  }
  
  return res;
}


int add_ints(int num1, int num2) {
  return num1 + num2;
}


// [[Rcpp::export]]
std::vector<int> write_vec_par(std::vector<int> x, int y, int ncores) {
  std::vector<int> res(x.size());
  // Copy elements of x into res
  std::copy(x.begin(), x.end(), res.begin()); 
  #pragma omp parallel num_threads(ncores) default(none) shared(res,y)
  {
    #pragma omp for
    for(int i = 0; i < res.size(); i++) {
       res[i] = add_ints(res[i], y);
    }
  }
  return res;
}
