#include <RcppArmadillo.h>
#include <algorithm>
#include <random>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//' ergodic process simulator
//' @param agents integer specifying the number of agents 
//' @param rounds integer specifying the number of rounds
//' @param money integer specifying initial money for agents 
//' @param win double specifying multiplier for money given a win (should be strictly positive)
//' @param loss double specifying multiplier for money given a loss (should be strictly positive)
//' @param prob double specifying probability of a win 
//' @return a matrix of agent money per round where rows are rounds and columns are agents 
// [[Rcpp::export]]
arma::mat ergodic_sim(int agents,
                      int rounds,
                      int money,
                      double win,
                      double loss,
                      double prob) {
  arma::mat out(rounds,agents);
  out.row(0).fill(money);
  
  for(int i = 0; i < agents; ++i) {
    for(int j = 1; j < rounds; ++j) {
      
      bool won = prob < (double) std::rand()/RAND_MAX;
      double prev = out(j-1,i);
      
      if(won) {
        out(j,i) = prev + prev * win;
      } else {
        out(j,i) = prev - prev * loss;
      }
      
    }
  }
  return out;
}



