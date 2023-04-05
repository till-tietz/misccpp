#include <RcppArmadillo.h>
#include <algorithm>
#include <random>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//' ergodic process simulator
//' @param agents integer specifying the number of agents 
//' @param rounds integer specifying the number of rounds
//' @param money double specifying initial money for agents 
//' @param win double specifying multiplier for money given a win (should be strictly positive)
//' @param loss double specifying multiplier for money given a loss (should be strictly positive)
//' @param prob double specifying probability of a win 
//' @return a matrix of agent money per round where rows are rounds and columns are agents 
// [[Rcpp::export]]
Rcpp::List ergodic_sim(int agents,
                       int rounds,
                       double money,
                       double win,
                       double loss,
                       double prob) {
  //storing money per agent iteration
  std::vector<std::vector<double>> money_mat(agents);
  //storing bust values
  std::vector<bool> bust;
  bust.reserve(agents);
  std::vector<int> bust_iter;
  bust_iter.reserve(agents);
  
  //run simulation for each agent
  for(int i = 0; i < agents; ++i) {
    std::vector<double> money_i;
    money_i.reserve(rounds);
    
    money_i.push_back(money);
    bust.push_back(false);
    
    bool run = true;
    int round = 0;
    //run simulation until agent goes bust 
    while(run) {
      round += 1;
      bool won = prob < (double) std::rand()/RAND_MAX;
      
      if(won) {
        double m_i = money_i[round - 1] + money_i[round - 1] * win;
        money_i.push_back(m_i);
      } else {
        double m_i = money_i[round - 1] - money_i[round - 1] * loss;
        money_i.push_back(m_i);
      }
      
      if(money_i[round] <= 0) {
        bust[i] = true;
        bust_iter.push_back(round);
        run = false;
      }
      
      if(round == rounds) {
        run = false;
      }
    }
    
    money_mat[i] = money_i;
  }
  
  //compute mean bust iteration 
  double mean_bust_iter = 0.0;
  for(int i = 0; i < bust_iter.size(); ++i) {
    mean_bust_iter += bust_iter[i];
  }
  mean_bust_iter = mean_bust_iter / bust_iter.size();
  
  //return results 
  Rcpp::List out = List::create(Named("sim_results") = money_mat,
                                _["bust"] = bust,
                                _["mean_bust_iter"] = mean_bust_iter);
  return out;
}




