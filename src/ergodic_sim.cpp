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
Rcpp::List ergodic_sim(const int& agents,
                       const int& rounds,
                       const double& money,
                       const double& win,
                       const double& loss,
                       const double& prob) {
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



//' gamblers ruin simulation 
//' @param n_runs integer number of simulations
//' @param money_start double initial money 
//' @param money_end double money to be achieved 
//' @param p_win double win probability 
//' @param risk_adjust double proportion of money to bet
//' @param adaptive bool indicating whether to use adaptive betting (based on bayesian estimation of win probability)
//' @param prior_beta integer vector holding alpha and beta parameters of the p_win prior 
//' @return a List holding a vector indicating rounds and a vector indicating busts for each run 
// [[Rcpp::export]]
Rcpp::List ruin_sim(const int& n_runs,
                    const double& money_start,
                    const double& money_end,
                    const double& p_win,
                    const double& risk_adjust,
                    const bool& adaptive,
                    const std::vector<int>& prior_p_win) {
  std::vector<int> rounds;
  rounds.reserve(n_runs);
  std::vector<bool> bust;
  bust.reserve(n_runs);
  
  for(int i = 0; i < n_runs; ++i) {
    double money_i = money_start;
    bool bust_i = false;
    int rounds_i = 0;
    bool run = true;
    int a = prior_p_win[0];
    int b = prior_p_win[1];
    double p;
    double bet;
    
    while(run) {
      rounds_i += 1;
      bool won = p_win < (double) std::rand()/RAND_MAX;
      
      if (adaptive) {
        if (won) {
          a += 1.0;
        } else {
          b += 1.0;
        }
        p = a / (a + b);
      }
      
      double bet = adaptive ? (p < 0.5 ? 1.0 : money_i * risk_adjust) : money_i * risk_adjust;
      
      
      if(won) {
        money_i = money_i + bet;
      } else {
        money_i = money_i - bet;
      }
      
      if(money_i <= 0) {
        bust_i = true;
        run = false;
      }
      
      if(money_i >= money_end) {
        run = false;
      }
    }
    
    rounds.push_back(rounds_i);
    bust.push_back(bust_i);
  }
  
  Rcpp::List out = List::create(Named("rounds") = rounds,
                                _["bust"] = bust);
  return out;
}































