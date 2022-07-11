#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

NumericMatrix ergodic_sim(int agents, int rounds, int money, double win, double loss){
  
  NumericMatrix out(rounds,agents);
  
  IntegerVector first_col_index = seq(0, agents - 1);
  first_col_index = first_col_index * rounds;
  
  for (int i = 0; i < first_col_index.size(); i++){
    
    int index = first_col_index[i];
    out[index] = money;
    
  }
  
  for(int i = 0; i < agents; i++){
    
    for(int j = 1; j < rounds; j++){
      
      NumericVector won = rbinom(1,1,0.5);
      double prev = out(j-1,i);
      
      if(won[0] == 1){
        out(j,i) = prev + prev * win;
      } else {
        out(j,i) = prev - prev * loss;
      }
      
    }
    
  }
  
  return out;
}

