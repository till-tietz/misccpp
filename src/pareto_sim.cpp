#include <Rcpp.h>
#include <algorithm>
#include <random>
using namespace Rcpp;

//' pareto process simulator 
//' @param pop_size integer specifying the number of agents
//' @param mon integer vector specifying initial money for each agent
//' @param prop double vector specifying the win probability for each agent 
//' @param iter integer specifying the number of iterations to run 
//' @return a list of agent money vectors 
// [[Rcpp::export]]
std::vector<std::vector<int>> pareto_sim(int pop_size,
                                         std::vector<int> mon,
                                         std::vector<double> prop,
                                         int iter){  
  //generate vector of agent indices
  std::vector<int> pop(pop_size);
  for(int i = 0; i < pop.size(); ++i){
    pop[i] = i;
  }
  
  //generate vector of vectors to store money of each iteration
  std::vector<std::vector<int>> lmoney(iter);
  lmoney[0] = mon;
  
  //begin simulation
  for(int i = 1; i < iter; ++i) {
    lmoney[i] = lmoney[i - 1];
    
    //erase all agents with 0 money
    //we need a while loop as we are erasing elements of the vector we are iterating over 
    auto it = pop.begin();
    while(it != pop.end()){
      if(lmoney[i - 1][*it] <= 0){
        pop.erase(it);
      } else {
        ++it;
      }
    }
    
    //shuffle vector of agents 
    //shuffeling gives us random combinations when we pair agents
    std::random_shuffle(pop.begin(),pop.end());
    
    //iterate through agents, form pairs and play coin-toss 
    for(int a = 0; a < pop.size() - 1; ++a){
      //start at 0 index, when index a is even take the next element and generate pair
      if((a % 2) == 0){
        
        //relative win probabilities for each agent 
        //(ensures that win probabilities of pair sum to 1)
        double prob1 = prop[pop[a]] / (prop[pop[a]] + prop[pop[a + 1]]);
        double prob2 = prop[pop[a + 1]] / (prop[pop[a]] + prop[pop[a + 1]]);
        
        //determine who wins 
        int win1 = prob1 < (double) std::rand()/RAND_MAX;
        int win2 = prob2 < (double) std::rand()/RAND_MAX;
        
        //allocate money 
        if(win1 < win2){
          lmoney[i][pop[a]] = lmoney[i][pop[a]] - 1;
          lmoney[i][pop[a + 1]] = lmoney[i][pop[a + 1]] + 1;
        } 
        
        if(win1 > win2){
          lmoney[i][pop[a]] = lmoney[i][pop[a]] + 1;
          lmoney[i][pop[a + 1]] = lmoney[i][pop[a + 1]] - 1;
        }
      }
    }
  }
  return lmoney;
}

