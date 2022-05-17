#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector rm_el(IntegerVector input){
  IntegerVector ret(input.size() - 1);
  for(int i = 1; i < input.size(); i++){
    ret[i - 1] = input[i];
  }
  return ret;
}

// [[Rcpp::export]]
IntegerMatrix gen_pairs(IntegerVector input){
  Function cseq("seq");
  IntegerVector v = sample(input, input.size(), false, R_NilValue);
  
  if(v.size() % 2 != 0){
    v = rm_el(v);
  }
  
  IntegerVector start = cseq(0, v.size() - 1, 2);
  
  IntegerMatrix pairs(start.size(), 2);
  
  for(int i = 0; i < start.size(); i++){
    int start_i = start[i];
    int end_i = start[i] + 1;
    
    int val1 = v[start_i];
    int val2 = v[end_i];
    
    pairs(i,0) = val1;
    pairs(i,1) = val2;
  }
  
  
  return pairs;
  
}

// [[Rcpp::export]]
IntegerVector extract(NumericMatrix m, IntegerVector r, int c){
  IntegerVector ret(r.size());
  for(int i = 0; i < r.size(); i++){
    ret[i] = m(i,c);
  }
  return ret;
}


// [[Rcpp::export]]
List pareto_sim(IntegerVector pop, IntegerVector mon, double prob, int iter){
  
  Function crbinom("rbinom");
  Function cwhich("which");
  
  List lmoney(iter);
  lmoney[0] = mon;
  
  for(int i = 1; i < iter; i++){
    
    IntegerVector moneyprev = lmoney[i - 1];
    IntegerVector money = clone(moneyprev);
    IntegerVector keepl = cwhich(money > 0);
    IntegerVector keep = keepl - 1;
    
    IntegerVector pop_i(keep.size());
    
    for(int r = 0; r < keep.size(); r++){
      int keepint = keep[r];
      pop_i[r] = pop[keepint];
    }
    
    IntegerMatrix pairs = gen_pairs(pop_i);
    
    IntegerVector wins = crbinom(pairs.nrow(),1,prob);
    
    for(int j = 0; j < pairs.nrow(); j++){
      
      int p1 = pairs(j,0) - 1;
      int p2 = pairs(j,1) - 1;
      
      if(wins[j] == 0){
        money[p1] = money[p1] - 1;
        money[p2] = money[p2] + 1;
      } else {
        money[p1] = money[p1] + 1;
        money[p2] = money[p2] - 1;
      }
    }
    lmoney[i] = money;
  }
  
  return lmoney;
}


