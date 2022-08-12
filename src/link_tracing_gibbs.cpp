#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector int_vec_insert(IntegerVector vec,
                             IntegerVector vals,
                             IntegerVector pos){

  for(int i = 0; i < vals.size(); i++){
    int pos_i = pos[i];
    vec[pos_i] = vals[i];
  }
  return vec;
}

// [[Rcpp::export]]
IntegerMatrix int_mat_insert(IntegerMatrix m,
                             IntegerVector col,
                             IntegerVector row,
                             IntegerVector vals){

  int nrow = m.nrow();
  IntegerVector pos = ((col - 1) * nrow) + row - 1;

  for(int i = 0; i < pos.length(); i++){
    m[pos[i]] = vals[i];
  }

  return m;
}


// [[Rcpp::export]]
IntegerMatrix mat_to_mat_insert(IntegerMatrix old_m,
                                IntegerMatrix new_m,
                                IntegerVector new_rows,
                                IntegerVector new_cols,
                                IntegerVector old_rows,
                                IntegerVector old_cols){

  for(int i = 0; i < new_rows.length(); i++){
    for(int j = 0; j < new_cols.length(); j++){
      new_m(new_rows[i] - 1, new_cols[j] - 1) = old_m(old_rows[i] - 1, old_cols[j] - 1);
    }
  }
  return new_m;
}

// [[Rcpp::export]]
NumericVector mat_by_mat(NumericMatrix m,
                         IntegerVector row,
                         IntegerVector col){
  NumericVector res(row.length());

  for(int i = 0; i < res.length(); i++){
    res[i] = m(row[i] - 1, col[i] - 1);
  }

  return res;
}

// make entirely c++ based without return to R 
// use array of integer vectors instead of lists to hold indices of units for each wave 
// [[Rcpp::export]]
List lt_permute(List link_list,
                IntegerVector wave,
                IntegerVector name){

  Function c_unlist("unlist");

  int n_inital = sum(wave == 1);
  int n_waves = table(wave).length();

  List wave_samples(n_waves);
  IntegerVector s0 = sample(as<IntegerVector>(name),n_inital, false, R_NilValue);
  wave_samples(0) = s0;

  for(int i = 1; i < n_waves; i++){
    LogicalVector get_elem =  in(name,as<IntegerVector>(wave_samples[i-1]));
    List l_i = link_list[get_elem];
    IntegerVector set1 = unique(as<IntegerVector>(c_unlist(l_i)));
    IntegerVector set2 = c_unlist(wave_samples[Range(0,i - 1)]);
    wave_samples(i) = setdiff(set1,set2);
  }
  return wave_samples;
}




// [[Rcpp::export]]
List lt_gibbs(DataFrame data,
              IntegerMatrix y_samp,
              IntegerVector strata,
              int n_strata,
              int n_waves,
              int total,
              int chain_samples,
              int chain_burnin,
              List priors,
              List param_init) {

  Function c_unlist("unlist");
  Function c_expand_grid("expand.grid");
  Function c_combn("combn");
  Function c_setdiff("setdiff");
  Function c_which("which");
  Function c_rdirichlet("rdirichlet");

  // permute data
  List data_p_waves = lt_permute(data["links_list"],data["rds_wave"],data["name"]);
  DataFrame data_p = data;

  // reordering samples to estimate N
  IntegerVector n_p(n_waves);

  for(int i = 0; i < n_waves; i++){
    n_p[i] = as<IntegerVector>(data_p_waves[i]).size();
  }

  List data_p_reorder(n_waves);
  data_p_reorder[0] = Range(1,n_p[0]);

  for(int i = 1; i < n_waves; i++){
    data_p_reorder[i] = Range(sum(head(n_p,i)) + 1, sum(head(n_p, i + 1)));
  }

  //assign seeds
  List l(chain_samples);
  List b(chain_samples);
  IntegerVector n(chain_samples);

  l(0) = as<NumericVector>(param_init["l_0"]);
  b(0) = as<NumericMatrix>(param_init["b_0"]);
  n[0] = param_init["n_0"];

  int prior_n = priors["p_n"];
  NumericVector prior_l = priors["p_l"];
  int prior_b = priors["p_b"];

  int t = 1;

  // begin MCMC
//  for(int t = 1; t < chain_samples; t++){

    //##################
    //# generate new N #
    //##################

    //get number of units in each strata
    IntegerVector data_p_strata = data_p["strata"];
    IntegerVector rows = c_unlist(head(data_p_waves, n_waves - 1));
    IntegerVector rows_pull = rows - 1;

    IntegerVector strata_t(rows_pull.size());

    for(int i = 0; i < rows_pull.size(); i++){
      strata_t[i] = data_p_strata[rows_pull[i]];
    }

    IntegerVector strata_count = table(strata_t);

    //get p(no link between strata
    NumericVector one = {1};
    NumericVector no_link_init =  rep(one,n_strata);

    for(int i = 0; i < n_strata; i++){
      for(int j = 0; j < n_strata; j++){
        NumericMatrix bi = b[t - 1];
        no_link_init[i] = no_link_init[i] * pow((1 - bi(j,i)),strata_count[j]);
      }
    }

    NumericVector no_link_l = as<NumericVector>(l[t-1]) * no_link_init;
    double no_link = sum(no_link_l);

    int nn_0 = as<IntegerVector>(data_p_waves[0]).size();
    int nn = as<IntegerVector>(c_unlist(data_p_waves)).size();

    IntegerVector n_post_range = Range(nn, total * 5);

    NumericVector n_sample_prob_vec(n_post_range.size());

    for(int i = 0; i < n_post_range.size(); i++){
     IntegerVector r_i = Range(n_post_range[i] + 1 - nn, n_post_range[i] - nn_0);
     n_sample_prob_vec[i] =  sum(log(r_i)) +
       (n_post_range[i] - nn) * log(no_link) - prior_n * log(n_post_range[i]);
    }

    NumericVector n_sample_prob = exp(n_sample_prob_vec - max(n_sample_prob_vec));
    n[t] = sample(n_post_range, 1, false, n_sample_prob)[0];


    //#######################
    //# generate new lambda #
    //#######################

    // assign strata to non sampled units

    // get indices of non sampled units
    IntegerVector n_range = Range(1, n[t]);
    IntegerVector not_sampled = setdiff(n_range, as<IntegerVector>(c_unlist(data_p_reorder))) - 1;

    IntegerVector stratum(n[t]);

    //fill stratum vector with strata of sampled units
    IntegerVector ins_pos_us = as<IntegerVector>(c_unlist(data_p_reorder));
    ins_pos_us = ins_pos_us - 1;
    rows_pull = as<IntegerVector>(c_unlist(data_p_waves));
    rows_pull = rows_pull - 1;
    IntegerVector ins_val_us(rows_pull.size());

    for(int i = 0; i < rows_pull.size(); i++){
      ins_val_us[i] = data_p_strata[rows_pull[i]];
    }

    stratum = int_vec_insert(as<IntegerVector>(stratum),
                             as<IntegerVector>(ins_val_us),
                             as<IntegerVector>(ins_pos_us));

    //fill stratum vector with strata of non sampled units
    IntegerVector strat = Range(1,n_strata);
    NumericVector pstrat = no_link_l / no_link;
    IntegerVector stratsamp = sample(as<IntegerVector>(strat), not_sampled.size(),
                                     true, as<NumericVector>(pstrat));

    stratum = int_vec_insert(as<IntegerVector>(stratum),
                             as<IntegerVector>(stratsamp),
                             as<IntegerVector>(not_sampled));



    // populate link matrix for reordered sample
    DataFrame link_comb = c_expand_grid(Range(0,n_waves - 1), Range(0,n_waves - 1));
    IntegerVector g1 = link_comb[0];
    IntegerVector g2 = link_comb[1];

    IntegerMatrix y(n[t],n[t]);

    for(int i = 0; i < g1.size() - 1; i++){
      y = mat_to_mat_insert(y_samp,
                            y,
                            data_p_reorder[g1[i]],
                            data_p_reorder[g2[i]],
                            data_p_waves[g1[i]],
                            data_p_waves[g2[i]]);
    }

    IntegerVector lp_1 = c_unlist(data_p_reorder[Range(0,data_p_reorder.size() - 2)]);
    IntegerVector lp_2 = Range(1,n[t]);
    IntegerMatrix link_pairs =  c_combn(c_setdiff(lp_2,lp_1),2);
    link_pairs  = transpose(link_pairs);

    int n_pairs = link_pairs.nrow();
    NumericVector link_prob = runif(n_pairs);

    IntegerVector assigned = c_which(link_prob < mat_by_mat(b[t],
                                                            stratum[link_pairs(_,0)],
                                                            stratum[link_pairs(_,1)]));

    IntegerVector link_pairs_0 = link_pairs(_,0);
    IntegerVector link_pairs_1 = link_pairs(_,1);
    IntegerVector vals (link_pairs_0.length(),1);

    y = int_mat_insert(y,
                       link_pairs_1[assigned - 1],
                       link_pairs_0[assigned - 1],
                       vals);

    y = int_mat_insert(y,
                       link_pairs_0[assigned - 1],
                       link_pairs_0[assigned - 1],
                       vals);


    // new lambda
    NumericVector strata_count_num = as<NumericVector>(table(stratum));
    IntegerVector strate_count_int = table(stratum);
    NumericMatrix dirichlet = c_rdirichlet(1, strata_count_num + prior_l);
    l(t) = dirichlet(0,_);

    //#####################
    //# generate new beta #
    //#####################



  }

  List ret(3);
  ret[0] = data_p_reorder;
  ret[1] = data_p_waves;
  ret[2] = ins_pos_us;
  return ret;
}

