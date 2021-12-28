#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]

List assign_cluster(NumericMatrix points, NumericMatrix centroids){
  int n_points = points.rows();
  int k = centroids.nrow();
  
  IntegerVector assigned_c (n_points);
  NumericVector distance_c (n_points);
  
  for(int i = 0; i < n_points; i++){
    NumericVector d (k);
    NumericVector p_i = points(i,_);
    
    for(int j = 0; j < k; j++){
      NumericVector c_j = centroids(j,_);
      NumericVector dif = p_i - c_j;
      NumericVector sq = dif * dif;
      double s = sum(sq);
      double ed = sqrt(s);
      d[j] = ed;
    }
    
    double min_d = min(d);
    distance_c[i] = min_d;
    
    int centroid_i = which_min(d);
    assigned_c[i] = centroid_i;
    
  }
  
  double sum_distance = sum(distance_c);
  List out = List::create(assigned_c,sum_distance);
  
  return out;
}

// [[Rcpp::export]]

NumericMatrix new_centroid(NumericMatrix points,IntegerVector assigned_c){
  Function cwhich("which");
  Function cnaomit("na.omit");
  IntegerVector c = unique(assigned_c);
  int dim = points.cols();
  int kn = c.length();
  
  NumericMatrix c_new(kn,dim);
  
  for(int i = 0; i < kn; i++){
    int c_i = c[i];
    LogicalVector pcl_i = assigned_c == c_i;
    IntegerVector pc_i = cwhich(pcl_i);
    
    int nres = pc_i.length() - 1;
    NumericMatrix pcp(nres,dim);
    
    for(int j = 0; j < nres; j++){
      int pc_j = pc_i[j];
      NumericVector pj = points(pc_j,_);
      pcp(j,_) = pj;
    }
    
    NumericVector md(dim);
    
    for(int l = 0; l < dim; l++){
      NumericVector pcp_l = pcp(_,l);
      md[l] = mean(pcp_l);
    }
    
    c_new(i,_) = md;
    
  }
  
  NumericMatrix c_out = cnaomit(c_new);
  return c_out;
}

// [[Rcpp::export]]

List kmean(NumericMatrix points, NumericMatrix centroids, int k, int max_iter){
  List init = assign_cluster(points, centroids);
  
  NumericVector error(max_iter);
  double error_init = init[1];
  error[0] = error_init;
  
  List assign(max_iter);
  assign[0] = init[0];
  
  List cs(max_iter);
  cs[0] = centroids;
  
  bool run = true;
  int i = 1;
  max_iter = max_iter - 1;
  
  while(run){
    int prev = i - 1;
    NumericMatrix centroid_i = new_centroid(points, assign[prev]);
    
    List cluster_i = assign_cluster(points, centroid_i);
    double error_i = cluster_i[1];
    
    if(error_i >= error[prev]){
      run = false;
    };
    
    if(max_iter == i){
      run = false;
    };
    
    assign[i] = cluster_i[0];
    cs[i] = centroid_i;
    error[i] = error_i;
    
    i++;
  }
  
  List out = List::create(Named("error") = error, _["cluster_assign"] = assign, _["cluster_coord"] = cs);
  return out;
}
