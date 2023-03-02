#include <RcppArmadillo.h>
#include <vector>
#include <math.h>
#include <stdio.h>
using namespace Rcpp;

// function to assign clusters to points 
std::vector<int> assign_cluster(const arma::mat& points, const arma::mat& centroids) {
  
  int n_dim = points.n_cols;
  int n_centroids = centroids.n_rows;
  int n_points = points.n_rows;
  
  std::vector<int> cluster;
  cluster.reserve(n_points);
  
  std::vector<int> dist(n_centroids);
  
  for(int i = 0; i < n_points; ++i) {
    for(int j = 0; j < n_centroids; ++j) {
      double sum_dist = 0.0;
      for(int k = 0; k < n_dim; ++k) {
        sum_dist += std::pow((points(i,k) - centroids(j,k)), 2);
      }
      dist[j] = sum_dist;
    }
    auto min_pos = std::min_element(std::begin(dist), std::end(dist));
    cluster.push_back(min_pos - dist.begin());
  }
  return cluster;
}




// helper to find indices of all occurrences of target in vector 
std::vector<int> find_item(const std::vector<int>& vec, int target) {
  std::vector<int> indices;
  
  for (int i = 0; i < vec.size(); ++i) {
    if (vec[i] == target) {
      indices.push_back(i);
    }
  }
  
  return indices;
}




// function to compute new centroids for clusters 
void new_centroid(const arma::mat& points, const std::vector<int>& assigned_cluster, arma::mat& centroids) {
  
  //number of dimensions
  int n_dim = points.n_cols;
  
  //get unique clusters 
  std::vector<int>::iterator it;
  std::vector<int> clusters = assigned_cluster;
  std::sort(clusters.begin(), clusters.end());
  it = std::unique(clusters.begin(), clusters.end());
  clusters.resize(std::distance(clusters.begin(), it));
  
  for(int i = 0; i < clusters.size(); ++i) {
    //find indices of points assinged to cluster 
    int cluster = clusters[i];
    std::vector<int> indices = find_item(assigned_cluster, cluster);
    //sum coordinates of each point in cluster
    std::vector<double> coords(n_dim,0.0);
    for(int j = 0; j < indices.size(); ++j) {
      for(int k = 0; k < n_dim; ++k) {
        coords[k] += points(indices[j],k);
      }
    }
    //compute average of point coordinates in cluster 
    int n_points_clust = indices.size();
    for(int j = 0; j < n_dim; ++j) {
      coords[j] = coords[j] / n_points_clust;
    }
    
    centroids.row(clusters[i]) = arma::rowvec(coords);
  }
  return;
}




//' kmean 
//' @param points a matrix where rows are individual points and columns are their coordinates 
//' @param k integer specifying the number of clusters to search for 
//' @param max_iter integer specifying the maximum number of iterations to run the algorithm for 
//' @return a list with an integer vector specifying the cluster assignment of points and a matrix with centroid point coordinates for each cluster
// [[Rcpp::export]]
List kmean(arma::mat points, int k, int max_iter) {
  
  int n_dim = points.n_cols;
  //get ranges of points  
  std::vector<double> col_min = arma::conv_to<std::vector<double>>::from(arma::min(points, 0));
  std::vector<double> col_max = arma::conv_to<std::vector<double>>::from(arma::max(points, 0));
  //make centroid matrix
  arma::mat centroids(k,n_dim);
  //set random number generator
  std::mt19937 engine;
  //generate random centroids
  for(int i = 0; i < n_dim; ++i) {
    std::uniform_real_distribution<double> distr(col_min[i], col_max[i]);
    std::vector<double> coords;
    coords.reserve(k);
    
    for(int j = 0; j < k; ++j) {
      coords.push_back(distr(engine));
    }
    
    centroids.col(i) = arma::vec(coords);
  }
  
  //assign initial clusters
  std::vector<int> cluster = assign_cluster(points, centroids);
  //run algorithm
  int iter = 0;
  bool run = true;
  
  while(run) {
    iter += 1;
    new_centroid(points, cluster, centroids);
    std::vector<int> cluster_new = assign_cluster(points, centroids);
    
    bool cluster_change;
    for(int i = 0; i < cluster.size(); ++i) {
      cluster_change = cluster_new[i] != cluster[i];
      if(cluster_change) {
        break;
      }
    }
    
    cluster = cluster_new;
    
    if((iter == max_iter) || (!cluster_change)) {
      run = false;
    }
  }
  List out = List::create(Named("cluster_assign") = cluster, _["cluster_coord"] = centroids);
  return out;
}


