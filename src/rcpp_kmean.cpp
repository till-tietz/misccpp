#include <RcppArmadillo.h>
#include <vector>
#include <math.h>
#include <stdio.h>
using namespace Rcpp;

// function to assign clusters to points 
// [[Rcpp::export]]
std::vector<int> assign_cluster(arma::mat points, arma::mat centroids) {
  
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
std::vector<int> find_item(std::vector<int> const &vec, int target) {
  std::vector<int> indices;
  
  for (int i = 0; i < vec.size(); ++i) {
    if (vec[i] == target) {
      indices.push_back(i);
    }
  }
  
  return indices;
}

// function to compute new centroids for clusters 
// [[Rcpp::export]]
arma::mat new_centroid(arma::mat points, std::vector<int> assigned_cluster, arma::mat centroids) {
  
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
  return centroids;
}

// [[Rcpp::export]]
List kmean(arma::mat points, arma::mat centroids, int max_iter) {
  
  std::vector<int> cluster = assign_cluster(points, centroids);
  
  int iter = 0;
  bool run = true;
  
  while(run) {
    iter += 1;
    centroids = new_centroid(points, cluster, centroids);
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


