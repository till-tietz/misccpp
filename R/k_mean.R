#' k-mean clustering algorithm 
#' @param points a numeric m x n matrix with m observations defined by n coordinate values. 
#' @param k integer, number of clusters 
#' @param max_iter integer, maximum number of clustering optimization iterations 
#' @param n_init integer, number of random initial cluster starting points to try
#' @return a list of squared errors, cluster assignments and cluster centroid coordinates for each iteration
#' @export

k_mean <- function(points, k, max_iter = 500L, n_init = 5L){
  val_range <- rbind(apply(points,2,min),apply(points,2,max))
  
  res <- vector(mode = "list", length = n_init)
  min_error <- rep(NA,n_init)
  
  for(i in 1:length(res)){
    centroid <- apply(val_range,2, function(i) runif(k, i[1], i[2]))
    
    res_i <- kmean(points = points, centroids = centroid, k = k, max_iter = max_iter)
    res_i <- lapply(res_i, function(i) i[1:length(which(res_i[[1]] > 0)) - 1])
    min_error[i] <- min(res_i[[1]])
    res[[i]] <- res_i
  }
  return(res[[which.min(min_error)]])
}  