#' linear regression with gradient descent 
#' @param data a data.frame or matrix of data 
#' @param y a vector of the column name or index of the response variable
#' @param x a vector of column names or indeces of the predictor variables
#' @param lr dbl specifying learning rate
#' @param iter dbl specifying number of iterations 
#' @return a list including a named vector of estimated coefficients, a vector of intercept values for each iteration and a matrix of coefficients for each iteration. 
#' @export

lm_gd <- function(data, y, x, lr = 0.01, iter = 1000){
  
  y <- data[[y]]
  x <- as.matrix(data[,x])
  
  alpha <- rep(NA,iter)
  beta <- matrix(NA,iter,ncol(x))
  
  alpha[1] <- runif(1, -10,10)
  beta[1,] <- runif(ncol(beta), -10,10)
  
  for(i in c(2:iter)){
    
    b_prev <- beta[i-1,]
    a_prev <- alpha[i-1]
    gx <- (y - (a_prev + rowSums(x %*% diag(b_prev))))
    
    beta[i,] <- b_prev - lr * colMeans(-2 * x * gx)
    alpha[i] <- a_prev - lr * mean(-2 * gx)
    
  }
  
  coef <- c(alpha[iter],beta[iter,])
  names(coef) <- c("intercept",colnames(x, do.NULL = FALSE))
  
  return(
    list(coef = coef,
         alpha = alpha,
         beta = beta)
    )
  
}

#' data generator function
#' @param n int specifying number of observations to generate 
#' @param n_features int specifying number of variables to generate
#' @return a data.frame of variables  
#' @export


gen_data <- function(n = 1000, n_features = 10){
  sigma <- randcorr::randcorr(n_features)
  
  data <- as.data.frame(MASS::mvrnorm(n = n, mu = rep(0,n_features), Sigma = sigma))
  colnames(data) <- c("y", paste("x",c(1:(n_features - 1)),sep = "_"))
  return(data)
}




