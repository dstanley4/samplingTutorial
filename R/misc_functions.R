is_value_in_interval <- function(value, interval) {
  is_in_interval <- FALSE
  check_interval<-findInterval(value,sort(interval),rightmost.closed = TRUE)
  if (check_interval==1) {
    is_in_interval <- TRUE
  }
  return(is_in_interval)
}

#' Calculate the percentage of rows that are true in a column
#' @param sample_statistic the column to be examined
#' @param middle_percent the desired middle percentage (default is 95)
#' @return values that bound the specified middle percent
#'@export
get_middle_percent <- function(sample_statistic, middle_percent = 95) {
  middle_prop <- round(middle_percent/100,2)
  start_prop <- (1 - middle_prop)/2
  end_prop <-  1 - start_prop
  sample_statistic = sort(sample_statistic)
  n = length(sample_statistic)
  xstart_n <- as.integer(n) * start_prop
  xstart_n <- round(xstart_n + 1)
  xstart_n <- as.integer(xstart_n)
  xend_n <- as.integer(n * end_prop)
  lower <- sample_statistic[xstart_n]
  upper <- sample_statistic[xend_n]
  output <- c(lower, upper)
  return(output)
}


#' Calculate the percentage of rows that are true in a column
#' @param x the column to be examined
#' @return The percent equal to TRUE
#'@export
ci_percent_true <- function(x) {
  sum_TRUE <- sum(x, na.rm = TRUE)
  sum_length <- sum(!is.na(x))
  return(sum_TRUE/sum_length*100)
}


#' Probability of a p-value in a specified range
#' @param data simulation data
#' @param min lower end of range (equal to or greater than)
#' @param data upper end of range (less than)
#' @return The probability
#' @export
p_range_count <- function(data,min,max) {
  num_rows <- dim(data)[1]
  id_greater <- data$p>= min
  id_less <- data$p<max
  id_both <- id_greater & id_less
  sum_both <- sum(id_both)
  prob_value = sum_both/num_rows


  output <- list()
  output$text <- sprintf("%g of %g sampes = %1.3f probabilty", sum_both, num_rows, prob_value)
  output$count <- sum_both
  output$prob <- prob_value
  class(output) <- "pvaluetutorialdata"

  return(output)
}

#' @export
print.pvaluetutorialdata<- function(x,...) {
  cat(x$text,"\n")
  cat("\n")
}






fast_mvrnorm <- function(Sigma, mu, n, K) {
  # This is the MASS::mvrnorm routine structured to eliminate
  # redundant calculations when repeated K times

  #Matrix approach for fast bivariate simulations

  p <- length(mu)
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values

  Xmulti <-  eS$vectors %*% diag(sqrt(pmax(ev, 0)), p)

  xs <- matrix(NA,n,K)
  ys <- matrix(NA,n,K)
  for (i in 1:K) {
    X <- matrix(rnorm(p * n), n)
    X2 <- Xmulti %*% t(X)
    X2 <- t(X2)
    xs[,i] <- X2[,1]
    ys[,i] <- X2[,2]
  }

  output <- list()
  output$x_true <- xs
  output$y_true <- ys
  return(output)
}
