
#' Calculate a number of sample correlations based on a specified population correlation
#' @param pop.r Population correlation. Do not use pop.data is you provide this value.
#' @param n Sample size for all samples If you use n, do not use n.min or n.max.
#' @param number.of.samples Number of samples to obtain
#' @param number.of.decimals Number of decimals to report in returned data frame
#' @param seed.value random number seed value
#' @return Data frame with sample correlations
#' @examples
#' get_r_samples(pop.r = .35,n=100)
#' @export
get_r_samples <- function(pop.r = NA, n, number.of.samples = 10, number.of.decimals = 3, seed.value = NULL) {

  if (!is.null(seed.value)) {
    set.seed(seed.value)
  }

  Sigma <- diag(2)
  Sigma[1,2] <- pop.r
  Sigma[2,1] <- pop.r
  mu <- c(0,0)
  K <- number.of.samples
  data_samples <- fast_mvrnorm(Sigma, mu, n, K)

  rs <- rep(NA,number.of.samples)
  dfs <- rep(NA,number.of.samples)
  ts <- rep(NA,number.of.samples)
  in_interval <- rep(NA, number.of.samples)
  ps <- rep(NA,number.of.samples)
  LLs <- rep(NA,number.of.samples)
  ULs <- rep(NA,number.of.samples)
  pi_LLs <- rep(NA,number.of.samples)
  pi_ULs <- rep(NA,number.of.samples)
  pi_in_interval <- rep(NA, number.of.samples)
  ci_as_pi_in_interval <- rep(NA, number.of.samples)
  for (i in 1:number.of.samples) {
    x <- data_samples$x_true[,i]
    y <- data_samples$y_true[,i]
    r_info <- stats::cor.test(x,y)
    rs[i] <- round(r_info$estimate,number.of.decimals)
    LLs[i] <- round(r_info$conf.int[1],number.of.decimals)
    ULs[i] <- round(r_info$conf.int[2],number.of.decimals)
    in_interval[i] <- is_value_in_interval(pop.r, c(r_info$conf.int[1], r_info$conf.int[2]))
    ts[i] <- round(r_info$statistic,number.of.decimals)
    dfs[i] <- round(r_info$parameter,number.of.decimals)
    ps[i] <- round(r_info$p.value,5)
    pi_info <- predictionInterval::pi.r(r = rs[i], n = n, rep.n = n)
    pi_LLs[i] <- round(pi_info$lower_prediction_interval, number.of.decimals)
    pi_ULs[i] <- round(pi_info$upper_prediction_interval, number.of.decimals)
    if (i > 1) {
      pi_in_interval[i-1] <- is_value_in_interval(rs[i], c(pi_LLs[i-1], pi_ULs[i-1]))
      ci_as_pi_in_interval[i-1]  <- is_value_in_interval(rs[i], c(LLs[i-1], ULs[i-1]))
    }
  }
  xx<-1:number.of.samples
  sample.number <- xx
  data.out <- data.frame(sample.number, pop.r = pop.r, n = n, r =  rs, ci.LL = LLs, ci.UL = ULs, ci.captures.pop.r = in_interval, p = ps)
  rownames(data.out) <- NULL


  pi_in_interval[i] <- is_value_in_interval(pop.r, c(r_info$conf.int[1], r_info$conf.int[2]))



  return(data.out)
}
