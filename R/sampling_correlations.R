
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
get_r_samples <- function(pop.r = NA, n, number.of.samples = 100, number.of.decimals = 3, seed.value = NULL) {

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
  }
  xx<-1:number.of.samples
  sample.number <- xx
  data.out <- data.frame(sample.number, pop.r = pop.r, n = n, r =  rs, LL = LLs, UL = ULs, ci.captured.pop.r = in_interval, p = ps)
  rownames(data.out) <- NULL


  pi_in_interval[i] <- is_value_in_interval(pop.r, c(r_info$conf.int[1], r_info$conf.int[2]))



  return(data.out)
}



#' Calculate a number of sample correlations based on a specified population correlation
#' @param pop.r Population correlation. Do not use pop.data is you provide this value.
#' @param n Sample size for all samples If you use n, do not use n.min or n.max.
#' @param number.of.samples Number of samples to obtain
#' @param number.of.decimals Number of decimals to report in returned data frame
#' @param seed.value random number seed value
#' @param n.min Use various sample sizes. Uniform distribution min.
#' @param n.max Use various sample sizes. Uniform distribution max
#' @param label Label column for all studies. Useful when combining multiple simulations.
#' @return Data frame with sample correlations
#' @examples
#' get_r_samples(pop.r = .35,n=100)
#' @export
get_r_samples_fast <- function(pop.r = NA, n, number.of.samples = 100,
                               number.of.decimals = 3, seed.value = NULL,
                               n.min = NULL, n.max = NULL, label = NULL,
                               alternative = "two.sided") {

  if (!is.null(seed.value)) {
    set.seed(seed.value)
  }

  if (!is.null(n.min) & !is.null(n.max)) {
    n <- abs(round(runif(min = n.min, max = n.max, n = number.of.samples)))
    n[n < 4] <- (n.max + n.min)/2# if too small use mean.n
  } else {
    n = rep(n, number.of.samples)
  }


  #make-zs
  pop.z = samplingTutorial:::r_to_z(pop.r)
  pop.se.z = samplingTutorial:::r_to_z_se(n)
  sample.z = rnorm(mean = pop.z, sd = pop.se.z, n = number.of.samples)
  sample.r = samplingTutorial:::z_to_r(sample.z)

  rs <- round(sample.r,2)
  dfs <- n - 2L
  ts <- r_tvalue(rs, n)
  ps <- rep(NA, number.of.samples)
  LLs <- r_ci_LL(rs,n)
  ULs <- r_ci_UL(rs,n)
  in_interval <- rep(NA, number.of.samples)
  for (i in 1:number.of.samples) {
    ps[i] <- r_pvalue(ts[i], n[i])
    in_interval[i] <- is_value_in_interval(pop.r, c(LLs[i], ULs[i]))
  }

  xx<-1:number.of.samples
  sample.number <- xx
  data.out <- data.frame(sample.number, pop.r = pop.r, n = n,
                         r = rs, number.of.decimals,
                         LL = round(LLs, number.of.decimals),
                         UL = round(ULs, number.of.decimals), ci.captured.pop.r = in_interval,
                         p = round(ps,number.of.decimals+2))
  rownames(data.out) <- NULL



  return(data.out)
}
