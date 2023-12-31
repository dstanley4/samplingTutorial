#' Calculate a number of sample d-values (unbiased) based on a specified (infinite) population correlation.
#' @param pop.d Population d-value
#' @param cell.n Cell size for both cells for all samples. If you use two values (e.g., c(20, 40), these represent -3/+3 SD for variable sample sizes
#' @param mean.n Use to specify mean of a distribution of sample sizes. Ignores cell.n if used.
#' @param sd.n Use to specify sd of a distribution of sample sizes. Ignores cell.n if used.
#' @param number.of.samples Number of samples to obtain
#' @param number.of.decimals Number of decimals to report in returned data frame
#' @param alternative indicates type of alternative hypothesis (e.g., "two.sided") for t.test
#' @param seed.value random number seed value
#' @return Data frame with sample d-values
#' @examples
#' get_d_rep_samples(pop.d = .35, cell.n = 100)
#' @export
get_d_rep_samples <- function(pop.d = NULL, n = NULL, mean.n = NULL, sd.n = NULL, number.of.samples = 100,
                              number.of.decimals = 3, alternative = "two.sided", seed.value = NULL) {

  if (!is.null(seed.value)) {
    set.seed(seed.value)
  }

  cell.n = n

  if (is.na(pop.d)) {return()}

  if (!is.null(mean.n) & !is.null(sd.n)) {
    ns.for.cell1 <- abs(round(rnorm(mean = mean.n, sd = sd.n, n = number.of.samples)))
    ns.for.cell1[ns.for.cell1 < 4] <- mean.n # if too small use mean.n
  } else {
    ns.for.cell1 = rep(cell.n, number.of.samples)
  }


  dfs <- rep(NA,number.of.samples)
  ts <- rep(NA,number.of.samples)
  ps <- rep(NA,number.of.samples)
  ds <- rep(NA,number.of.samples)
  d2s <- rep(NA,number.of.samples)
  LLs <- rep(NA,number.of.samples)
  ULs <- rep(NA,number.of.samples)
  in_interval <- rep(NA,number.of.samples)

  for (i in 1:number.of.samples) {
    cell1.n <- ns.for.cell1[i]

    group1.data <- rnorm(cell1.n) + pop.d
    tout <- t.test(group1.data, alternative = alternative)

    dfs[i] <- round(tout$parameter, number.of.decimals)
    ts[i] <- round(tout$statistic, number.of.decimals)
    ps[i] <- round(tout$p.value, number.of.decimals)

    ciinfo <- MBESS::ci.sm(ncp = ts[i], N = cell1.n)
    ds[i] <- round(ciinfo$Standardized.Mean, number.of.decimals)

    in_interval[i] <- is_value_in_interval(pop.d, c(ciinfo$Lower.Conf.Limit.Standardized.Mean, ciinfo$Upper.Conf.Limit.Standardized.Mean))

    LLs[i] <- round(ciinfo$Lower.Conf.Limit.Standardized.Mean, number.of.decimals)
    ULs[i] <- round(ciinfo$Upper.Conf.Limit.Standardized.Mean, number.of.decimals)
  }
  xx<-1:number.of.samples
  sample.number <- xx
  data.out <- data.frame(sample.number, pop.d = pop.d, n = ns.for.cell1, d = ds, LL = LLs, UL = ULs, ci.captured.pop.d = in_interval, t = ts, df = dfs, p = ps)
  rownames(data.out) <- NULL

  return(data.out)
}

