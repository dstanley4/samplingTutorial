% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling_d_rep.R
\name{get_d_rep_samples}
\alias{get_d_rep_samples}
\title{Calculate a number of sample d-values (unbiased) based on a specified (infinite) population correlation.}
\usage{
get_d_rep_samples(
  pop.d = NULL,
  n = NULL,
  mean.n = NULL,
  sd.n = NULL,
  number.of.samples = 100,
  number.of.decimals = 3,
  alternative = "two.sided",
  seed.value = NULL
)
}
\arguments{
\item{pop.d}{Population d-value}

\item{mean.n}{Use to specify mean of a distribution of sample sizes. Ignores cell.n if used.}

\item{sd.n}{Use to specify sd of a distribution of sample sizes. Ignores cell.n if used.}

\item{number.of.samples}{Number of samples to obtain}

\item{number.of.decimals}{Number of decimals to report in returned data frame}

\item{alternative}{indicates type of alternative hypothesis (e.g., "two.sided") for t.test}

\item{seed.value}{random number seed value}

\item{cell.n}{Cell size for both cells for all samples. If you use two values (e.g., c(20, 40), these represent -3/+3 SD for variable sample sizes}
}
\value{
Data frame with sample d-values
}
\description{
Calculate a number of sample d-values (unbiased) based on a specified (infinite) population correlation.
}
\examples{
get_d_rep_samples(pop.d = .35, cell.n = 100)
}
