% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_discrete.R
\name{plot_weighting_discrete}
\alias{plot_weighting_discrete}
\title{Plot weights subdivided by a discrete covariate}
\usage{
plot_weighting_discrete(mod, covariate, alpha = 0.05, ...)
}
\arguments{
\item{mod}{Weighting model object}

\item{covariate}{Covariate vector}

\item{alpha}{Number between zero and one indicating the desired alpha level
for confidence intervals.}

\item{...}{unused arguments}
}
\description{
This provid
}
\examples{
y <- rnorm(100)
a <- rbinom(100, 1, 0.5)
x <- rnorm(100)
g <- sample(1:4, 100, replace = TRUE)
mod <- stats::lm(y ~ a + x)
rw_mod <- calculate_weights(mod, "a")
plot_weighting_discrete(rw_mod, g)
}