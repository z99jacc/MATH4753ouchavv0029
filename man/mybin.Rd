% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mybin.r
\name{mybin}
\alias{mybin}
\title{Binomial}
\usage{
mybin(iter = 100, n = 10, p = 0.5)
}
\arguments{
\item{iter}{number of iterations of the experiment}

\item{n}{number of trials}

\item{p}{probability of success}
}
\value{
graph of distribution
}
\description{
Builds a graph for a binomial distribution
}
\examples{
n = 10 trials, with probability of success p = 0.6 for each. 100 iterations of the experiment. mybin(100,10,0.6)
}
