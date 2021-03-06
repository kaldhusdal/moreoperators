% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval_operators.R
\name{Interval operators}
\alias{Interval operators}
\alias{\%[]\%}
\alias{\%()\%}
\alias{\%(]\%}
\alias{\%[)\%}
\title{Interval operators}
\usage{
x \%[]\% y

x \%()\% y

x \%(]\% y

x \%[)\% y
}
\arguments{
\item{x}{An atomic vector.}

\item{y}{An atomic vector of length 2 containing the limits of a continous set.}
}
\value{
The operators return an atomic vector of class \code{logical} with the same length as \code{x}.
}
\description{
Operators to check if values are inside continuous (half) open and closed sets.
}
\examples{

4 \%[]\% c(4, 6)
3 \%[]\% c(4, 6)
4 \%(]\% c(4, 6)
6 \%(]\% c(4, 6)
4 \%[)\% c(4, 6)
6 \%[)\% c(4, 6)
6 \%()\% c(4, 6)
5 \%()\% c(4, 6)
Sys.time() \%()\% (Sys.time() + c(0, 1) * 60)
Sys.Date() \%[]\% c("2020-01-01", "2020-12-31")
set.seed(29)
2 \%()\% range(rnorm(100))

}
