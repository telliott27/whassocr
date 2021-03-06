% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lagged_association.R
\name{lar}
\alias{lar}
\alias{null.lar}
\title{Lagged Association Rate}
\usage{
lar(x, group = c(1:dim(x)[1]), t = 1, cutoff = 0)

null.lar(x, group = c(1:dim(x)[1]), t = 1, cutoff = 0)
}
\arguments{
\item{x}{a m X n observation matrix}

\item{group}{an integer vector of length m indicating the time grouping of the observations.
Group values should be sequential with no gaps. The vector does not need to be sorted.}

\item{t}{the order of the lag}

\item{cutoff}{the number of times above which two entities must be observed together within
a single time group to count as being associated}
}
\description{
Calculate the lagged association rate for a two-mode observation matrix
}
\details{
The grouping variable \code{group} is used to aggregate observational data to
 daily, weekly, yearly, etc., rates of association. This allows you to calculate a lagged association
 rate between two months or two years. Cases that should be grouped together should be assigned the same group number.

 \code{cutoff} is only useful if you group observations, and allows you to set a minimum number of times
 two entities must be observed together in order to count as being associated. Note that two entities must
 be observed together MORE THAN the value of \code{cutoff}
}
\section{Functions}{
\itemize{
\item \code{null.lar}: Calculate the null lagged association rate
}}

