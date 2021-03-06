% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_graphs.R
\name{randomGraphs}
\alias{randomGraphs}
\title{Create random data based off of observed data}
\usage{
randomGraphs(x, n = 1000, symmetric = TRUE)
}
\arguments{
\item{x}{a m x n matrix of observatinos by entities.}

\item{n}{the number of random graphs to generate. Default is 1000, however
the proper number of random graphs to generate will depend on the size and
sparseness of \code{x}
@param symmetric should the association rates calculated by symmetric?}
}
\value{
A list containing three objects:
 \item{g}{A list of length \code{n} containing all the random m x n matrices}
 \item{a}{A list of length \code{n} containing all the association
   matrices generated from g above}
 \item{expected}{A n x n matrix containing the average association index from the graphs in \code{a}}
}
\description{
Implements a method described by Manly (1995) to generate
random datasets based on observed data. Random datasets preserve
the marginal distributions (both row and column) of the original data.
}

