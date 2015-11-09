#' A generalized function to calculate network densities
#'
#' This function calculates network densities of either two-mode observational matrices
#' or association matrices.
#'
#' @param x either a two-mode observational matrix or an association matrix
#' @param type One of either "weighted" (default) or "twomode" indicating what kind of
#'    matrix x is
#'
#' @return A singular value equal to the density of the matrix.

net.density<-function(x,type="weighted") {
  if( class(x) != "matrix" ) x<-as.matrix(x)
  if( type == "weighted" ) {
    x[diag(x)]<-0
    s<-sum(x)
    n<-dim(x)[1]
    den<-s/(n*(n-1))
  } else if (type == "twomode") {
    s<-sum(x)
    n<-dim(x)[1]
    m<-dim(x)[2]
    den<-s/(n*m)
  } else {
    stop("Type incorrectly specified")
  }
  return(den)
}

#' Calculate the weighted degree of an association matrix
#'
#' This uses the method described by Opsahl, Agneessens, and Svoretz (2010) to calculate a weighted
#' degree centrality for each node in an association matrix.
#'
#' @param x a n X n association matrix
#' @param alpha The value of the tuning parameter. The value should be a number greater
#' than or equal to 0. See below for details.
#'
#' @details The tuning parameter, \code{alpha}, adjudicates the importance of the number of ties
#'  a node has versus the strength of those ties in calculating the weighted degree. If \code{alpha} is
#'  equal to zero, then the resulting degree scores are identical to a calculation of undirected
#'  degree scores on an unvalued matrix in which any value > 0 counts as a tie. If \code{alpha} is
#'  equal to 1, then the degree scores are equal to the sum of the values of the ties connected
#'  to the node. If \code{alpha} is greater than 1, then the number of ties a node has is weighted
#'  inversely to the degree score, so that the more ties a node has, the lower its degree score will be.
#'
#' @return A named vector of degree scores calculated for each node in the association matrix.
#'
#' @references
#' Opsahl, T., F. Agneessens, and J. Svoretz. "Node centrality in weighted networks:
#' Generalizing degree and shortest paths." Social Networks, 32:245-251.

wdegree<-function(x,alpha=1) {
  if( class(x) != "matrix" ) x<-as.matrix(x)
  if( length(colnames(x)) > 0 ) cnames<-colnames(x) else cnames<-character(0)
  s<-apply(x,MARGIN=2,FUN=sum)
  k<-apply((x>0)*1,MARGIN=2,FUN=sum)
  d<-(k^(1-alpha)) * (s^alpha)
  return(d)
}

#' a function to calculate the geometric mean
#'
#' Used internally by \code{\link{wclus}}
#'
#' @param x a vector or other object that can be coerced into a vector
#' @param na.rm logical. Should missing values be removed?
#'
#' @return the geometric mean.

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#' Weighted clustering
#'
#' A function to calculate the weighted clustering of an association matrix
#'
#' @param x a n X n association matrix
#' @param mode one of "athr" (arithmetic mean), "geo" (geometric mean), "min", or "max" indicating which statistic to use for triplet values
#'
#' @details This function calculates a clustering coefficient for a weighted graph by calculating a value
#' for every triplet of nodes based on the statistic chosen in \code{mode}. The clustering coefficient
#' is equal to the sum of triplet values for closed triads divided by the sum of all triplet values.
#'
#' @references
#' Ospsahl, T. and P. Panzarasa. 2009. "Clustering in weighted network." Social Networks, 31:155-163.

wclust<-function(x,mode=c("athr","geo","min","max")) {
  wm<-match.arg(mode)
  if ( wm=="athr" ) clcal<-function(x) return(mean(x))
  else if (wm == "geo") clcal<-function(x) return(gm_mean(x))
  else if (wm=="min") clcal<-function(x) return(min(x))
  else if (wm=="max") clcal<-function(x) return(max(x))
  triads<-combn(dim(x)[1],3)
  numer<-0
  denom<-0
  for( i in c(1:dim(triads)[2])) { #should try to convert this to apply function
    one<-x[triads[1,i],triads[2,i]]
    two<-x[triads[1,i],triads[3,i]]
    three<-x[triads[2,i],triads[3,i]]
    edges<-vector()
    if( one!= 0 ) edges<-c(edges,one)
    if( two!= 0 ) edges<-c(edges,two)
    if ( three!=0 ) edges<-c(edges,three)
    if(length(edges)>0) w<-clcal(edges) else w<-0
    denom<-denom+w
    if(length(edges)==3) numer<-numer+w
  }
  return(numer/denom)
}
