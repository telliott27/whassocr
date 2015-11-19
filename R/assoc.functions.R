



pmin2 <- function(k,x) (x+k - abs(x-k))/2

pmax2 <- function(k,x) (x+k + abs(x-k))/2

#' Generate Association Matrix from Observed Association Data
#'
#' \code{makeAssociation} takes as its input a m x n two-mode matrix
#' of observed association data and generates a n x n matrix of
#' association rates between n entities.
#'
#' @param x a m x n data frame or matrix that contains the entities observed
#' in the columns, the observations in rows, and each cell=c(0,1) indicating whether
#' the entity was observed in the observation
#' @return A n x n matrix containing the association rates between each n entity in x
#' @export

makeAssociation<-function(x) {
  #get column names
  a<-colnames(x)
  #get number of rows
  n<-dim(x)[1]
  #get number of columns
  cols<-dim(x)[2]
  #if there were no column names, then create unlabeled matrix
  #else create a labeled matrix
  if ( length(a) == 0 ) {
    m<-matrix(nrow=cols,ncol=cols)
  } else {
    m<-matrix(nrow=length(a),ncol=length(a))
    rownames(m)<-colnames(m)<-a
  }

  xy<-function(i,j,x) {
    if( i == j ) {
      t<-0
    } else {
      n<-dim(x)[1]
      c1<-x[,i]
      c2<-x[,j]
      D<-sum(1-pmax2(c1,c2))
      z<-sum(pmin2(c1,c2))
      t<-z/(n-D)
      if ( is.nan(t) ) t<-0
    }
    return(t)
  }
  copy.tri <- function(m) {
    m[lower.tri(m)] <- t(m)[lower.tri(m)]
    m
  }

  for( i in c(1:cols) ) {
    for( j in c(i:cols) ) {
      m[i,j]<-xy(i,j,x)
    }
  }
  m<-copy.tri(m)
  return(m)
}





