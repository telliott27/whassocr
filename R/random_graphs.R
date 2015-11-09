
#' Create random data based off of observed data
#'
#' Implements a method described by Manly (1995) to generate
#' random datasets based on observed data. Random datasets preserve
#' the marginal distributions (both row and column) of the original data.
#'
#' @param x a m x n matrix of observatinos by entities.
#' @param n the number of random graphs to generate. Default is 1000, however
#'  the proper number of random graphs to generate will depend on the size and
#'  sparseness of \code{x}
#'
#' @return A list containing three objects:
#'  \item{g}{A list of length \code{n} containing all the random m x n matrices}
#'  \item{a}{A list of length \code{n} containing all the association
#'    matrices generated from g above}
#'  \item{expected}{A n x n matrix containing the average association index from the graphs in \code{a}}


randomGraphs<-function(x,n=1000) {
  assign("num.calls",0,envir=.GlobalEnv)

  pb<-txtProgressBar(min=0,max=n,style=3)
  graphs<-list()
  assoc<-list()
  g<-x
  j<-floor(n/2)
  for( i in c(1:j) ) {
    graphs[[i]]<-g
    assoc[[i]]<-makeAssociation(g)
    g<-createRandomGraph(g)
    setTxtProgressBar(pb,i)
  }
  g<-x
  for( i in c(1:j) ) {
    graphs[[i]]<-g
    assoc[[i]]<-makeAssociation(g)
    g<-createRandomGraph(g)
    setTxtProgressBar(pb,i+j)
  }
  expected<-Reduce("+",assoc)/n
  r<-list(g=graphs,a=assoc,expected=expected)
  close(pb)
  return(r)
}


#' Find indices of the matrix that match the pattern for producing random graph
#'
#' Searches for and randomly selects indices in an observation matrix that matches
#' \code{matrix(c(0,1,1,0),nrow=2)} or \code{matrix(c(1,0,0,1),nrow=2)}. Used internally
#' by \code{\link{createRandomGraph}} for generating random graphs.
#'
#' @param x a m x n matrix or data frame of observational data
#'
#' @return A list containing the rows and columns associated with the matching pattern


sampleMatrix<-function(x) {

  rn<-rbinom(1,1,0.5)
  m1<-which(x==1,arr.ind=TRUE)
  m0<-which(x==0,arr.ind=TRUE)
  ri<-intersect(m0[,1],m1[,1])
  ci<-intersect(m0[,2],m1[,2])
  m0<-m0[which(m0[,1]%in%ri&m0[,2]%in%ci),]
  m1<-m1[which(m1[,1]%in%ri&m1[,2]%in%ci),]
  problems<-FALSE

  if( rn == 1 ) {
    rand1<-sample(1:dim(m0)[1],1)
    row<-m0[rand1,1]
    col<-m0[rand1,2]
    pos<-merge(m1[which(m1[,2]==col),1],m1[which(m1[,1]==row),2])
    names(pos)<-c("row","col")
    inter<-intersect(pos,m0)
    if( !dim(inter)[1]>0 ) problems<-TRUE
    rand2<-sample(1:dim(inter)[1],1)
    row2<-inter[rand2,1]
    col2<-inter[rand2,2]
  } else {
    rand1<-sample(1:dim(m1)[1],1)
    row<-m1[rand1,1]
    col<-m1[rand1,2]
    pos<-merge(m0[which(m0[,2]==col),1],m0[which(m0[,1]==row),2])
    names(pos)<-c("row","col")
    inter<-intersect(pos,m1)
    if( !dim(inter)[1]>0 ) problems<-TRUE
    rand2<-sample(1:dim(inter)[1],1)
    row2<-inter[rand2,1]
    col2<-inter[rand2,2]
  }
  rows<-c(row,row2)
  cols<-c(col,col2)
  result<-list(rows=rows,cols=cols)
  if( problems ) result<-sampleMatrix(x)
  return(result)
}

#' Create a single random graph
#'
#' Used internally by \code{\link{randomGraphs}} to generate a single random graph.
#'
#' @param x a m x n matrix or data frame of observational data
#'
#' @return a m x n matrix that is a random permutation of \code{x}

createRandomGraph<-function(x) {
  #set found to false
  found<-FALSE
  #create two patterns we are swapping out
  a<-matrix(c(1,0,0,1),nrow=2)
  b<-matrix(c(0,1,1,0),nrow=2)
  while( !found ) {
    #grab four elements
    t<-sampleMatrix(x)
    if ( all(x[t$rows,t$cols]==a) ) {
      x[t$rows,t$cols]<-b
      found<-TRUE
    } else if ( all(x[t$rows,t$cols]==b) ) {
      x[t$rows,t$cols]<-a
      found<-TRUE
    }
  }
  return(x)
}
