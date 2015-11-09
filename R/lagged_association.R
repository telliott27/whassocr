#' Lagged Association Rate
#'
#' Calculate the lagged association rate for a two-mode observation matrix
#'
#' @param x a m X n observation matrix
#' @param group an integer vector of length m indicating the time grouping of the observations.
#'  Group values should be sequential with no gaps.
#' @param t the order of the lag
#' @param cutoff the number of times above which two entities must be observed together within
#'  a single time group to count as being associated
#'
#' @details The grouping variable \code{group} is used to aggregate observational data to
#'  daily, weekly, yearly, etc., rates of association. This allows you to calculate a lagged association
#'  rate between two months or two years.
#'
#'  \code{cutoff} is only useful if you group observations, and allows you to set a minimum number of times
#'  two entities must be observed together in order to count as being associated. Note that two entities must
#'  be observed together MORE THAN the value of \code{cutoff}
#'
#'  @return The lagged association rate for the observation matrix for a lag of \code{t}

#need to make this work when there is no grouping
lar<-function(x,group,t=1,cutoff=0) {
  times<-group-min(group)+1
  x<-as.matrix(x)
  sumN<-0
  sumD<-0
  if( t>= max(times) ) stop("Time lag exceeds total time")
  for( i in c(1:(length(times)-t)) ) {
    a<-x[which(times==i),]
    a<-t(a)%*%a
    a<-(a>cutoff)*1
    b<-as.matrix(x[which(times==(i+t)),])
    b<-t(b)%*%b
    b<-(b>cutoff)*1
    for( j in c(1:dim(a)[1]) ) {
      for( k in c(1:dim(a)[1] ) ) {
        if( j != k ) {
          ajXY<-a[j,k]
          akXY<-b[j,k]
          akXX<-b[j,j]
          sumN<-sumN + (ajXY*akXY)
          sumD<-sumD + (ajXY*akXX)
        }

      }
    }
  }
  g<-sumN/sumD
  return(g)
}

#' Null Lagged Association Rate
#'
#' Calculate the null lagged association rate for a two-mode observation matrix. This is the
#' expected lagged association rate if entities associated randomly.
#'
#' @param x a m X n observation matrix
#' @param group an integer vector of length m indicating the time grouping of the observations.
#'  Group values should be sequential with no gaps.
#' @param t the order of the lag
#' @param cutoff the number of times above which two entities must be observed together within
#'  a single time group to count as being associated
#'
#' @details The grouping variable \code{group} is used to aggregate observational data to
#'  daily, weekly, yearly, etc., rates of association. This allows you to calculate a lagged association
#'  rate between two months or two years.
#'
#'  \code{cutoff} is only useful if you group observations, and allows you to set a minimum number of times
#'  two entities must be observed together in order to count as being associated. Note that two entities must
#'  be observed together MORE THAN the value of \code{cutoff}
#'
#'  @return The null lagged association rate for the observation matrix for a lag of \code{t}

nulllar<-function(x,group,t=1,cutoff=0) {
  times<-group-min(group)+1
  sumN<-0
  sumD<-0
  if( t>= max(times) ) stop("Time lag exceeds total time")
  for( i in c(1:(length(times)-t)) ) {
    a<-x[which(times==i),]
    a<-t(a)%*%a
    a<-(a>cutoff)*1
    b<-as.matrix(x[which(times==(i+t)),])
    b<-t(b)%*%b
    b<-(b>cutoff)*1
    da<-sum(apply(a,2,sum))-1
    db<-sum(apply(b,2,sum))-1

    sumN<-sumN + da + db

    sumD<-sumD+dim(a)[1]
    sumD<-sumD+dim(a)[1]
  }

  gr<-sumN/sumD
  nlar<-gr/(dim(x)[2]-1)
  return(nlar)
}



