#' Lagged Association Rate
#'
#' Calculate the lagged association rate for a two-mode observation matrix
#'
#' @param x a m X n observation matrix
#' @param group an integer vector of length m indicating the time grouping of the observations.
#'  Group values should be sequential with no gaps. The vector does not need to be sorted.
#' @param t the order of the lag
#' @param cutoff the number of times above which two entities must be observed together within
#'  a single time group to count as being associated
#'
#' @details The grouping variable \code{group} is used to aggregate observational data to
#'  daily, weekly, yearly, etc., rates of association. This allows you to calculate a lagged association
#'  rate between two months or two years. Cases that should be grouped together should be assigned the same group number.
#'
#'  \code{cutoff} is only useful if you group observations, and allows you to set a minimum number of times
#'  two entities must be observed together in order to count as being associated. Note that two entities must
#'  be observed together MORE THAN the value of \code{cutoff}
#'
#'  @return The lagged association rate for the observation matrix for a lag of \code{t}
#'  @export

#need to make this work when there is no grouping
lar<-function(x,group=c(1:dim(x)[1]),t=1,cutoff=0) {
  #make groups generic
  times<-group-min(group)+1
  #check if observations are grouped
  ngroups<-length(unique(times))
  nogroups<-ngroups==dim(x)[1]
  x<-as.matrix(x)
  sumN<-0
  sumD<-0
  if( t>= max(times) ) stop("Time lag exceeds total time")
  if( nogroups & cutoff > 0 ) stop("cutoff cannot be greater than 0 if not grouping observations")
  for( i in c(1:(length(times)-t)) ) {
    a<-as.matrix(x[which(times==i), ,drop=F])
    b<-as.matrix(x[which(times==(i+t)), ,drop=F])
    #have to reverse the order of matrix multiplication if extracting
    #only one row versus more than one row
    a<-t(a)%*%a
    b<-t(b)%*%b
    a<-(a>cutoff)*1
    b<-(b>cutoff)*1
    c<-a*b
    #the sum of the number of pair observations (minus the diagonal)
    sumN<-sumN+sum(c)-sum(diag(c))
    #the sum of a given b minus the diagonal
    #essential the number of times a single entity is observed in both time periods
    sumD<-sumD+sum(a%*%diag(b))-sum(diag(c))
  }
  g<-sumN/sumD
  return(g)
}

##NULL association rate is not being calculated correctly

#' @describeIn lar Calculate the null lagged association rate
#' @export
null.lar<-function(x,group=c(1:100),t=1,cutoff=0) {
  times<-group-min(group)+1
  #check if observations are grouped
  ngroups<-length(unique(times))
  nogroups<-ngroups==dim(x)[1]
  sumN<-0
  sumD<-0
  if( t>= max(times) ) stop("Time lag exceeds total time")
  if( nogroups & cutoff > 0 ) stop("cutoff cannot be greater than 0 if not grouping observations")
  for( i in c(1:(length(times)-t)) ) {
    a<-as.matrix(x[which(times==i), ,drop=F])
    b<-as.matrix(x[which(times==(i+t)), ,drop=F])
    #have to reverse the order of matrix multiplication if extracting
    #only one row versus more than one row
    a<-t(a)%*%a
    b<-t(b)%*%b
    a<-(a>cutoff)*1
    b<-(b>cutoff)*1
    diag(a)<-0
    diag(b)<-0
    da<-sum(apply(a,2,sum))
    db<-sum(apply(b,2,sum))

    sumN<-sumN + da + db

    sumD<-sumD+2*dim(a)[1]
  }

  gr<-sumN/sumD
  nlar<-gr/(dim(x)[2]-1)
  return(nlar)
}



