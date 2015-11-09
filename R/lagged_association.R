
laggedAssoc<-function(n,t=1,cutoff=0) {
  sumN<-0
  sumD<-0
  totalT<-length(n)
  if ( t >= totalT ) stop("Time lag exceeds total time")
  for( i in c(1:(totalT-t) ) ) {
    a<-n[[i]]$twomode
    b<-n[[(i+t)]]$twomode
    X<-colnames(a)
    for( j in X ) {
      dfj<-a[which(a[,j]==1),!names(dfj)%in%j]
      if( dim(dfj)[1] > 0 ) {
        numj<-apply(dfj,2,sum)
        numj<-(numj>cutoff)*1
      } else {
        numj<-0
      }
      dfk<-b[which(b[,j]==1),!names(dfk)%in%j]
      if ( dim(dfk)[1] > 0 ){
        numk<-apply(dfk,2,sum)
        numk<-(numk>cutoff)*1
        XX<-1
      } else {
        numk<-0
        XX<-0
      }
      XY<-numj*numk
      sumN<-sumN+sum(XY)
      sumD<-sumD + (sum(numj)*XX)
    }
  }
  g<-sumN/sumD
  return(g)
}

nullLagAssoc<-function(n,t=1,cutoff=0) {
  totalT<-length(n)
  if ( t >= totalT ) stop("Time lag exceeds total time")
  sumN<-0
  sumD<-0
  for( i in c(1:(totalT-t)) ) {
    x<-n[[i]]$twomode
    y<-n[[(i+t)]]$twomode
    X<-x %>% bind_rows(y)
    associates<-0
    for( j in names(X) ) {
      df<-X[which(X[,j]==1),!names(X)%in%j]
      num<-apply(df,2,sum)
      num<-(num>cutoff)*1
      sumN<-sumN+sum(num)
    }
    sumD<-sumD+dim(X)[2]
  }

  gr<-sumN/sumD
  nlar<-gr/(dim(X)[2]-1)
  return(nlar)
}
