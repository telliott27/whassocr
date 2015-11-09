

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
  }
  return(den)
}

wdegree<-function(x,alpha=1) {
  if( class(x) != "matrix" ) x<-as.matrix(x)
  if( length(colnames(x)) > 0 ) cnames<-colnames(x) else cnames<-character(0)
  s<-apply(x,MARGIN=2,FUN=sum)
  k<-apply((x>0)*1,MARGIN=2,FUN=sum)
  d<-(k^(1-alpha)) * (s^alpha)
  return(d)
}

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

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
