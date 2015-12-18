library(whassocr)
library(testthat)
context("Network Functions")

#set up test data frame
set.seed(12987)
test.df<-matrix(rbinom(600,1,0.3),nrow=100,ncol=6)
test.df<-as.data.frame(test.df)
names(test.df)<-c("Cat","Dog","Pig","Hawk","Snake","Mouse")
assoc<-makeAssociation(test.df)
load("../testdata/wdeg.Rda")

test_that("Weighted density calculated correctly",{
  expect_equal(round(net.density(assoc,type="weighted"),7),0.1746183)
})


test_that("Two-Mode density calculated correctly",{
  expect_equal(round(net.density(test.df,type="twomode"),7),0.3066667)
})


test_that("Weighted degree calculated correctly",{
  expect_equal(wdegree(assoc),wdeg1)
  expect_equal(wdegree(assoc,alpha=0.2),wdeg2)
  expect_equal(wdegree(assoc,alpha=0.8),wdeg3)
  expect_equal(wdegree(assoc,alpha=1.2),wdeg4)
  expect_equal(wdegree(assoc,alpha=0),wdeg5)
})

test_that("weighted clustering coefficient for arithmetic mean",{
  assoc[3,]<-0
  assoc[,3]<-0
  expect_equal(wclust(assoc,"athr"),0.5)
})


test_that("weighted clustering coefficient for gemometric mean",{
  assoc[3,]<-0
  assoc[,3]<-0
  expect_equal(round(wclust(assoc,"geo"),7),0.4921257)
})

test_that("weighted clustering coefficient for max",{
  assoc[3,]<-0
  assoc[,3]<-0
  expect_equal(round(wclust(assoc,"max"),7),0.5664929)
})

test_that("weighted clustering coefficient for min",{
  assoc[3,]<-0
  assoc[,3]<-0
  expect_equal(round(wclust(assoc,"min"),7),0.4323184)
})


