library(whassocr)
library(testthat)
context("Random Graphs and Significance")

#set up test data frame
set.seed(12987)
test.df<-matrix(rbinom(300,1,0.3),nrow=50,ncol=6)
test.df<-as.data.frame(test.df)
names(test.df)<-c("Cat","Dog","Pig","Hawk","Snake","Mouse")

test_that("randomGraphs returns list",{
  expect_is(randomGraphs(test.df,n=50),"list")
  expect_equal(length(randomGraphs(test.df,n=10)),3)
})

test_that("randomGraphs calculates expected results correctly",{
  set.seed(45783)
  rgraphs<-randomGraphs(test.df,n=50)
  expect_equal(round(rgraphs$expected[1,2],7),0.1007553)
  expect_equal(round(rgraphs$expected[3,5],8),0.09806268)
  expect_equal(rgraphs$expected[6,6],0)
})

set.seed(714299)
rgraphs<-randomGraphs(test.df,n=50)
sig<-getSig(makeAssociation(test.df),rgraphs)

test_that("getSig returns a list",{
  expect_is(sig,"list")
  expect_equal(length(sig),5)
})

test_that("getSig returns consistenct results",{
  expect_equal(round(sig$pl[1,2],7),0.6)
  expect_equal(round(sig$pl[3,5],7),0.44)
  expect_equal(round(sig$S,9),0.007392317)
  expect_equal(round(sig$Sp,7),0.76)
})
