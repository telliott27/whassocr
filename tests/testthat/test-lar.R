library(whassocr)
library(testthat)
context("Lagged Association Rates")

#set up test data frame
set.seed(12987)
test.df<-matrix(rbinom(600,1,0.3),nrow=100,ncol=6)
test.df<-as.data.frame(test.df)
names(test.df)<-c("Cat","Dog","Pig","Hawk","Snake","Mouse")
group<-rep(1:10,length.out=100)
group<-sort(group)

test_that("Aggregated LAR calculated correctly",{
  expect_equal(round(lar(test.df,group),7),0.5960265)
})

test_that("Aggregated LAR with longer lag calculated correctly",{
  expect_equal(round(lar(test.df,group,t=2),7),0.5238095)
  expect_equal(lar(test.df,group,t=5),0.65)
})

test_that("Aggregated LAR with cutoff calculated correctly",{
  expect_equal(round(lar(test.df,group,cutoff=1),7),0.1724138)
  expect_equal(round(lar(test.df,group,cutoff=2),7),0.2)
})

test_that("Ungrouped LAR calculated correctly",{
  expect_equal(round(lar(test.df),7),0.2727273)
})

test_that("Ungrouped LAR with longer lag calculated correctly",{
  expect_equal(round(lar(test.df,t=2),7),0.3037975)
  expect_equal(round(lar(test.df,t=5),7),0.4269663)
})

test_that("Ungrouped LAR with cutoff>0 stops",{
  expect_error(lar(test.df,cutoff=2))
})
