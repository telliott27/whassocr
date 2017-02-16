library(whassocr)
library(testthat)
context("Make Association Matrix")

#set up test data frame
set.seed(12987)
test.df<-matrix(rbinom(300,1,0.3),nrow=50,ncol=6)
test.df<-as.data.frame(test.df)
names(test.df)<-c("Cat","Dog","Pig","Hawk","Snake","Mouse")

test_that("Make Association returns matrix",{
  expect_is(makeAssociation(test.df),"matrix")
})

test_that("makeAssociation assigns names correctly",{
  expect_equal(row.names(makeAssociation(test.df)),names(test.df))
})

test_that("makeAssociation calculates association rates correctly",{
  #have to round to seven digits
  expect_equal(round(makeAssociation(test.df)[1,2],7),0.1785714)
  expect_equal(round(makeAssociation(test.df)[3,5],7),0.1851852)
  expect_equal(makeAssociation(test.df)[6,6],0)
})


test_that("makeNonSymAssociation calculates association rates correctly",{
  #have to round to seven digits
  expect_equal(round(makeNonSymAssociation(test.df)[1,2],7),0.3333333)
  expect_equal(round(makeAssociation(test.df)[3,5],7),0.3571429)
  expect_equal(makeAssociation(test.df)[6,6],0)
})
