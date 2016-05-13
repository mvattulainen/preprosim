library(preprosim)
context("Basic tests")

testdataobject <- createdata(iris)
expect_is(testdataobject, "preprosimdata")
