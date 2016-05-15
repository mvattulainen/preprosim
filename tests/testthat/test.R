library(preprosim)
context("Basic tests")

## TEST CONTAMINATION FUNCTIONS CAN BE EXECUTED

testdataobject <- createdata(iris)
testparam <- new("preprosimparameter", noisecol=1, noiseparam=0.1, noiseorder=1,
                                          lowvarcol=2, lowvarparam=0.1, lowvarorder=2,
                                          misvalcol=1:4, misvalparam=0.1, misvalorder=8,
                                          irfeaturecol=4, irfeatureparam=0.1, irfeatureorder=3,
                                          classswapcol=4, classswapparam=0.1, classswaporder=4,
                                          classimbalancecol=4, classimbalanceparam=0.1, classimbalanceorder=5,
                                          volumedecreasecol=4, volumedecreaseparam=0.1, volumedecreaseorder=6,
                                          outliercol=4, outlierparam=0.1, outlierorder=7)

noisetest <- noisefunction(testdataobject, testparam)
lowvartest <- lowvarfunction(testdataobject, testparam)
misvaltest <- misvalfunction(testdataobject, testparam)
irfeaturetest <- irfeaturefunction(testdataobject, testparam)
classswaptest <- classswapfunction(testdataobject, testparam)
classimbalancetest <- classimbalancefunction(testdataobject, testparam)
volumedecreasetest <- volumedecreasefunction(testdataobject, testparam)
outliertest <- outlierfunction(testdataobject, testparam)
