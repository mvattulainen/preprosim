# S4: preprosim parameter class

#' An S4 class to represent preprosimparameter
#'
#'@slot noisecol (numeric)
#'@slot noiseparam (numeric)
#'@slot noiseorder (numeric)
#'@slot noisefunction (character)
#'@slot lowvarcol (numeric)
#'@slot lowvarparam (numeric)
#'@slot lowvarorder (numeric)
#'@slot lowvarfunction (character)
#'@slot misvalcol (numeric)
#'@slot misvalparam (numeric)
#'@slot misvalorder (numeric)
#'@slot misvalfunction (character)
#'@slot irfeaturecol (numeric)
#'@slot irfeatureparam (numeric)
#'@slot irfeatureorder (numeric)
#'@slot irfeaturefunction (character)
#'@slot classswapcol (numeric)
#'@slot classswapparam (numeric)
#'@slot classswaporder (numeric)
#'@slot classswapfunction (character)
#'@slot classimbalancecol (numeric)
#'@slot classimbalanceparam (numeric)
#'@slot classimbalanceorder (numeric)
#'@slot classimbalancefunction (character)
#'@slot volumedecreasecol (numeric)
#'@slot volumedecreaseparam (numeric)
#'@slot volumedecreaseorder (numeric)
#'@slot volumedecreasefunction (character)
#'@slot outliercol (numeric)
#'@slot outlierparam (numeric)
#'@slot outlierorder (numeric)
#'@slot outlierfunction (character)
setClass("preprosimparameter", representation(noisecol="numeric", noiseparam="numeric", noiseorder="numeric", noisefunction="character",
                                              lowvarcol="numeric", lowvarparam="numeric", lowvarorder="numeric", lowvarfunction="character",
                                              misvalcol="numeric", misvalparam="numeric", misvalorder="numeric", misvalfunction="character",
                                              irfeaturecol="numeric", irfeatureparam="numeric", irfeatureorder="numeric", irfeaturefunction="character",
                                              classswapcol="numeric", classswapparam="numeric", classswaporder="numeric", classswapfunction="character",
                                              classimbalancecol="numeric", classimbalanceparam="numeric", classimbalanceorder="numeric", classimbalancefunction="character",
                                              volumedecreasecol="numeric", volumedecreaseparam="numeric", volumedecreaseorder="numeric", volumedecreasefunction="character",
                                              outliercol="numeric", outlierparam="numeric", outlierorder="numeric", outlierfunction="character"),
         prototype(noisefunction="noisefunction(data, param)", lowvarfunction="lowvarfunction(data, param)",
                   misvalfunction="misvalfunction(data, param)", irfeaturefunction="irfeaturefunction(data, param)",
                   classswapfunction = "classswapfunction(data, param)", classimbalancefunction="classimbalancefunction(data, param)",
                   volumedecreasefunction="volumedecreasefunction(data, param)", outlierfunction="outlierfunction(data, param)")
)

#' Create new preprosimparameter object
#' @return preprosimparameter class object
#' @details all columns, parameters and order are set to zero
#' @examples
#' ## res <- newparam()
#' @export
newparam <- function(){
  object <- new("preprosimparameter", noisecol=0, noiseparam=0, noiseorder=0,
                  lowvarcol=0, lowvarparam=0, lowvarorder=0,
                  misvalcol=0, misvalparam=0, misvalorder=0,
                  irfeaturecol=0, irfeatureparam=0, irfeatureorder=0,
                  classswapcol=0, classswapparam=0, classswaporder=0,
                  classimbalancecol=0, classimbalanceparam=0, classimbalanceorder=0,
                  volumedecreasecol=0, volumedecreaseparam=0, volumedecreaseorder=0,
                  outliercol=0, outlierparam=0, outlierorder=0)
}

#' Change preprosimparametes
#' @param object (preprosimparameter object)
#' @param contamination (character) one of the following: noise, lowvar, misvalue, irfeature, classswap, classimbalance, volumedecrease, outlier
#' @param param (character) one of the following: cols, param, order
#' @param value (numeric) vector of parameter values
#' @examples
#' ## res <- newparam()
#' ## res <- changeparam(res, "noise", "cols", value=1)
#' ## res <- changeparam(res, "noise", "param", value=c(0,0.1))
#' ## res <- changeparam(res, "noise", "order", value=1)
#' @export

changeparam <- function(object, contamination, param, value) {

  if (contamination=="noise")
  {
    if (param=="cols") {object@noisecol <- value}
    if (param=="param") {object@noiseparam <- value}
    if (param=="order") {object@noiseorder <- value}
  }

  if (contamination=="lowvar")
  {
    if (param=="cols") {object@lowvarcol <- value}
    if (param=="param") {object@lowvarparam <- value}
    if (param=="order") {object@lowvarorder <- value}
  }

  if (contamination=="misval")
  {
    if (param=="cols") {object@misvalcol <- value}
    if (param=="param") {object@misvalparam <- value}
    if (param=="order") {object@misvalorder <- value} # VALIDATION: This must always be last
  }

  if (contamination=="irfeature")
  {
    if (param=="cols") {object@irfeaturecol <- value}
    if (param=="param") {object@irfeatureparam <- value}
    if (param=="order") {object@irfeatureorder <- value}
  }

  if (contamination=="classswap")
  {
    if (param=="cols") {object@classswapcol <- value}
    if (param=="param") {object@classswapparam <- value}
    if (param=="order") {object@classswaporder <- value}
  }

  if (contamination=="classimbalance")
  {
    if (param=="cols") {object@classimbalancecol <- value}
    if (param=="param") {object@classimbalanceparam <- value}
    if (param=="order") {object@classimbalanceorder <- value}
  }

  if (contamination=="volumedecrease")
  {
    if (param=="cols") {object@volumedecreasecol <- value}
    if (param=="param") {object@volumedecreaseparam <- value}
    if (param=="order") {object@volumedecreaseorder <- value}
  }

  if (contamination=="outlier")
  {
    if (param=="cols") {object@outliercol <- value}
    if (param=="param") {object@outlierparam <- value}
    if (param=="order") {object@outlierorder <- value}
  }

  object

}



#' An S4 class to represent preprosim data
#'
#'@slot x (data frame) data frame consisting of numeric features
#'@slot y (factor) vector of class labels

setClass("preprosimdata", representation(x="data.frame", y="factor"))

createdata <- function(data) {

dataclassobject <- new("preprosimdata")
dataclassobject@x <- data[sapply(data, is.numeric)]
dataclassobject@y <- factor(data[sapply(data, is.factor)][,1])
return(dataclassobject)
}

#' An S4 class to represent preprosim analysis output
#'
#'@slot grid (data frame) data frame consisting of combinations of preprosimparameters
#'@slot data (list) list of simulated data sets
#'@slot output (numeric) vector of classification accuracies
#'@slot variableimportance (data frame) data frame consisting of variable importance values
#'@slot outliers (numeric) vector of outlier scores
setClass("preprosimanalysis", representation(grid="data.frame", data="list", output="numeric", variableimportance="data.frame", outliers="numeric"))


