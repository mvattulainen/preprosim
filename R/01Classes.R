# S4: preprosim parameter class

#' An S4 class representing simulation control parameters
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

#' Create new simulation control parameter object
#'
#' Preprosim parameter objects contain eight contaminations: noise, lowvar, misval, irfeature, classswap, classimbalance, volumedecrease and outlier.
#' Each contamination has three sub parameters: cols as columns the contamination is applied to, param as the
#' parameter of the contamination itself (i.e. intensity of contamination) and order as order in which the parameter
#' is applied to the data.
#' @param dataframe (data frame) original data to be used in simulations
#' @param type (character) creation type: empty, default or custom, defaults to "default"
#' @param x (character) primary contamination of interest such as "misval"
#' @param z (character) secondary contamination of interest such as "noise"
#' @return preprosimparameter class object
#' @details For argument type: empty creates a preprosimparameter object with empty params (but not empty cols or order).
#' default creates 6561 combinations with all params 0, 0.1, 0.2. custom creates params seq(0, 0.9, by 0.1) for primary (x)
#' and 0., 0.1, 0.2 for secondary (z). The implicit y (not an argument) refers to classification accuracy.
#' @examples
#' pa <- newparam(iris)
#' pa1 <- newparam(iris, "empty")
#' pa2 <- newparam(iris, "custom", "misval", "noise")
#' @export
newparam <- function(dataframe, type="default", x, z){

  test <- createdata(dataframe)
  allcols <- 1:ncol(test@x)
  randcols <- sample(allcols, 2)

  if (type=="empty") {

  object <- new("preprosimparameter", noisecol=allcols, noiseparam=0, noiseorder=1,
                  lowvarcol=randcols[1], lowvarparam=0, lowvarorder=2,
                  misvalcol=allcols, misvalparam=0, misvalorder=8,
                  irfeaturecol=0, irfeatureparam=0, irfeatureorder=4,
                  classswapcol=0, classswapparam=0, classswaporder=5,
                  classimbalancecol=0, classimbalanceparam=0, classimbalanceorder=6,
                  volumedecreasecol=0, volumedecreaseparam=0, volumedecreaseorder=7,
                  outliercol=randcols[2], outlierparam=0, outlierorder=3)

  }

  if (type=="default") {

  object <- new("preprosimparameter", noisecol=allcols, noiseparam=c(0, 0.1, 0.2), noiseorder=1,
                  lowvarcol=randcols[1], lowvarparam=c(0, 0.1, 0.2), lowvarorder=2,
                  misvalcol=allcols, misvalparam=c(0, 0.1, 0.2), misvalorder=8,
                  irfeaturecol=0, irfeatureparam=c(0, 0.1, 0.2), irfeatureorder=4,
                  classswapcol=0, classswapparam=c(0, 0.1, 0.2), classswaporder=5,
                  classimbalancecol=0, classimbalanceparam=c(0, 0.1, 0.2), classimbalanceorder=6,
                  volumedecreasecol=0, volumedecreaseparam=c(0, 0.1, 0.2), volumedecreaseorder=7,
                  outliercol=randcols[2], outlierparam=c(0, 0.1, 0.2), outlierorder=3)

  }

  if (type=="custom") {

    object <- new("preprosimparameter", noisecol=allcols, noiseparam=0, noiseorder=1,
                  lowvarcol=randcols[1], lowvarparam=0, lowvarorder=2,
                  misvalcol=allcols, misvalparam=0, misvalorder=8,
                  irfeaturecol=0, irfeatureparam=0, irfeatureorder=4,
                  classswapcol=0, classswapparam=0, classswaporder=5,
                  classimbalancecol=0, classimbalanceparam=0, classimbalanceorder=6,
                  volumedecreasecol=0, volumedecreaseparam=0, volumedecreaseorder=7,
                  outliercol=randcols[2], outlierparam=0, outlierorder=3)

    object <- changeparam(object, x, "param", value=seq(0, 0.9, by=0.1))
    object <- changeparam(object, z, "param", value=c(0, 0.1, 0.2))
  }

  validatepreprosimparameters(object)

  object
}

#' Change simulation control parameter object
#'
#' Preprosim parameter objects contain eight contaminations: noise, lowvar, misval, irfeature, classswap, classimbalance, volumedecrease and outlier.
#' Each contamination has three sub parameters: cols as columns the contamination is applied to, param as the
#' parameter of the contaminations itself (i.e. intensity of contamination) and order as order in which the parameter
#' is applied to the data.
#' @param object (preprosimparameter object)
#' @param contamination (character) one of the following: noise, lowvar, misval, irfeature, classswap, classimbalance, volumedecrease, outlier
#' @param param (character) one of the following: cols, param, order
#' @param value (numeric) scalar (for order) or vector (for cols and param) of parameter values
#' @return preprosimparameter class object
#' @details The order of contaminations (cols parameter) must be between 1 and 8, and no two contaminations can have the same order.
#' The contamination parameter (param parameter) must start with 0 (e.g. param="param", value=c(0,0.3))
#'
#' @examples
#' pa <- newparam(iris)
#' pa <- changeparam(pa, "noise", "cols", value=1)
#' pa <- changeparam(pa, "noise", "param", value=c(0,0.1))
#' pa <- changeparam(pa, "noise", "order", value=1)
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

  validatepreprosimparameters(object)

  object

}

validatepreprosimparameters <- function(object){

  exeorder <- getexecutionorder(object)

  isunique <- !any(duplicated(exeorder))
  isinrange <- range(exeorder)[1]==1 & range(exeorder)[2]==8

  if (isunique==FALSE) {stop("No two contaminations can have the same execution order.")}
  if (isinrange==FALSE) {stop("Contamination orders must start from 1 and end to 8.")}

}

getexecutionorder <- function(object){

  paramslots <- names(getSlots("preprosimparameter"))

  cols <- grep("col", paramslots)
  params <- grep("param", paramslots)
  orders <- grep("order", paramslots)

  exeorder <- numeric()
  for (i in 1:length(orders))
  {
    exeorder[i] <- slot(object, paramslots[orders[i]])
  }

  exeorder
}

#' An S4 class representing preprosim data
#'
#'@slot x (data frame) data frame consisting of numeric features
#'@slot y (factor) vector of class labels
#'@slot z (boolean) safe for classification (i.e. no NAs)
#'@keywords internal

setClass("preprosimdata", representation(x="data.frame", y="factor",z="logical"))

createdata <- function(data) {

if(class(data)!="data.frame"){stop("Argument 'data' must be a data frame.")}
if(sum(sapply(data, is.factor)==TRUE)!=1) {stop("Argument 'data' must have one and only one factor column.")}
if(sum(sapply(data, is.numeric)==TRUE)!=ncol(data)-1) {stop("Argument 'data' must have only numeric columns and one factor column.")}
if(any(apply(data, 1:2, is.na))==TRUE) {stop("Argument 'data' must not have missing values.")}

dataclassobject <- new("preprosimdata")
dataclassobject@x <- data[sapply(data, is.numeric)]
dataclassobject@y <- factor(data[sapply(data, is.factor)][,1])
return(dataclassobject)
}

validatedata <- function(data){

  hasvariance <- length(caret::nearZeroVar(data@x))==0
  hasonlyfinite <- !any(apply(data@x, 1:2, function(x) x=="Inf" | x=="-Inf"))
  hasnotnans <- !any(apply(data@x, 1:2, is.nan))

  issafe <- all(hasvariance, hasonlyfinite, hasnotnans)
  data@z <- issafe
  data
}


#' An S4 class representing simulation run results
#'
#'@slot grid (data frame) data frame consisting of combinations of preprosimparameters
#'@slot data (list) list of simulated data sets
#'@slot output (numeric) vector of classification accuracies
#'@slot variableimportance (data frame) data frame consisting of variable importance values
#'@slot outliers (numeric) vector of outlier scores
setClass("preprosimanalysis", representation(grid="data.frame", data="list", output="numeric", variableimportance="data.frame", outliers="numeric"),
         prototype(output=NULL, variableimportance=NULL, outliers=NULL))



