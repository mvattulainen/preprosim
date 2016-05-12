## FUNCTIONS

# Add noise (parameter: variance)
noisefunction <- function(data, param)

{
  if (param@noiseparam==0){return(data)}

  newdata <- apply(data@x[param@noisecol], 2, function(x) noise(x, param))
  newdata <- lapply(seq_len(ncol(newdata)), function(i) newdata[,i])
  data@x[param@noisecol] <- newdata
  data
}

noise <- function(x, param){newvalue <- rnorm(length(x), x, param@noiseparam)}

# Add low variance

lowvarfunction <- function(data, param) {
  if (param@lowvarparam==0){return(data)}

  newdata <- apply(data@x[param@lowvarcol], 2, function(x) lowvariance(x, param@lowvarparam))
  newdata <- lapply(seq_len(ncol(newdata)), function(i) newdata[,i])
  data@x[param@lowvarcol] <- newdata
  data
}

lowvariance <- function(x, lowvarianceparameter) {

  multiplierdifftomean <- lowvarianceparameter * scale(x, scale=FALSE)
  newvalue <- x - multiplierdifftomean
  newvalue <- as.numeric(newvalue)
}

# Add missing values

misvalfunction <- function(data, param) {

  if (param@misvalparam==0){return(data)}

  newdata <- apply(data@x[param@misvalcol], 2, function(x) misvalue(x, param@misvalparam))
  newdata <- lapply(seq_len(ncol(newdata)), function(i) newdata[,i])
  data@x[param@misvalcol] <- newdata
  data
}

misvalue <- function(x, missingvaluesparameter) {

  x[sample(c(TRUE, NA), prob = c(1-missingvaluesparameter, missingvaluesparameter), size = length(x), replace = TRUE)]

}

irfeaturefunction <- function(data, param) {

  if (param@irfeatureparam==0){return(data)}

  numberofirrelevantfeatures <- as.integer(param@irfeatureparam * ncol(data@x))
  basedata <- data@x

    for (i in 1:numberofirrelevantfeatures)
    {
    basedata <- data.frame(basedata, newvar=runif(nrow(data@x), -1, 1))
    }

  data@x <- basedata
  data

}

classswapfunction <- function(data, param) {
  if (param@classswapparam==0){return(data)}


  numberoflabelstobeswapped <- as.integer(param@classswapparam * length(data@y))
  labelstobeswapped <- sample(1:length(data@y), numberoflabelstobeswapped)
  baseclass <- data@y
  alllabels <- levels(data@y)

  for (i in 1:numberoflabelstobeswapped)

    {
    a1 <- labelstobeswapped[i]
    label <- as.character(baseclass[a1])
    optionallabels <- alllabels[!alllabels %in% label]
    newlabel <- sample(optionallabels, 1)
    baseclass[a1] <- newlabel
  }

  data@y <- baseclass
  data

}


classimbalancefunction <- function(data, param){

  if (param@classimbalanceparam==0){return(data)}

  class <- data@y

  q <- as.data.frame(table(class))
  w <- q[order(q$Freq)[1],1]
  e <- which(class==w) # items that belong to the most infrequent class
  numbertoberemoved <- as.integer(length(e)*0.1) # FIX THIS: param@classimbalance
  removed <- sample(e, numbertoberemoved)

  data@y <- data@y[-removed]
  data@x <- data@x[-removed,]

  data

}

volumedecreasefunction <- function(data, param) {

  if (param@volumedecreaseparam==0){return(data)}

a <- as.integer(unname(caret::createDataPartition(data@y, times = 1, p = param@volumedecreaseparam, list=FALSE)))

data@y <- data@y[-a]
data@x <- data@x[-a,]
data
}

outlierfunction <- function(data, param) {

  if (param@outlierparam==0){return(data)}

  newdata <- apply(data@x[param@outliercol], 2, function(x) outlieraux(x, param@outlierparam))
  newdata <- lapply(seq_len(ncol(newdata)), function(i) newdata[,i])
  data@x[param@outliercol] <- newdata
  data
}

outlieraux <- function(x, outlierparameter) {

  a <- summary(x)
  b <- IQR(x)
  d <- as.integer(length(x)*outlierparameter)
  smallestoutlier <- unname(a[5]+1.5*b)
  largestoutlier <- unname(a[5]+2.0*b)
  outliers <- runif(d, smallestoutlier, largestoutlier)
  tobereplaced <- sample(1:length(x),d)
  x[tobereplaced] <- outliers
  x
}


