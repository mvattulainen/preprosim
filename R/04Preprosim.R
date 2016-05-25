
#' Run data quality simulation
#' @param data (data frame) one factor columns for class labels, other columns numeric, no missing values
#' @param param (preprosimparameter object) simulation parameters, defaults to parameters set automatically for data.
#' @param seed (integer) seed to be used for reproducible results, defaults to 1
#' @param caretmodel (character) a model from package Caret, defaults to gbm (gbm must be installed before preprosimrun)
#' @param holdoutrounds (integer) number of holdout rounds, defaults to 10
#' @param cores (integer) number of cores used in parallel processing, defaults to 1
#' @return preprosimanalysis class object
#' @examples
#' ## res <- preprosimrun(iris)
#' ## res1 <- preprosimrun(iris, param=newparam(iris, "custom", x="misval", z="noise"), cores=2)
#' ## res2 <- preprosimrun(iris, caretmodel="rpart")
#' @export

preprosimrun <- function(data, param=newparam(data, "default"), seed=1, caretmodel="gbm", holdoutrounds=10, cores=1) {

  doParallel::registerDoParallel(cores=cores)


  print("Creation of data sets in progress.")

  set.seed(seed)

  datainput <- createdata(data)

   ## FIX THIS To POINT to FUNCTION ARGUMENT

  ## READ PARAMETERS

  # Get the functions to be executed and their execution order

  paramslots <- names(getSlots("preprosimparameter"))

  cols <- grep("col", paramslots)
  params <- grep("param", paramslots)
  orders <- grep("order", paramslots)

  # get the execution order
  exeorder <- numeric()
  for (i in 1:length(orders))
  {
    exeorder[i] <- slot(param, paramslots[orders[i]])
  }
  exeorder <- order(exeorder)
  exeorder2 <- orders[exeorder]

  # get the functions in execution order

  funcorder <- exeorder2+1
  forder <- character() # functions to be executed
  for (i in 1:length(funcorder))
  {
    forder[i] <- slot(param, paramslots[funcorder[i]])
  }

  # get the parameters in execution order

  paramorder2 <- params[exeorder]
  paramorder2 <- exeorder2-1

  forder2 <- list() # functions to be executed
  for (i in 1:length(paramorder2))
  {
    forder2[[i]] <- slot(param, paramslots[paramorder2[i]])
  }

  # create grid

  q <- expand.grid(forder2)
  w <- sub("function(data, param)", "", forder, fixed = TRUE)
  colnames(q) <- w

  e <- vector("list", length=nrow(q))

  ## NOT IN USE : e <- foreach::foreach(i=iterators::icount(length(nrow(q)))) %dopar% {

  for (i in 1:nrow(q)){

    data <- datainput

      for (j in 1:ncol(q)){

          param <- changeparam(param, contamination=w[j], param="param", value=q[i,j])
          data <- eval(parse(text=forder[j]))

          }

    e[[i]] <- data

  }

  print("Data sets created.")
  print("Validating the data sets.")
  #r <- lapply(e, validatedata)
  print("Data sets validated.")
  print("Computing classification accuracies.")

## MODEL FITTING: CLASSIFICATION ACCURACY AND VARIABLE IMPORTANCE

output <- fitmodels(e, caretmodel, holdoutrounds)

doParallel::stopImplicitCluster()

###

# variable importance

nonemptyelements <- output[[2]][!unlist(lapply(output[[2]], is.null))]
numofrows <- unlist(lapply(nonemptyelements, nrow))
longestdf <- which.max(numofrows)
longestrownames <- rownames(nonemptyelements[[longestdf]])
lengthlongest <- max(numofrows)
#lengthdiff <- lengthlongest - numofrows

# Create list of empty data frames and name rows

emptydf <- data.frame(matrix(nrow=lengthlongest, ncol=1))
rownames(emptydf) <- longestrownames
colnames(emptydf) <- "Overall"

newlist <- vector("list", length=length(nonemptyelements))
for (i in 1:length(newlist))
{
  newdf <- emptydf
  newdf[1:numofrows[i], 1] <- nonemptyelements[[i]][,1]
  newlist[[i]] <- newdf
}

varimportance <- data.frame(t(do.call(cbind, newlist)))

# outlier score
expdata <- data.frame(output[[1]], q)
outlier.scores <- DMwR::lofactor(expdata, k=5)
outliers <- data.frame(outlier.scores)

# analysis object creation
preprosimobject <- new("preprosimanalysis")
preprosimobject@grid <- q
preprosimobject@data <- e
preprosimobject@output <- output[[1]]
preprosimobject@variableimportance <- varimportance
preprosimobject@outliers <- outlier.scores

print("Classification accuracies computed.")

preprosimobject
}

















