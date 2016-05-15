
#' Run data quality simulation
#' @param seed (integer) seed to be used for reproducible results, defaults to 1
#' @param data (data frame) one factor columns for class labels, other columns numeric, no missing values
#' @param param (preprosimparameter object) simulation parameters, defaults to preprosimdefaultparameters
#' @param holdoutrounds (integer) number of holdout rounds,defaults to 10
#' @return preprosimanalysis class object
#' @examples
#' ## res1 <- preprosimrun(iris, holdoutrounds=3) # RUNTIME SIX MINUTES on Intel Celeron 1.4 GHz
#' ## res2 <- preprosimrun(iris, preprosimdefaultparam, seed=10, holdoutrounds=20)
#' @export

preprosimrun <- function(data, param=preprosimdefaultparam, seed=1, holdoutrounds=10) {

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

  for (i in 1:nrow(q))

    {

    data <- datainput

      for (j in 1:ncol(q)){

          param <- changeparam(param, contamination=w[j], param="param", value=q[i,j])
          data <- eval(parse(text=forder[j]))

          }

    e[[i]] <- data

}

  print("Creation of data sets completed.")
  print("Computation of classification accuracies in progress.")




## MISCLAS ERROR

output <- vector("numeric", length=nrow(q))
varimportance <- vector("list", length=nrow(q))


for (i in 1:nrow(q))

{
  accuracy <- numeric()

      for (j in 1:holdoutrounds){
        training <- caret::createDataPartition(e[[i]]@y, times = 1, p = 0.66, list=FALSE)
        model <- rpart::rpart(e[[i]]@y[training] ~., e[[i]]@x[training,])
        testPred <- predict(model, e[[i]]@x[-training,], type="class")
        accuracy[j] <- mean(testPred==e[[i]]@y[-training])
      }

        output[[i]] <- mean(accuracy)
        varimportance[[i]] <- model$variable.importance

}

###






varimportance <- plyr::ldply(varimportance, rbind)
varimportance[is.na(varimportance)] <- 0

expdata <- data.frame(output, q)

outlier.scores <- DMwR::lofactor(expdata, k=5)
outliers <- data.frame(outlier.scores)

preprosimobject <- new("preprosimanalysis")
preprosimobject@grid <- q
preprosimobject@data <- e
preprosimobject@output <- output
preprosimobject@variableimportance <- varimportance
preprosimobject@outliers <- outlier.scores

print("Computation of classification accuracies completed.")

preprosimobject
}

















