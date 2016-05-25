
fitmodels <- function(e,caretmodel="gbm", holdoutrounds){

output <- vector("numeric", length=length(e))
varimportance <- vector("list", length=length(e))

# Tune parameters
fitControl <- caret::trainControl(method="boot", number=3, savePredictions=TRUE)
dftune <- data.frame(class=e[[1]]@y, e[[1]]@x)
modtune <- caret::train(class ~., data=dftune, method=caretmodel, trControl=fitControl, na.action = na.pass, verbose = FALSE)
tuning <- modtune$bestTune

modelpackage <- modtune$modelInfo$library[1]

for (i in 1:length(e))

{


  # Variable importance
  dfvarimp <- data.frame(class=e[[i]]@y, e[[i]]@x)
      tryCatch({
        modvarimp <- caret::train(class ~., data=dfvarimp, method=caretmodel, tuneGrid=tuning, trControl=caret::trainControl(method="none"), na.action = na.pass, verbose = FALSE)
        tempvarimportance <- caret::varImp(modvarimp, scale = FALSE)[[1]]
      }, error= function(e) return({tempvarimportance <- NA}) )
      varimportance[[i]] <- tempvarimportance




    accuracy <- foreach(j=1:holdoutrounds, .combine='c', .packages=c('caret', 'randomForest', modelpackage)) %dopar% {

        training <- caret::createDataPartition(dfvarimp$class, times=1, list=FALSE, p=0.66)[,1]
        intrain <- dfvarimp[training,]
        intest <- dfvarimp[-training,]

        tryCatch({
          modres <- caret::train(class ~., data=intrain, method=caretmodel, tuneGrid=tuning, trControl=caret::trainControl(method="none"), na.action = na.pass, verbose = FALSE)

          prediction <- predict(modres, newdata=intest, na.action = na.pass)
          accuracy <- mean(prediction==intest$class) # accuracy[j]

          }, error= function(e) return({accuracy[j] <- NA}) )

        }
      output[i] <- mean(accuracy)
      cat(round(output[i],2), " ")

    }
list (output, varimportance)
}



