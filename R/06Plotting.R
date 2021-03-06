
#' Get simulation run result data
#' @param type (character) type of data: accuracy, varimportance, outliers or xz
#' @param object (preprosimanalysis class object) object
#' @param x (character) x axis contamination
#' @param z (character) z axis contamination
#' @details contaminations are : noise, lowvar, misval, irfeature, classswap, classimbalance, volumedecrease, outlier
#' @examples
#' ## res <- preprosimrun(iris)
#' ## getpreprosimdata(res, "accuracy")
#' ## getpreprosimdata(res, type="xz", x="misval", z="noise")
#' @export


getpreprosimdata <- function(object, type="accuracy", x, z){

  if (is.null(object@output)) {stop("Model was not fitted by preprosimrun function. There are no results that can be plotted.")}

  if (type=="accuracy") {

    plotdata <- data.frame(accuracy=object@output)
    plotdata$index <- seq(1,length(object@output),1)
  }

  if (type=="varimportance") {

    plotdata <- object@variableimportance
    plotdata <- reshape2::melt(plotdata)
  }

  if (type=="outliers") {

    plotdata <- data.frame(outlierscore=object@outliers)

  }

  if (type=="xz") {

    selection <- paste("grid.",x, sep="")
    selectionz <- paste("grid.",z, sep="")

    plotdata <- data.frame(grid=object@grid, accuracy=object@output)

    colnames(plotdata)[which(colnames(plotdata)==selection)] <- "x"
    colnames(plotdata)[which(colnames(plotdata)==selectionz)] <- "z"

    others <- which(!colnames(plotdata) %in% c("x", "z", "accuracy"))

    test <- apply(plotdata[,others], 1, function(x) all(x==0))
    plotdata <- plotdata[test,]
  }

  plotdata

}

#' Get a contaminated data frame
#' @param object (preprosimanalysis class object) object to be plotted
#' @param paramvector (numeric) contamination combinations to be searched for
#' @examples
#' ## res <- preprosimrun(iris)
#' ## df <- preprosimdf(res, c(0,0,0,0,0,0,0,0)) # returns uncontaminated original data set
#' @export

getpreprosimdf <- function(object, paramvector){

  if (length(paramvector)!=8){stop("Argument 'param' must be a numeric vector of length eight, one parameter for each contamination.")}

  row.is.a.match <- apply(object@grid, 1, function(x) identical(as.numeric(paste(x)), paramvector))
  match.idx <- which(row.is.a.match)

  if (length(match.idx)==0){stop("There were no contamination combinations matching parameter 'param'. Please, check yourobjectname@grid")}

  s4data <- object@data[[match.idx]]
  data <- data.frame(x=s4data@x, y=s4data@y)

}








#' Plot simulation run results
#' @param type (character) type of plot: accuracy, varimportance, outliers or xz; defaults to accuracy
#' @param object (preprosimanalysis class object) object to be plotted
#' @param x (character) x axis contamination
#' @param z (character) z axis contamination plotted as panels
#' @details contaminations are : noise, lowvar, misval, irfeature, classswap, classimbalance, volumedecrease, outlier
#' @examples
#' ## res <- preprosimrun(iris)
#' ## preprosimplot(res)
#' ## preprosimplot(res, type="xz", x="misval", z="noise")
#' @export

preprosimplot <- function(object, type="accuracy", x, z){

  if (type=="accuracy") {

    plotdata <- getpreprosimdata(object, type="accuracy")
    p <- ggplot(plotdata, aes(y=accuracy, x=index)) + geom_boxplot() + theme_bw() + coord_flip() + ggtitle("Classification accuracy")
  }

  if (type=="varimportance") {

    plotdata <- getpreprosimdata(object, type="varimportance")
    p <- ggplot(plotdata, aes(factor(variable), value)) + geom_boxplot() + theme_bw() + coord_flip() + ggtitle("Variable importance")

  }

  if (type=="outliers") {

    plotdata <- getpreprosimdata(object, type="outliers")
    p <- ggplot(plotdata, aes(x=outlierscore)) + geom_density() + theme_bw() + ggtitle("Outlier score")

  }

  if (type=="xz") {

    plotdata <- getpreprosimdata(object, type="xz", x, z)

    p <- ggplot(plotdata, aes(x=factor(x), y=accuracy, group = 1)) + geom_point() + geom_line() + theme_bw() + facet_wrap(~ z) + ggtitle("Effect")

  }

suppressWarnings(p)

}





