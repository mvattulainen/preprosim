
#' Plot data quality simulation results
#' @param type (character) type of plot: accuracy, varimportance, outliers
#' @param object (preprosimanalysis class object) object to be plotted
#' @export

preprosimplot <- function(object, type, xaxis){

  if (type=="accuracy") {

    plotdata <- data.frame(accuracy=object@output)
    plotdata$index <- seq(1,length(object@output),1)
    p <- ggplot(plotdata, aes(y=accuracy, x=index)) + geom_boxplot() + theme_bw() + coord_flip()
  }

  if (type=="varimportance") {

    plotdata <- data.frame(variableimportance=object@variableimportance)
    plotdata <- reshape2::melt(plotdata)
    p <- ggplot(plotdata, aes(factor(variable), value)) + geom_boxplot() + theme_bw() + coord_flip()

  }

  if (type=="outliers") {

    plotdata <- data.frame(outlierscore=object@outliers)
    p <- ggplot(plotdata, aes(x=outlierscore)) + geom_density() + theme_bw()

  }

  if (type=="accuracybyx") {

    x <- "misval"
    z <- "noise"

    selection <- paste("grid.",x, sep="")
    selectionz <- paste("grid.",z, sep="")

    plotdata <- data.frame(grid=object@grid, accuracy=object@output)

    colnames(plotdata)[which(colnames(plotdata)==selection)] <- "x"
    colnames(plotdata)[which(colnames(plotdata)==selectionz)] <- "z"

    others <- which(!colnames(plotdata) %in% c("x", "z", "accuracy"))

    test <- apply(plotdata[,others], 1, function(x) all(x==0))
    plotdata <- plotdata[test,]

    #plotdata <- reshape2::melt(plotdata, id.vars=c("accuracy","x"))

    p <- ggplot(plotdata, aes(x=factor(x), y=accuracy, group = 1)) + geom_point() + geom_line() + theme_bw() + facet_wrap(~ z)

  }




p

}





