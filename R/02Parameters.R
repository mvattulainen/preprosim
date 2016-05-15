# S4: default parameter

#' Default parameters for Iris simulation runs
#'
#' Defaultparameters include eight data contaminations: noise, low variance, missing values,
#' irrelevant features, class label swap, class imbalance, volume decrease and outliers.
#'
#' For each contamination there is column (integer vector) the contamination is applied to
#' in the Iris dataset, parameters for contamination (integer vector) corresponding to zero,
#' ten percent and twenty percent contamination intensity and order in which contaminations
#' are applied.
#'
#' There are total 6561 combinations of parameters in the preprosimdefaultparameters.
#' @export

preprosimdefaultparam <- new("preprosimparameter", noisecol=1:4, noiseparam=c(0, 0.1, 0.2), noiseorder=1,
                    lowvarcol=2, lowvarparam=c(0, 0.1, 0.2), lowvarorder=2,
                    misvalcol=1:4, misvalparam=c(0, 0.1, 0.2), misvalorder=8,
                    irfeaturecol=0, irfeatureparam=c(0, 0.1, 0.2), irfeatureorder=3,
                    classswapcol=0, classswapparam=c(0, 0.1, 0.2), classswaporder=4,
                    classimbalancecol=0, classimbalanceparam=c(0, 0.1, 0.2), classimbalanceorder=5,
                    volumedecreasecol=0, volumedecreaseparam=c(0, 0.1, 0.2), volumedecreaseorder=6,
                    outliercol=3, outlierparam=c(0, 0.1, 0.2), outlierorder=7
                    )


## IMPORTS

#' @import ggplot2
NULL

#' @importFrom methods setClass new prototype getSlots slot
NULL

#'@importFrom stats IQR predict rnorm runif
NULL

globalVariables(c("accuracy","index", "variable", "value", "outlierscore"))

