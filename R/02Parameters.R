# S4: default parameter

#' preprosimdefaultparam
#'
#' defaultparameters include eight data contaminations each having three parameter levels
#' @export

preprosimdefaultparam <- new("preprosimparameter", noisecol=1, noiseparam=c(0, 0.5), noiseorder=1,
                    lowvarcol=2, lowvarparam=c(0, 0.1), lowvarorder=2,
                    misvalcol=1:4, misvalparam=c(0, 0.1, 0.2, 0.3, 0.4), misvalorder=8,
                    irfeaturecol=4, irfeatureparam=c(0, 0.1), irfeatureorder=3,
                    classswapcol=4, classswapparam=c(0, 0.1), classswaporder=4,
                    classimbalancecol=4, classimbalanceparam=c(0, 0.1), classimbalanceorder=5,
                    volumedecreasecol=4, volumedecreaseparam=c(0, 0.1), volumedecreaseorder=6,
                    outliercol=4, outlierparam=c(0, 0.1), outlierorder=7
                    )


## IMPORTS

#' @import ggplot2
NULL

#' @importFrom methods setClass new prototype
NULL

globalVariables(c("accuracy","index", "variable", "value", "outlierscore"))

