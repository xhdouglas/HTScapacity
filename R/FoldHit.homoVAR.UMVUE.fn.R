#' @title Calculate FoldHit
#' @description a function to calculate the UMVUE of FoldHit under homoscedasticity for two groups excluding NA values
#' @author: Xiaohua Douglas Zhang        02/2020
#' @param Y1 a value or a vector for the first group (reference group)
#' @param Y2 a vector for the second group
#' @examples y1 = rnorm(12, 0, 1); y2 = rnorm(12, 3, 1)
#'   FoldHit.homoVAR.UMVUE.fn(y1, y2)
#' @return FoldHit
#'
#' @importFrom stats var
#'
#' @export

FoldHit.homoVAR.UMVUE.fn = function(Y1, Y2)
{
#*******************************************************************************************
# function to calculate the UMVUE of FoldHit under homoscedasticity for
#   two groups excluding NA values
# Author: Xiaohua Douglas Zhang        02/2020
# Augment
#   Y1: a value or a vector for the first group (reference group)
#   Y2: a vector for the second group
# Example
#   y1 = rnorm(12, 0, 1); y2 = rnorm(12, 3, 1)
#   FoldHit.homoVAR.UMVUE.fn(y1, y2)
#********************************************************************************************
  n1 = sum( !is.na(Y1) )
  n2 = sum( !is.na(Y2) )
  mean1 = mean(Y1, na.rm=TRUE)
  mean2 = mean(Y2, na.rm=TRUE)
  var1 = var(Y1, na.rm=TRUE)
  var2 = var(Y2, na.rm=TRUE)
  DF = n1 + n2 - 2
  C = sqrt( (1/n1+1/n2)/DF ) * exp( lgamma(DF/2)-lgamma((DF-1)/2) )
  tValue = (mean2-mean1)/sqrt( (1/n1+1/n2)/DF*( (n1-1)*var1+(n2-1)*var2 ) )

  FoldHit = C/3*tValue
  return(FoldHit)
}
