#' @title Calculate z factor
#' @description a function to calculate plug-in estimate of z factor
#' @author Xiaohua Douglas Zhang
#'
#' @param x a value or a vector for the first group (reference group)
#' @param y a value of a vector for the second group
#' @examples x=rnorm(20)
#'   y=rnorm(30)
#'   zFactor.fn(x, y)
#' @importFrom stats sd
#' @export

zFactor.fn = function(x, y, k=3)
{
#*****************************************************************************
# function to calculate plug-in estimate of z factor
# Author: Xiaohua Douglas Zhang        10/2019
# x: a value or a vector for the first group (reference group)
# y: a vector for the second group.
#******************************************************************************
  meanX = mean(x, na.rm=T);  meanY = mean(y, na.rm=T)
  sdX = sd(x, na.rm=T);    sdY = sd(y, na.rm=T)
  zEst = 1 - k*(sdX + sdY)/abs(meanY - meanX)
  return( zEst )
}
