#' @title Display FoldHit
#' @description a function to plot density of measured value to demonstrate FoldHit under homoscedasticity for two groups excluding NA values
#' @author  Xiaohua Douglas Zhang        02/2020
#' @param    FoldHit a vector of FoldHit values for controls
#' @param    color colors mark controls
#' @param    meanHit mean of measured values of a maximally detectable hit
#' @param    meanNeg mean of measured value of the negative reference
#' @param    normCut SSMD value for defining a maximally detectable hit
#' @param    xlab the label for the x-axis
#' @param    ylab the label for the y-axis
#' @param    main the label for the figure title
#' @param    lwd width of lines
#' @param    length.out number of points for showing density
#' @param    round.point number of round points for showing capacity values
#' @examples FoldHit = c( -3.29058, -1.29296)
#'   FoldHit.homoVAR.plot.fn( FoldHit, color=c("green","grey","red", "orange"), xlab="Measured value", ylab="Density", main="" )
#' @return no return value
#'
#' @importFrom graphics plot axis box lines points legend text
#'
#' @export

FoldHit.homoVAR.plot.fn =
function( FoldHit, color=c("green","grey","red", "orange"), meanHit=-1, meanNeg=0, normCut=3,
          xlab="Measured value", ylab="Density", main="", lwd=3, length.out=1000, round.point=2 )
{
#***********************************************************************************************
# function to plot density of measured value to demonstrate FoldHit under
#   homoscedasticity for two groups excluding NA values
# Author: Xiaohua Douglas Zhang        02/2020
# Augment
#   FoldHit: a vector of FoldHit values for controls
#   color: colors mark controls
#   meanHit: mean of measured values of a maximally detectable hit
#   meanNeg: mean of measured value of the negative reference
#   normCut: SSMD value for defining a maximally detectable hit
#   xlab: the label for the x-axis
#   ylab: the label for the y-axis
#   main: the label for the figure title
#   lwd: width of lines
#   length.out: number of points for showing density
#   round.point: number of round points for showing capacity values
# Example
#   FoldHit = c( -3.29058, -1.29296)
#   FoldHit.homoVAR.plot.fn( FoldHit, color=c("green","grey","red", "orange"),
#          xlab="Measured value", ylab="Density", main="" )
#************************************************************************************************
  nPos = length( FoldHit )
  SD = abs(meanHit - meanNeg)/(3*sqrt(2))
  meanPos = meanNeg + FoldHit*3*sqrt(2)*SD
  yNeg.vec = seq( from=meanNeg-normCut*SD, to=meanNeg+normCut*SD, length.out=length.out)
  dNeg.vec = dnorm(yNeg.vec, mean=meanNeg, sd=SD)
  yHit.vec = seq( from=meanHit-normCut*SD, to=meanHit+normCut*SD, length.out=length.out)
  dHit.vec = dnorm(yHit.vec, mean=meanHit, sd=SD)
  yPos.mat = dPos.mat = matrix(nrow=nPos, ncol=length.out)
  for( i in 1:nPos ) {
    yPos.mat[i,] = seq(from=meanPos[i]-normCut*SD, to=meanPos[i]+normCut*SD, length.out=1000)
    dPos.mat[i,] = dnorm(yPos.mat[i,], mean=meanPos[i], sd=SD)
  }
  xRange = range( c(yNeg.vec, yHit.vec, range(yPos.mat)) )
  yRange = range( c(dNeg.vec, dHit.vec, range(dPos.mat)) )
  plot( xRange, yRange, type="n", xlab=xlab, ylab=ylab, axes=F, main=main)
  lines( rep(meanHit, 2), c(-0.04, max(dHit.vec)*2), col="grey" )
  axis(1, at = c(meanNeg, meanHit), labels=round(c(meanNeg, meanHit),2) , tck = -0.08 )
  axis(1, at = xRange, labels=rep("", 2), tck=0 )
  lines(yNeg.vec, dNeg.vec, col=color[1], lwd=lwd)
  lines(yHit.vec, dHit.vec, col=color[2], lwd=lwd)
  for( i in 1:nPos ) lines(yPos.mat[i,], dPos.mat[i,], col=color[i+2], lwd=lwd)
  for( i in 1:nPos ) lines( rep(meanPos[i],2),  c(-0.04, max(dPos.mat[i,])/20) )
  text( x=c(meanPos), y=rep(max(dPos.mat)/8, 2), labels=round(meanPos, round.point) )
}
