#' @title Display assay capacity briefly
#' @description a function to plot density of measured value to demonstrate FoldHit under
#'   homoscedasticity for two groups excluding NA values
#' @author Xiaohua Douglas Zhang 05/2020
#' @param FoldHit a vector of FoldHit values for controls, the first element must be 0, the second element must be 1 or -1, the remainings are for positive controls
#' @param color colors mark negative reference, MDhit and positive controls
#' @param xRange range for x-axis
#' @param yRange range for y-axis
#' @param normCut SSMD value for defining a maximally detectable hit
#' @param length.out number of points for showing density
#' @param round.point number of round points for showing capacity values
#' @param lwd width of density lines
#' @param mar margin for the plot
#' @param lineScale value of line in showing scale
#' @param lineXaxis: value of line in showing labels in x-axis
#' @examples FoldHit = c(0, -1, -4.308057, -1.674495)
#'   par(mfrow=c(1,2))
#'   capacityPlot.homoVAR.simple.fn( FoldHit, color=c("green","black","red", "purple"))
#'   capacityPlot.homoVAR.simple.fn( -FoldHit, color=c("green","black","red", "purple"))
#' @return no return value
#'
#' @importFrom graphics plot axis box lines points legend title par mtext
#'
#' @export

capacityPlot.homoVAR.simple.fn =
function( FoldHit, color=c("green","black","red", "purple"), xRange=NULL, yRange=NULL,
          normCut=3, length.out=1000, round.point=2, lwd=3, mar=c(8.1, 4.1, 1.1, 1.1),
          lineScale = c(0.9, 3.7, 6.4), lineXaxis = c(0, 2.8, 5.5) )
{
#*************************************************************************************
# function to plot density of measured value to demonstrate FoldHit under
#   homoscedasticity for two groups excluding NA values
# Author: Xiaohua Douglas Zhang        05/2020
# Augment
#   FoldHit: a vector of FoldHit values for controls, the first element must be 0
#           the second element must be 1 or -1, the remainings are for positive controls
#   color: colors mark negative reference, MDhit and positive controls
#   normCut: SSMD value for defining a maximally detectable hit
#   xRange: range for x-axis
#   yRange: range for y-axis
#   length.out: number of points for showing density
#   round.point: number of round points for showing capacity values
#   lineScale: value of line in showing scale
#   lineXaxis: value of line in showing labels in x-axis
# Example
#   FoldHit = c(0, -1, -4.308057, -1.674495)
#   par(mfrow=c(1,2))
#   capacityPlot.homoVAR.simple.fn( FoldHit, color=c("green","black","red", "purple"))
#   capacityPlot.homoVAR.simple.fn( -FoldHit, color=c("green","black","red", "purple"))
#**************************************************************************************
  par(mar=mar)
  nType = length( FoldHit )
  meanNeg = FoldHit[1]; meanHit = FoldHit[2]
  SD = abs(meanHit - meanNeg)/(3*sqrt(2))
#  meanPos = meanNeg + FoldHit*3*sqrt(2)*SD
  y.mat = d.mat = matrix(nrow=nType, ncol=length.out)
  for( i in 1:nType ) {
    y.mat[i,] = seq(from=FoldHit[i]-normCut*SD, to=FoldHit[i]+normCut*SD, length.out=1000)
    d.mat[i,] = dnorm(y.mat[i,], mean=FoldHit[i], sd=SD)
  }
  if(is.null(xRange)) xRange = range(y.mat)
  if(is.null(yRange)) yRange = range(d.mat)
  yRange[2] = yRange[2]*1.4

  plot( xRange, yRange, type="n", xlab="", ylab="", axes=F)
  mtext("Density                ", side=2, adj=1, line=0)

  for(i in 1:(length(FoldHit)-1) ) {
    rFoldHit = FoldHit/abs(FoldHit[i+1])
	axis(1, at = c(-1000, 1000), labels=rep("", 2), line=lineXaxis[i])
    axis(1, , at = FoldHit, labels=round(rFoldHit, round.point), line=lineXaxis[i] )
	if(i==1) {
	  mtext("FoldHit", at=par('usr')[1], side=1, line=lineScale[i] )
	} else if (nType==3 & i==2) {
	  mtext("rFoldHit", at=par('usr')[1], side=1, line=lineScale[i] )
	} else {
      mtext(paste("rFoldHit", i-1, sep=""), at=par('usr')[1], side=1, line=lineScale[i] )
	}
  }
  for( i in 1:nType ) lines(y.mat[i,], d.mat[i,], col=color[i], lwd=lwd)

  posText = max( abs(FoldHit[-c(1,2)]) )/2*sign(meanHit) + meanHit
  lines(rep(meanHit, 2), c(-0.2, yRange[2])*0.9 )
  arrows(meanHit, yRange[2]*0.8, xRange[(sign(meanHit)+3)/2], yRange[2]*0.8,
         lwd=lwd, col="blue")
  text( posText, yRange[2]*0.9, "    Detectable Hits" )
  text( meanHit, yRange[2], "MDhit" )

  par( mar=c(5.1, 4.1, 4.1, 2.1) )
}
