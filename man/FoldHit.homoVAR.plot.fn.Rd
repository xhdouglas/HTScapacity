% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FoldHit.homoVAR.plot.fn.R
\name{FoldHit.homoVAR.plot.fn}
\alias{FoldHit.homoVAR.plot.fn}
\title{Display FoldHit}
\usage{
FoldHit.homoVAR.plot.fn(
  FoldHit,
  color = c("green", "grey", "red", "orange"),
  meanHit = -1,
  meanNeg = 0,
  normCut = 3,
  xlab = "Measured value",
  ylab = "Density",
  main = "",
  lwd = 3,
  length.out = 1000,
  round.point = 2
)
}
\arguments{
\item{FoldHit}{a vector of FoldHit values for controls}

\item{color}{colors mark controls}

\item{meanHit}{mean of measured values of a maximally detectable hit}

\item{meanNeg}{mean of measured value of the negative reference}

\item{normCut}{SSMD value for defining a maximally detectable hit}

\item{xlab}{the label for the x-axis}

\item{ylab}{the label for the y-axis}

\item{main}{the label for the figure title}

\item{lwd}{width of lines}

\item{length.out}{number of points for showing density}

\item{round.point}{number of round points for showing capacity values}
}
\value{
no return value
}
\description{
a function to plot density of measured value to demonstrate FoldHit under homoscedasticity for two groups excluding NA values
}
\examples{
FoldHit = c( -3.29058, -1.29296)
  FoldHit.homoVAR.plot.fn( FoldHit, color=c("green","grey","red", "orange"), xlab="Measured value", ylab="Density", main="" )
}
\author{
Xiaohua Douglas Zhang        02/2020
}
