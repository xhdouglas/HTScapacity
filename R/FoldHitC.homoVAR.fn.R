#' @title Calculate the critical value of FoldHit
#' @description a function to calculate the critical value of FoldHit (i.e., FoldHitC)
#' @author Xiaohua Douglas Zhang        02/2020
#' @param   n1 sample size for the first group (reference group)
#' @param   n2 sample size for the second group
#' @param   Alpha significant level
#' @param   FoldHit0 population value of FoldHit
#'
#' @examples FoldHitC.homoVAR.fn(304, 16, Alpha=0.05, FoldHit0=1)
#' @return FoldHitC
#'
#' @importFrom stats qt
#'
#' @export

FoldHitC.homoVAR.fn = function(n1, n2, Alpha, FoldHit0=1)
{
#*****************************************************************************
# function to calculate the critical value of FoldHit (i.e., FoldHitC)
# Author: Xiaohua Douglas Zhang        02/2020
# Augment
#   n1, n2: sample size in two groups respectively
#   Alpha: significant level
#   FoldHit0: population value of FoldHit
# Example:
#   FoldHitC.homoVAR.fn(304, 16, Alpha=0.05, FoldHit0=1)
#******************************************************************************
  DF = n1 + n2 - 2
  C1 = 3/sqrt( 1/2*(1/n1+1/n2) )
  NCP = C1 * FoldHit0
  C2 = sqrt( 2/DF ) * exp( lgamma(DF/2)-lgamma((DF-1)/2) )
  Tcritical = ifelse( FoldHit0 > 0, qt(p=1-Alpha, df=DF, ncp=NCP),
                                   qt(p=Alpha, df=DF, ncp=NCP) )
  FoldHitC = C2/C1*Tcritical
  return(FoldHitC)
}
