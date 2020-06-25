#' @title Calculate FoldHit with error control in the middle
#' @description a core function to search for FoldHit0 corresponding to FoldHitC (FoldHit critical value)
#' @author Xiaohua Douglas Zhang        02/2020
#' @param   n1 sample size for the first group (reference group)
#' @param   n2 sample size for the second group
#' @param   Alpha significant level
#' @param   FoldHitC the critical value of FoldHit
#' @param   length.out the length of vectors of FoldHit0 candidate values for search FoldHit0.
#' @param   minDIFF if the difference between the input FoldHitC and the calculated FoldHitC corresponding to a FoldHit0 value, then stop the calculation process and return this calculated value as what we want
#' @examples FoldHit0core.homoVAR.fn(FoldHitC=-1.121588, n1=304, n2=16, Alpha=0.05, minDIFF=0.001)
#' @return FoldHit0
#'
#' @export

FoldHit0core.homoVAR.fn = function(FoldHitC, n1, n2, Alpha, length.out=1000, minDIFF=0.01 )
{
#*************************************************************************************
# core function to search for FoldHit0 corresponding to FoldHitC (FoldHit critical value)
# Author: Xiaohua Douglas Zhang        02/2020
# Augment
#   n1, n2: sample size in two groups respectively
#   Alpha: significant level
#   FoldHitC: the critical value of FoldHit
#   length.out: the length of vectors of FoldHit0 candidate values for search FoldHit0.
#   minDIFF: if the difference between the input FoldHitC and the calculated FoldHitC
#            corresponding to a FoldHit0 value, then stop the calculation process and
#            return this calculated value as what we want
# Example:
#   FoldHit0core.homoVAR.fn(FoldHitC=-1.121588, n1=304, n2=16, Alpha=0.05, minDIFF=0.001)
#*************************************************************************************
  FoldHit0.vec = seq(FoldHitC, 0, length.out=length.out)
  for( i in 1:length.out ) {
    FoldHit0 = FoldHit0.vec[i]
    FoldHit0C = FoldHitC.homoVAR.fn(n1, n2, Alpha, FoldHit0)
	Diff = abs(FoldHitC - FoldHit0C)
	if( Diff < minDIFF ) {break}
	if(i==1) {Diff.vec = Diff} else {Diff.vec = c(Diff.vec, Diff)}
  }
  if( length(Diff.vec) == length.out ) {
     idx = which(Diff.vec==min(Diff.vec))[1]
	 FoldHit0 = FoldHit0.vec[idx]
	 warning(paste("The absolute difference between FoldHitC and the critical value from FoldHit0 is ",
         Diff.vec[idx], "which does not reach 'minDIFF'. Please consider to increase 'length.out' ",
		 "or decrease 'minDIFF' and then re-run the program."))
  }
  return(FoldHit0)
}
