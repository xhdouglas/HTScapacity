#' @title calculate FoldHit with error control
#' @description a function to search for FoldHit0 corresponding to the estimated value of FoldHit
#' @author Xiaohua Douglas Zhang        02/2020
#' @param Y1 a value or a vector for the first group (reference group)
#' @param Y2 a vector for the second group
#' @param Alpha the significant level
#' @param length.out the length of vectors of FoldHit0 candidate values for search FoldHit0.
#' @param minDIFF If the difference between the input FoldHitC and the calculated FoldHitC corresponding to a FoldHit0 value, then stop the calculation process and return this calculated value as what we want
#' @examples  y1 = rnorm(12, 0, 1); y2 = rnorm(12, -3, 1)
#'   FoldHit.homoVAR.UMVUE.fn(y1, y2)
#'   FoldHit0.homoVAR.fn(y1, y2, Alpha=0.05, length.out=1000, minDIFF=0.001)
#'   y1 = -y1; y2 = -y2
#'   FoldHit.homoVAR.UMVUE.fn(y1, y2)
#'   FoldHit0.homoVAR.fn(y1, y2, Alpha=0.05, length.out=1000, minDIFF=0.001)
#' @return FoldHit0
#'
#' @importFrom stats var
#'
#' @export

FoldHit0.homoVAR.fn = function(Y1, Y2, Alpha=0.05, length.out=1000, minDIFF=0.01 )
{
#*************************************************************************************
# function to search for FoldHit0 corresponding to the estimated value of FoldHit
# Author: Xiaohua Douglas Zhang        02/2020
# Augment
#   Y1: a value or a vector for the first group (reference group)
#   Y2: a vector for the second group
#   Alpha: significant level
#   length.out: the length of vectors of FoldHit0 candidate values for search FoldHit0.
#   minDIFF: if the difference between the input FoldHitC and the calculated FoldHitC
#            corresponding to a FoldHit0 value, then stop the calculation process and
#            return this calculated value as what we want
# Example
#   y1 = rnorm(12, 0, 1); y2 = rnorm(12, -3, 1)
#   FoldHit.homoVAR.UMVUE.fn(y1, y2)
#   FoldHit0.homoVAR.fn(y1, y2, Alpha=0.05, length.out=1000, minDIFF=0.001)
#   y1 = -y1; y2 = -y2
#   FoldHit.homoVAR.UMVUE.fn(y1, y2)
#   FoldHit0.homoVAR.fn(y1, y2, Alpha=0.05, length.out=1000, minDIFF=0.001)
#*************************************************************************************
  n1 = sum( !is.na(Y1) )
  n2 = sum( !is.na(Y2) )
  FoldHitC = FoldHit.homoVAR.UMVUE.fn(Y1, Y2)
  FoldHit0 = FoldHit0core.homoVAR.fn(FoldHitC, n1, n2, Alpha, length.out, minDIFF)
  return( FoldHit0 )
}

