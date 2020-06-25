#' @title Calculate empirical confidence interval
#' @description a function to calculate empirical confidence interval
#' @author Xiaohua Douglas Zhang  05/2020
#' @param x a vector of values
#' @param alpha to specify 1-alpha confidence interval
#' @examples x = rnorm(1000)
#'   CIempirical.fn(x, alpha=0.5)
#' @return CI
#'
#' @importFrom stats quantile
#'
#' @export
#'
CIempirical.fn = function( x, alpha=0.05)
{
    CI= c( quantile(x, alpha/2), quantile(x, 1-alpha/2) )
    return( CI )
}
