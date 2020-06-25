#' @title calculate confidence interval from t-distribution
#' @description a function to calculate confidence interval of values from the t-distribution
#' @author Xiaohua Douglas Zhang  05/2020
#' @param x a vector of values
#' @param alpha to specify 1-alpha confidence interval
#' @examples x = rnorm(1000)
#'   CIt.fn(x, alpha=0.5)
#' @return CI
#'
#' @importFrom stats qt sd
#'
#' @export
#'
CIt.fn = function(x, alpha=0.05)
{
   mX = mean(x, na.rm=TRUE)
   n = sum( !is.na(x) )
   me = qt( 1-alpha/2, n-1 )*sd(x)/sqrt(n)
   CI = c(mX-me, mX+me)
   return(CI)
}
