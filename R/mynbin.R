#' Negative Binomial
#'
#' Performs calculations for negative binomial distributions
#'
#' @param y # of trials until rth success
#' @param r # of desired successes
#' @param p probability of success
#'
#' @return
#' @export
#'
#' @examples
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}




