#' My Confidence interval
#' Finds 95% confidence interval for mean of sample d
#'
#' @param d sample for which a confidence interval is being calculated
#' @param ...
#'
#' @return confidence interval values
#' @export
#'
#' @examples
#' d=c(1,2,3,4,5), myci(d)
myci<-function(d,...){
  dmean = mean(d)
  dsd = sd(d)
  n = length(d)
  ta2= qt(1-0.05/2,n-1)
  ci=c()
  ci[1]=dmean - (ta2 * dsd/sqrt(n))
  ci[2]=dmean + (ta2 * dsd/sqrt(n))
  ci
}
