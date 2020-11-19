#' Binomial
#'
#' Builds a graph for a binomial distribution
#'
#' @param iter number of iterations of the experiment
#' @param n number of trials
#' @param p probability of success
#'
#' @return graph of distribution
#' @export
#'
#' @examples
#' n = 10 trials, with probability of success p = 0.6 for each. 100 iterations of the experiment. mybin(100,10,0.6)
mybin=function(iter=100,n=10, p=0.5){
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
