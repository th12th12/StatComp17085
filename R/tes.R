#' @title A two-sample Cramer-von Mises test using R
#' @description Implement the two-sample Cramer-von Mises test for equal distributions as a permutation test.
#' @param X  The data under an certain distribution
#' @param Y  The data under an certain distribution
#' @return the test chart
#' @examples
#' \dontrun{
#' attach(chickwts)
#' x <- sort(as.vector(weight[feed == "soybean"]))
#' y <- sort(as.vector(weight[feed == "linseed"]))
#' }
#' @export
tes=function(x,y){
  f <- function(x1,x2){
    Fx1<-ecdf(x1)
    Fx2<-ecdf(x2)
    n<-length(x1)
    m<-length(x2)
    w1<-sum((Fx1(x1)-Fx2(x1))^2)+sum((Fx1(x2)-Fx2(x2))^2)
    w2<-w1*m*n/((m+n)^2)
    return(w2)
  }
  t=f(x,y)
  m=c(x,y)
  z <- numeric(1000)
  m1=length(m)
  m2=length(x)
  q=1:m1
  for (i in 1:1000) {
    d= sample(q, size = m2, replace = FALSE)
    q1 <- m[d]
    q2 <- m[-d]
    z[i] <- f(q1,q2)
  }
  p <- mean(abs(c(t, z)) >= abs(t))
  p
  hist(z, main = "", freq = FALSE, xlab = "Cramer-von Mises statistic", breaks = "scott")
  points(t, 0)
}
