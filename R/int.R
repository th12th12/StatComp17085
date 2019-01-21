#' @title Interval estimation using R
#' @description One side or two side interval estimation in any confidence using R
#' @param x the inpput data
#' @param sigma the overall standard deviation
#' @param side one side or two side interval estimation
#' @param alpha cofidence
#' @return the interval
#' @examples
#' \dontrun{
#' sigma=-1
#' side=0
#' alpha=0.05
#' x= c(28,26,33,24,34,-44,27,16,40,-2,29,22,24,21,25,30,23,29,31,19)
#' interval_estimated(x,sigma,side,alpha)
#' }
#' @export
interval_estimated <- function(x, sigma, side, alpha){
  n <- length(x); xb <- mean(x)
  if (sigma >= 0){
    if (side < 0){
      tmp <- sigma/sqrt(n)*qnorm(1-alpha)
      a <- -Inf; b <- xb+tmp
    } else if (side > 0){
      tmp <- sigma/sqrt(n)*qnorm(1-alpha)
      a <- xb-tmp; b <- Inf
    } else{
      tmp <- sigma/sqrt(n)*qnorm(1-alpha/2)
      a <- xb-tmp; b <- xb+tmp
    }
    df <- n
  } else{
    if (side < 0){
      tmp <- sd(x)/sqrt(n)*qt(1-alpha,n-1)
      a <- -Inf; b <- xb+tmp
    } else if(side > 0){
      tmp <- sd(x)/sqrt(n)*qt(1-alpha,n-1)
      a <- xb-tmp; b <- Inf
    } else{
      tmp <- sd(x)/sqrt(n)*qt(1-alpha/2,n-1)
      a <- xb-tmp; b <- xb+tmp
    }
    df <- n-1
  }
  data.frame(a=a, b=b)
}
