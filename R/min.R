#' @title Take the minimum using R
#' @description Take the minimum using R
#' @param X the input sequence
#' @return the minimum of X
#' @examples
#' \dontrun{
#' x=c(311,365,343,377,254)
#' y=min(x)
#' print(y)
#' }
#' @export
min=function(x){
  y=x[1]
  n=length(x)
  for(i in 1:n-1){
    if(y<x[i+1])  y=y
    else y=x[i+1]
  }
  return(y)
}
