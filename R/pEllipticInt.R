#' Partial elliptic integral
#' 
#' Partial elliptic integral
#' @param x the \eqn{x}-coordinate.
#' @param saxes  a \eqn{(2 \times 1)} vector of the length of the ellipse semi-axes.
#' @param n the number of iterations.
#' @return  Return the partial elliptic integral.
#' @source 
#' Van de Vel, H. (1969). \emph{On the series expansion method for Computing 
#'     incomplete elliptic integrals of the first and second kinds}, 
#'       Math. Comp. 23, 61-69.
#' @seealso  \code{\link{arcLengthEllipse}}
#' @examples
#' # Ellipse with semi-axes: a = 5, b= 2
#' saxes <- c(5,2)
#' 
#' # 1 iteration
#' pEllipticInt(3,saxes,n=1)
#' # 5 iterations
#' pEllipticInt(3,saxes,n=5)
#' # 10 iterations
#' pEllipticInt(3,saxes,n=10)
#' @export
#' @name pEllipticInt
#' @rdname pEllipticInt

pEllipticInt <- function(x,saxes,n=5){
  theta <- asin(round(x/saxes[1],12))
  # excentricity
  if(saxes[2]>saxes[1]){
    ex <- sqrt(1-(saxes[1]/saxes[2])^2)
  }else{
    ex <- sqrt(1-(saxes[2]/saxes[1])^2)
  }
  I <- theta
  E <- I
  for(i in 1:n){
    I <- (2*i - 1)/(2*i) * I - sin(theta)^(2*i-1) * cos(theta)/(2*i)
    E <- E - abs(choose(-0.5,i)) * ex^(2*i)/(2*i - 1) *  I
  }
  return(as.numeric(saxes[1]*E))
}
