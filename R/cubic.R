#' Roots of the cubic equation.
#' 
#' Return the roots of a cubic equation of the form \eqn{ax^3 + bx^2 + cx + d=0}.
#'
#' @param p a \eqn{(4 \times 1)} vector of the four parameters \eqn{(a, b, c, d)} of the cubic equation.
#' @return  A vector corresponding to the roots of the cubic equation.
#' @source  W. H. Press, S.A. Teukolsky, W.T. Vetterling, B.P. Flannery (2007). 
#' \emph{NUMERICAL RECIPES - the art of scientific computing}. 
#' Cambridge, University Press, chap 5.6, p. 227-229.
#' @examples 
#' # cubic equation x^3 - 6x^2 + 11x - 6 = 0
#' # parameter
#' b <- c(1,-6, 11, -6)
#' 
#' # roots
#' x0 <- cubic(b)
#' 
#' # plot
#' x <- seq(0,4, by=0.001)
#' y <- b[1]*x^3 + b[2]*x^2 + b[3]*x + b[4]
#' 
#' # plot
#' plot(x,y,type="l")
#' abline(h=0,v=0)
#' points(cbind(x0,c(0,0,0)), pch=20,col="red",cex=1.8)
#' @export
#' @name cubic
#' @rdname cubic

cubic <- function(p){
  if(abs(p[1])<.Machine$double.eps^0.95){
    stop('Coefficient of highest power must not be zero!\n')
  } 
  if(!all(is.numeric(p)) || length(p) != 4){
    stop('p is not a numeric or/and has not 4 elements!\n')
  } 
  if(any(is.complex(p))){
    stop("the coefficient must be real")
  }
  a <- numeric(3)
  for(i in 2:4) {a[i-1]=p[i]/p[1]}
  Q <- (a[1]^2 - 3*a[2])/9
  R <- (2*a[1]^3 - 9*a[1]*a[2]+27*a[3])/54
  x <- numeric(3)
  # case 1 - 3 real roots
  if(R^2 < Q^3){
    theta <- acos(R/sqrt(Q^3))
    x[1] <- -2 * sqrt(Q) * cos(theta/3) - a[1]/3
    x[2] <- -2 * sqrt(Q) * cos((theta + 2*pi)/3) - a[1]/3
    x[3] <- -2 * sqrt(Q) * cos((theta - 2*pi)/3) - a[1]/3
  }else{
    A = - sign(R)*( abs(R) + sqrt(R^2-Q^3))^(1/3)
    if(isTRUE(all.equal(0, A))){
      B <- 0
    }else{
      B <- Q/A
    }
    x[1] <- (A + B) - a[1]/3
    x[2] <- -0.5*(A+B) - a[1]/3 + sqrt(3)*complex(real=0,imaginary=1)*(A-B)/2
    x[3] <- -0.5*(A+B) - a[1]/3 - sqrt(3)*complex(real=0,imaginary=1)*(A-B)/2
  }
  return(x)
}
