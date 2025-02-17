#' Arc length of an ellipse
#' 
#' This function computes the arc length of an ellipse centered in \eqn{(0,0)} 
#' with the semi-axes aligned with the \eqn{x}- and \eqn{y}-axes. 
#' The arc length is defined by the points \eqn{1} and \eqn{2}. 
#' These two points do not need to lie exactly on the ellipse: 
#' the \eqn{x}-coordinate of the points and the quadrant where they lie 
#' define the positions on the ellipse used to compute the arc length.  
#' 
#' If the coordinates \code{p2} of the point \eqn{2} are omitted the function 
#' \code{arcLengthEllipse} computes the arc length between the point \eqn{1} 
#' and the point defined by \eqn{(0,b)}, \eqn{b} beeing the minor semi-axis.
#' 
#' @param p1 a \eqn{(2 \times 1)} vector of the Cartesian coordinates of point \eqn{1}.
#' @param p2 a \eqn{(2 \times 1)} vector of the Cartesian coordinates of point \eqn{2} (optional).
#' @param saxes a \eqn{(2 \times 1)} vector of length of the semi-axes of the ellipse.
#' @param n the number of iterations used in the numerical approximation of the incomplete elliptic integral of the second kind.
#' @return  The length of the shortest arc of the ellipse defined by the 
#'           points 1 and 2.
#' @source  Van de Vel, H. (1969).
#' \emph{On the series expansion method for Computing incomplete elliptic 
#' integrals of the first and second kinds}, Math. Comp. 23, 61-69.
#' @seealso \code{\link{pEllipticInt}}
#' @examples
#' p1 <- c(3,1)
#' p2 <- c(0,2)
#' 
#' # Ellipse with semi-axes: a = 5, b= 2
#' saxes <- c(5,2)
#' 
#' # 1 iteration
#' arcLengthEllipse(p1,p2,saxes,n=1)
#' 
#' # 5 iterations
#' arcLengthEllipse(p1,p2,saxes,n=5)
#' 
#' # 10 iterations
#' arcLengthEllipse(p1,p2,saxes,n=10)
#' @export
#' @name arcLengthEllipse
#' @rdname arcLengthEllipse
arcLengthEllipse <- function(p1,p2=NULL,saxes,n=5){
  if( (abs(p1[1]) - saxes[1]) >  .Machine$double.eps^0.75) stop("x-coordinate of point 1 larger/smaller than a!\n")
  L1 <- pEllipticInt(as.numeric(p1[1]),saxes,n=n)
  if(!is.null(p2)){
    if( abs(p2[1]) - saxes[1] > .Machine$double.eps^0.75) stop("x-coordinate of point 2 larger/smaller than a!\n")
    L2 <- pEllipticInt(as.numeric(p2[1]),saxes,n=n)
    # check 1 - m?me quadrant
    if( (length(p1)==1 && length(p2)==1) || sign(p1[2]) == sign(p2[2])){
      #
      LT <- abs(L1 - L2)		
    }else{
      # arc length of a quadrant
      Lqua <- pEllipticInt(as.numeric(saxes[1]),saxes,n=n)
      LT <- 2*Lqua - abs(max(L1,L2)) + abs(min(L1,L2)) 
    }
    return(LT)
  }else{
    return(L1)
  }
}
