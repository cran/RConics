#' Transformation of the ellipse parameters into the matrix representation
#' 
#' Transformation of the ellipse parameters (Cartesian coordinates of the 
#' ellipse center, length of the semi-axes and angle of rotation) into the 
#' \eqn{(3 \times 3)} into the matrix representation of conics.
#' 
#' @param saxes a \eqn{(2 \times 1)} vector of the length of the
#'              ellipse semi-axes.
#' @param loc   a \eqn{(2 \times 1)} vector of the Cartesian coordinates of 
#'              the ellipse center.
#' @param theta the angle of rotation of the ellipse (in radians).
#' @return A \eqn{(3 \times 3)} matrix that represents the ellipse.
#' @seealso     \code{\link{conicMatrixToEllipse}}
#' @examples
#' # Ellipse parameters
#' saxes <- c(5,2)
#' loc <- c(0,0)
#' theta <- pi/4
#' # Matrix representation of the ellipse
#' C <- ellipseToConicMatrix(saxes,loc,theta)
#' @export
#' @name ellipseToConicMatrix
#' @rdname ellipseToConicMatrix


ellipseToConicMatrix <- function(saxes=c(1,1), loc=c(0,0), theta = 0){
  if(length(saxes)<2) stop("Arg saxes contains less than 2 elt.\n")
  if(length(loc)<2)  stop("Arg loc contains less than 2 elt.\n")
  if(length(theta)>1){
    theta <- theta[1]
    warning("First element of theta is used\n")
  }
  
  # creation of the matrix in homogeneous coordinates of the ellipse
  CC = matrix(c(1/saxes[1]^2, 0 ,0, 0, 1/saxes[2]^2, 0, 0,0,-1),nrow=3, byrow=TRUE)  
  # Rotation + Translation
  CC <- t(translation(-loc)) %*% t(rotation(theta)) %*% CC %*% rotation(theta) %*% translation(-loc)
  # symmetrization
  CC <- (CC + t(CC))/2
  CC[abs(CC) < .Machine$double.eps^0.95] <- 0
  return(CC)
}

