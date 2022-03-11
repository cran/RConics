#' Affine planar transformations matrix
#' 
#' \eqn{(3 \times 3)} affine planar transformation matrix corresponding 
#' to reflection, rotation, scaling and translation in projective geometry. 
#' To transform a point \eqn{p} multiply the transformation matrix \eqn{A} with 
#' the homogeneous coordinates \eqn{(x,y,z)} of \eqn{p} 
#' (e.g. \eqn{p_{transformed} = Ap}).
#' @param alpha the angle made by the line of reflection (in radian). 
#' @param theta the angle of the rotation (in radian).
#' @param pt the homogeneous coordinates of the rotation center (optional). 
#' @param s the \eqn{(2 \times 1)} scaling vector in direction \eqn{x} and \eqn{y}. 
#' @param v the \eqn{(2 \times 1)} translation vector in direction \eqn{x} and \eqn{y}. 
#' @return  A \eqn{(3 \times 3)} affine transformation matrix.
#' @source  Richter-Gebert, Jürgen (2011). 
#' \emph{Perspectives on Projective Geometry - A Guided Tour Through Real
#' and Complex Geometry}, Springer, Berlin, ISBN: 978-3-642-17285-4
#' @examples
#' p1 <- c(2,5,1)  # homogeneous coordinate
#' 
#' # rotation
#' r_p1 <- rotation(4.5) %*% p1
#' 
#' # rotation centered in (3,1)
#' rt_p1 <- rotation(4.5, pt=c(3,1,1)) %*% p1
#' 
#' # translation
#' t_p1 <- translation(c(2,-4)) %*% p1
#' 
#' # scaling
#' s_p1 <- scaling(c(-3,1)) %*% p1
#' 
#' # plot
#' plot(t(p1),xlab="x",ylab="y", xlim=c(-5,5),ylim=c(-5,5),asp=1)
#' abline(v=0,h=0, col="grey",lty=1)
#' abline(v=3,h=1, col="grey",lty=3)
#' points(3,1,pch=4)
#' points(t(r_p1),col="red",pch=20)
#' points(t(rt_p1),col="blue",pch=20)
#' points(t(t_p1),col="green",pch=20)
#' points(t(s_p1),col="black",pch=20)
#' @export
#' @name rotation
#' @rdname transformation

rotation <- function(theta, pt = NULL){
  if(!is.numeric(theta)) stop("theta must be numeric!\n")
  rot <- matrix(c(cos(theta), +sin(theta), 0,-sin(theta),cos(theta),0,0,0,1), nrow=3, byrow=TRUE)
  if(is.null(pt)){
    return(rot)
  }else if(is.vector(pt,mode="numeric") && length(pt)>1){
    return(translation(pt)%*%rot%*%translation(-pt))
  }else{
    stop("Error, pt must be numeric and have at least 2 elements")
  }
  
}

#' @export
#' @name translation
#' @rdname transformation
translation <- function(v){
  matrix(c(1, 0, v[1], 0 ,1, v[2], 0,0,1), nrow=3, byrow=TRUE)
}

#' @export
#' @name scaling
#' @rdname transformation
scaling <- function(s){
  matrix(c(s[1], 0, 0, 0 ,s[2], 0, 0,0,1), nrow=3, byrow=TRUE)
}

#' @export
#' @name reflection
#' @rdname transformation
reflection <- function(alpha){
  rot <- matrix(c(cos(alpha), +sin(alpha), 0,sin(alpha),-cos(alpha),0,0,0,1), nrow=3, byrow=TRUE)
}
