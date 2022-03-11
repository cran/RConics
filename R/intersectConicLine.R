#' Intersections between a conic and a line
#'
#' Returns the point(s) of intersection between a conic and a line in homogeneous coordinates.
#' @param C \eqn{(3 \times 3)} matrix representation of conics.
#' @param l a \eqn{(3 \times 3)} vector of the homogeneous representation of a line.
#' @return The homogeneous coordinates of the intersection points. 
#' If there are two points of intersection, it returns a \eqn{(3 \times 2)} 
#' matrix whose columns correspond to the homogeneous coordinates of the 
#' intersection points. If there is only one point, a \eqn{(3 \times 1)} 
#' vector of the homogeneous coordinates of the intersection point is returned. 
#' If there is no intersection, \code{NULL} is returned.
#' @source  Richter-Gebert, Jürgen (2011). 
#' \emph{Perspectives on Projective Geometry - A Guided Tour Through Real
#' and Complex Geometry}, Springer, Berlin, ISBN: 978-3-642-17285-4
#' @examples
#' #' # Ellipse with semi-axes a=8, b=2, centered in (0,0), with orientation angle = -pi/3
#' C <- ellipseToConicMatrix(c(8,2),c(0,0),-pi/3)
#' 
#' # line
#' l <- c(0.25,0.85,-3)
#' 
#' # intersection conic C with line l:
#' p_Cl <- intersectConicLine(C,l)
#' 
#' # plot
#' plot(ellipse(c(8,2),c(0,0),-pi/3),type="l",asp=1)
#' addLine(l,col="red")
#' points(t(p_Cl), pch=20,col="red")
#' @export
#' @name intersectConicLine
#' @rdname intersectConicLine

intersectConicLine <- function(C,l){  
  if(isTRUE(all.equal(c(0,0,0),l))){
    warning("Line undefined (0,0,0), return NULL\n")
    return(NULL)
  }
  M <- skewSymmetricMatrix(l)
  B <- t(M) %*% C %*% M
  l[abs(l) < .Machine$double.eps^0.5]  <-0
  i <- which(l!=0)
  subB_index <- c(1:3)[-i[1]]
  detB <- B[subB_index[1],subB_index[1]]*B[subB_index[2],subB_index[2]] - B[subB_index[1],subB_index[2]]*B[subB_index[2],subB_index[1]]
  if(Re(detB) < 0){
    alpha  <- (1/l[i[1]]) * sqrt(- detB )
    A <- B + alpha*M
    # index for a non-zero value
    idn0 <- which(A!=0, arr.ind=TRUE)
    if(nrow(idn0)>0){
      np <- c(A[idn0[1,1],3]!=0, A[3,idn0[1,2]]!=0)
      P <- matrix(nrow=3,ncol=2)
      P[,1] <- A[idn0[1,1],]  /A[idn0[1,1],3]
      P[,2] <- A[,idn0[1,2]]	/A[3,idn0[1,2]]
      return( P[,np] )
    }
  }
  return(NULL)
}
