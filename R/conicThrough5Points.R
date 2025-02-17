#' Compute the conic that passes through 5 points
#'
#'Return the matrix representation of the conic that passes through exactly 5 points.
#' @param p1 \eqn{(3 \times 1)} vectors of the homogeneous coordinates of one of the five points.
#' @param p2 \eqn{(3 \times 1)} vectors of the homogeneous coordinates of one of the five points.
#' @param p3 \eqn{(3 \times 1)} vectors of the homogeneous coordinates of one of the five points.
#' @param p4 \eqn{(3 \times 1)} vectors of the homogeneous coordinates of one of the five points.
#' @param p5 \eqn{(3 \times 1)} vectors of the homogeneous coordinates of one of the five points.
#' @return A \eqn{(3 \times 3)} matrix representation of the conic passing through the 5 points.
#' @source  Richter-Gebert, Jürgen (2011). 
#' \emph{Perspectives on Projective Geometry - A Guided Tour Through Real
#' and Complex Geometry}, Springer, Berlin, ISBN: 978-3-642-17285-4
#' @examples
#' # five points
#' p1 <- c(-4.13, 6.24, 1)
#' p2 <- c(-8.36, 1.17, 1)
#' p3 <- c(-2.03, -4.61, 1)
#' p4 <- c(9.70, -3.49, 1)
#' p5 <- c(8.02, 3.34, 1)
#' 
#' # matrix representation of the conic passing
#' # through the five points
#' C5 <- conicThrough5Points(p1,p2,p3,p4,p5)
#' 
#' # plot
#' plot(rbind(p1,p2,p3,p4,p5),xlim=c(-10,10), ylim=c(-10,10), asp=1) 
#' # from matrix to ellipse parameters
#' E5 <- conicMatrixToEllipse(C5)
#' lines(ellipse(E5$saxes, E5$loc, E5$theta, n=500))
#' @export
#' @name conicThrough5Points
#' @rdname conicThrough5Points

conicThrough5Points <- function(p1,p2,p3,p4,p5){
  g1 <- join(p1, p3)
  g2 <- join(p2, p4)
  h1 <- join(p1, p4)
  h2 <- join(p2, p3)
  G <- g1 %*% t(g2)
  H <- h1 %*% t(h2)
  M <- as.numeric(t(p5) %*% H %*% p5) * G - as.numeric(t(p5) %*% G %*% p5) * H
  M + t(M)
}
