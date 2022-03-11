#' \eqn{(3 \times 3)} skew symmetric matrix
#' 
#' Return a \eqn{(3 \times 3)} skew symmetric matrix from three parameters \eqn{(\lambda, \mu, \tau)}.
#' @param p a \eqn{(3 \times 1)} vector \eqn{(\lambda, \mu, \tau)}
#' @return  A \eqn{(3 \times 3)} skew symmetric matrix, with :
#' \itemize{
#'   \item \eqn{A_{1,2}  = -A_{2,1} = \tau}
#'   \item \eqn{-A_{1,3} =  A_{3,1} = \mu}
#'   \item \eqn{A_{3,2}  = -A_{2,3} = \lambda}
#' }
#' @source  Richter-Gebert, Jürgen (2011). 
#' \emph{Perspectives on Projective Geometry - A Guided Tour Through Real
#' and Complex Geometry}, Springer, Berlin, ISBN: 978-3-642-17285-4
#' @examples
#' p <- c(3,7,11)
#' skewSymmetricMatrix(p)
#' @export
#' @name skewSymmetricMatrix
#' @rdname skewSymmetricMatrix

skewSymmetricMatrix <- function(p){
  matrix(c(0,p[3], -p[2],-p[3], 0, p[1],p[2], -p[1], 0),byrow=TRUE,ncol=3,nrow=3)
}
