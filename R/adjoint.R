#' Adjoint matrix
#' 
#' Compute the classical adjoint (also called adjugate) of a square matrix. The adjoint is the transpose of the cofactor matrix.
#' @param A a square matrix.
#' @return The adjoint matrix of A (square matrix with the same dimension as A).
#' @seealso \code{\link{cofactor}}, \code{\link{minor}}
#' @examples
#' A <- matrix(c(1,4,5,3,7,2,2,8,3),nrow=3,ncol=3)
#' A
#' B <- adjoint(A)
#' B
#' @export
#' @name adjoint
#' @rdname adjoint


adjoint <- function(A) {
  n <- nrow(A)
  t(outer(1:n, 1:n, Vectorize(
    function(i,j) cofactor(A,i,j)
  )))
}

