#' \eqn{(i,j)}-cofactor and \eqn{(i,j)}-minor of a matrix
#' 
#' Compute the \eqn{(i,j)}-cofactor, respectively the \eqn{(i,j)}-minor of 
#' the matrix \eqn{A}. The \eqn{(i,j)}-cofactor is obtained by multiplying 
#' the \eqn{(i,j)}-minor by \eqn{(-1)^{i+j}}. The \eqn{(i,j)}-minor of \eqn{A}, 
#' is the determinant of the \eqn{(n - 1) \times (n - 1)} matrix that results 
#' by deleting the \eqn{i}-th row and the \eqn{j}-th column of \eqn{A}.
#' @param A a square matrix.
#' @param i the \eqn{i}-th row.
#' @param j the \eqn{j}-th column.
#' @return  The \eqn{(i,j)}-minor/cofactor of the matrix \eqn{A} (single value).
#' @seealso \code{\link{adjoint}}
#' @examples
#' A <- matrix(c(1,4,5,3,7,2,2,8,3),nrow=3,ncol=3)
#' A
#' minor(A,2,3)
#' cofactor(A,2,3)


#' @export
#' @name cofactor
#' @rdname cofactor
cofactor <- function(A, i, j){
  (-1)^(i+j) * minor(A,i,j)
}

#' @export
#' @name minor
#' @rdname cofactor
minor <- function(A, i, j){
  det( A[-i,-j] )
} 
