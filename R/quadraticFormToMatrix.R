#' Transformation of the quadratic conic representation into the matrix representation.
#' 
#' Transformation of the quadratic conic representation into the matrix representation.
#' @param v a \eqn{(6 \times 1)} vector of the parameters \eqn{(a, b, c, d, e, f)} 
#' of the quadratic form \eqn{ax^2 + bxy + cy^2 + dxz + eyz + fz^2 = 0}.
#' @return A \eqn{(3 \times 3)} matrix representation of the conic (symmetric matrix).
#' @examples
#' v <- c(2,2,-2,-20,20,10)
#' quadraticFormToMatrix(v)
#' @export
#' @name quadraticFormToMatrix
#' @rdname quadraticFormToMatrix


quadraticFormToMatrix <- function(v){
  if(length(v)!=6 || !all(is.numeric(v)) ){
    stop("v must have 6 elements and be numeric!\n")
  }
  return(matrix(c(v[1], v[2]/2, v[4]/2, 
                  v[2]/2, v[3], v[5]/2, 
                  v[4]/2, v[5]/2, v[6]), 
                nrow=3, ncol=3))
}
