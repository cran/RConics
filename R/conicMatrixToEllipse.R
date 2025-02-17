#' @export
conicMatrixToEllipse <- function(A){
  if(!all.equal(dim(A),c(3,3))) stop("Conic matrix should be of size 3x3\n")
  # if rank 3 and if detB > 0 => ellipse
  # detB = the top-left 2×22×2 submatrix of AA,
  detB <- A[1,1]*A[2,2] - A[1,2]^2
  if(!(qr(A)$rank == 3 && detB > 0)) stop("This is not an ellipse \n")
  b2mac <- (A[1,2]^2-A[1,1]*A[2,2])
  # location
  x0 <- (A[2,2]*A[1,3] - A[1,2]*A[2,3])/b2mac
  y0 <- (A[1,1]*A[2,3] - A[1,2]*A[1,3])/b2mac
  # parameter (semi-axis)
  NUM <- 2*(A[1,1]*A[2,3]^2 + A[2,2]*A[1,3]^2 + A[3,3]*A[1,2]^2 - 2*A[1,2]*A[1,3]*A[2,3]-A[1,1]*A[2,2]*A[3,3])
  sqrtac24b2 <- sqrt((A[1,1]-A[2,2])^2 + 4*A[1,2]^2)
  ac <- A[1,1] + A[2,2]
  a <- sqrt(NUM/(b2mac*(sqrtac24b2 - ac)))
  b <- sqrt(NUM/(b2mac*(-sqrtac24b2 - ac)))
  # theta (orientation)
  if(isTRUE(all.equal(b,0)) && A[1,1] < A[2,2]){
    theta <- 0
  }else if(isTRUE(all.equal(b,0)) && A[1,1] > A[2,2]){
    theta <- 0.5*pi
  }else if(!isTRUE(all.equal(b,0)) && A[1,1] < A[2,2]){
    theta <- 0.5*atan(2*A[1,2]/(A[1,1]-A[2,2]))
  }else if(!isTRUE(all.equal(b,0)) && A[1,1] > A[2,2]){
    theta <- pi/2 + 0.5*atan(2*A[1,2]/(A[1,1]-A[2,2]))
  }
  return(list(loc=c(x0,y0), saxes=c(a,b), theta = theta))
}
