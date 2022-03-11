#' Split degenerate conic
#' 
#' Split a degenerate conic into two lines.
#' 
#' @param C a \eqn{(3 \times 3)} matrix representation of a degenerate conic.
#' @return A \eqn{(3 \times 2)} matrix whose columns correspond to the homongeneous representation of two lines (real or complex).
#' @source  Richter-Gebert, Jürgen (2011). 
#' \emph{Perspectives on Projective Geometry - A Guided Tour Through Real
#' and Complex Geometry}, Springer, Berlin, ISBN: 978-3-642-17285-4
#' @examples
#' # tw0 lines
#' g <- c(0.75,0.25,3)
#' h <- c(0.5,-0.25,2)
#' 
#' # a degenerate conic 
#' D <- g %*% t(h) + h %*% t(g)
#' 
#' # split the degenerate conic into 2 lines
#' L <- splitDegenerateConic(D)
#' 
#' # plot
#' plot(0,0,xlim=c(-10,5),ylim=c(-10,10),type="n")
#' addLine(L[,1],col="red")
#' addLine(L[,2],col="green")
#' @export
#' @name splitDegenerateConic
#' @rdname splitDegenerateConic


splitDegenerateConic <- function(C){
  if( !isTRUE(all.equal(0,det(C)))) stop("The conic defined by C is not degenerated\n")
  if(qr(C)$rank==2){
    B <- -adjoint(C)
    i <- which(diag(B)!=0)
    if(length(i)<1) stop("problem 1")
    bet <- sqrt(as.complex(B[i[1],i[1]]))
    p <- B[,i[1]]/bet
    A <- C + skewSymmetricMatrix(p)
  }else if(qr(C)$rank == 1){
    A <- C
  }
  ij <- which(A!=0, arr.ind=TRUE)
  if(nrow(ij) < 1)  stop("problem 2")
  P <- matrix(nrow=3,ncol=2)
  P[,1] <- A[ij[1,1],]
  P[,2] <- A[,ij[1,2]]
  return(P)
}
