#' Intersection between two conics
#' 
#' Returns the point(s) of intersection between two conics in homogeneous coordinates.
#' @param C1 \eqn{(3 \times 3)} matrix representation of conics.
#' @param C2 \eqn{(3 \times 3)} matrix representation of conics.
#' @return  The homogeneous coordinates of the intersection points. 
#' If there are \eqn{k} points of intersection, it returns a \eqn{(3 \times k)} 
#' matrix whose columns correspond to the homogeneous coordinates of the intersection points. 
#' If there is only one point, a \eqn{(3 \times 1)} vector of the homogeneous 
#' coordinates of the intersection point is returned. 
#' If there is no intersection, \code{NULL} is returned.
#' @source  Richter-Gebert, Jürgen (2011). 
#' \emph{Perspectives on Projective Geometry - A Guided Tour Through Real
#' and Complex Geometry}, Springer, Berlin, ISBN: 978-3-642-17285-4
#' @examples
#' # Ellipse with semi-axes a=8, b=2, centered in (0,0), with orientation angle = -pi/3
#' C1 <- ellipseToConicMatrix(c(8,2), c(0,0), -pi/3)
#' 
#' # Ellipse with semi-axes a=5, b=2, centered in (1,-2), with orientation angle = pi/5
#' C2 <- ellipseToConicMatrix(c(5,2), c(1,-2), pi/5)
#' 
#' # intersection conic C with conic C2
#' p_CC2 <- intersectConicConic(C1,C2)
#' 
#' # plot
#' plot(ellipse(c(8,2), c(0,0), -pi/3),type="l",asp=1)
#' lines(ellipse(c(5,2), c(1,-2), pi/5), col="blue")
#' points(t(p_CC2), pch=20,col="blue")
#' @export
#' @name intersectConicConic
#' @rdname intersectConicConic

intersectConicConic <- function(C1,C2){
  alp <- det(t(C1))
  bet <- det(rbind(C1[,1], C1[,2],C2[,3])) + det(rbind(C1[,1],C2[,2],C1[,3])) + det(rbind(C2[,1],C1[,2],C1[,3]))
  gam <- det(rbind(C1[,1],C2[,2],C2[,3])) + det(rbind(C2[,1],C1[,2],C2[,3])) + det(rbind(C2[,1],C2[,2],C1[,3]))
  del <- det(t(C2))
  # solve det(lambda*C1 + C2) = det(CC) = 0, CC = degenerate
  # = solve  alp*lambda[2]^3 + bet*lambda[2]^2 * mu + gam*lambda[2]*mu^2 + del*mu^3
  # with mu = 1 	
  lambda <- cubic(c(alp, bet, gam, del))	# fx cubic 
  # select a real value for lambda
  lambdaRe <- Re(lambda[Im(lambda)==0])
  CC <- lambdaRe[1]*C1 + C2  
  CC[abs(CC)<.Machine$double.eps^0.95] <- 0
  # split the degenerate conic CC into two lines
  BB <- -adjoint(CC)
  # indice for a non-zero element of the diagonal
  idn0 <- which(abs(diag(BB))>.Machine$double.eps^0.95)
  if(length(idn0) > 0){
    Bi <- BB[idn0[1],idn0[1]]
    if(Re(Bi) <0 ){
      return(NULL)
    }else{
      beta2 <- sqrt(Bi)
      p <- BB[,idn0[1]]/beta2
      Mp <- matrix(c(0,p[3], -p[2],-p[3], 0, p[1],p[2], -p[1], 0),byrow=TRUE,ncol=3,nrow=3)
      newC <- CC + Mp 
      idn0_2 <- which(abs(newC)>.Machine$double.eps^0.95, arr.ind=TRUE)
      if(nrow(idn0_2)>0){
        l1 <- newC[idn0_2[1,1],]
        l2 <- newC[,idn0_2[1,2]]
        myP1 <- intersectConicLine(C1,Re(l1))
        myP2 <- intersectConicLine(C1,Re(l2))
        if(all(myP1!=FALSE) && all(myP2!=FALSE)){
          return(cbind(myP1,myP2))
        }else if(all(myP1==FALSE)){
          return(myP2)
        }else if(all(myP2==FALSE)){
          return(myP1)
        }else{
          return(cbind(myP1,myP2))
        }
      }
    } 
    
  }
  return(NULL)
}
