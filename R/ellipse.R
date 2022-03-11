#' Return ellipse points
#' 
#' Return ellipse points. Usefull for ploting ellipses.
#' 
#' \describe{
#'   \item{\code{"default"}}{returns points according to the polar equation;}
#'   \item{\code{"angle"}}{returns points radially equidistant;}
#'   \item{\code{"distance"}}{ returns points that are equidistant on the ellipse arc.}
#' }
#' @param saxes a \eqn{(2 \times 1)} vector of the length of the ellipse semi-axes.
#' @param loc  a \eqn{(2 \times 1)} vector of the Cartesian coordinates of the ellipse center.
#' @param theta the angle of rotation of the elllipse (in radians).
#' @param n the number of points returned by the function.
#' @param method The method used to return the points: either \code{"default"}, \code{"angle"}, or \code{"distance"} (see Details).
#' @return A \eqn{(n \times 2)} matrix whose columns correspond to the Cartesian coordinates of the points lying on the ellipse.
#' @examples
#' # Ellipse parameters
#' saxes <- c(5,2)
#' loc <- c(0,0)
#' theta <- pi/4
#' 
#' # Plot
#' plot(ellipse(saxes, loc, theta, n=500),type="l")
#' points(ellipse(saxes, loc, theta, n=30),pch=20,col="red")
#' points(ellipse(saxes, loc, theta, n=30, method="angle"),pch=20,col="blue")
#' points(ellipse(saxes, loc, theta, n=30, method="distance"),pch=20,col="green")
#' @export
#' @name ellipse
#' @rdname ellipse

ellipse <- function(saxes=c(1,1), loc = c(0,0), theta = 0, n = 201, 
                    method=c("default","angle","distance")){
  method = match.arg(method, c("default","angle","distance"))  
  if(method=="default"){
    phi <- 2*pi*seq(0,1, len = n)
    P <- matrix(nrow=n,ncol=2)
    P[,1] <- saxes[1] * cos(phi)
    P[,2] <- saxes[2] * sin(phi)
  }else if(method=="angle"){
    b <- min(saxes[1],saxes[2])
    a <- max(saxes[1],saxes[2])
    d2 <- (a-b)*(a+b)                   #= a^2 - b^2
    phi <- 2*pi*seq(0,1, len = n)
    sp <- sin(phi)
    cp <- cos(phi)
    r <- a*b / sqrt((saxes[2] * cp)^2 +  (saxes[1] * sp)^2)
    P <- matrix(nrow=n,ncol=2)
    P[,1] <- r * cp
    P[,2] <- r * sp
  }else if(method=="distance"){
    n <- round(n/4)*4
    phi <- 0.5*pi*seq(0,1, by = 1/n)
    P <- matrix(nrow=length(phi),ncol=2)
    P[,1] <- saxes[1] * cos(phi)
    P[,2] <- saxes[2] * sin(phi)
    d2<-c(0,cumsum(sqrt(apply((diff(P))^2,1,sum))))
    phi_new <- approx(d2,phi,xout = seq(0,tail(d2, n=1), length.out=n/4))$y
    phi_new <- phi_new[-length(phi_new)]
    phi_new <- c(phi_new, pi/2,pi - phi_new, phi_new + pi,3*pi/2, 2*pi - phi_new)
    P <- matrix(nrow=length(phi_new),ncol=2)
    P[,1] <- saxes[1] * cos(phi_new)
    P[,2] <- saxes[2] * sin(phi_new)
  }
  if(theta != 0){
    P <- P %*% matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),byrow=TRUE,nrow=2,ncol=2)
  }
  P <- P + matrix(loc[1:2],nrow=nrow(P),ncol=2,byrow=TRUE)
  return(P)
}
