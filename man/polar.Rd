\name{polar}
\alias{polar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Polar line of point with respect to a conic
}
\description{
Return the polar line \eqn{l} of a point \eqn{p} with respect to a conic with matrix representation \eqn{C}. The polar line \eqn{l} is defined by \eqn{l = Cp}.
}
\usage{
polar(p, C)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
    a \eqn{(3 \times 1)} vector of the homogeneous coordinates of a point.
  }
  \item{C}{
     a \eqn{(3 \times 3)} matrix representation of the conic.
}
}
\details{
The polar line of a point \eqn{p} on a conic is tangent to the conic on \eqn{p}.
}
\value{
  A  \eqn{(3 \times 1)} vector of the homogeneous representation of the polar line.
}
\references{
Richter-Gebert, Jürgen (2011). \emph{Perspectives on Projective Geometry - A Guided Tour Through Real and Complex Geometry}, Springer, Berlin, ISBN: 978-3-642-17285-4
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
  # Ellipse with semi-axes a=5, b=2, centered in (1,-2), with orientation angle = pi/5
  C <- ellipseToConicMatrix(c(5,2),c(1,-2),pi/5)
  
  # line
  l <- c(0.25,0.85,-1)
  
  # intersection conic C with line l:
  p_Cl <- intersectConicLine(C,l)
  
  # if p is on the conic, the polar line is tangent to the conic
  l_p <- polar(p_Cl[,1],C)
  
  # point outside the conic
  p1 <- c(5,-3,1)
  l_p1 <- polar(p1,C)
  
  # point inside the conic
  p2 <- c(-1,-4,1)
  l_p2 <- polar(p2,C)
  
  # plot
  plot(ellipse(c(5,2),c(1,-2),pi/5),type="l",asp=1, ylim=c(-10,2))
  # addLine(l,col="red")
  points(t(p_Cl[,1]), pch=20,col="red")
  addLine(l_p,col="red")
  points(t(p1), pch=20,col="blue")
  addLine(l_p1,col="blue")
  points(t(p2), pch=20,col="green")
  addLine(l_p2,col="green")
  
  # DUAL CONICS
  saxes <- c(5,2)
  theta <- pi/7
  E <- ellipse(saxes,theta=theta, n=50)
  C <-  ellipseToConicMatrix(saxes,c(0,0),theta)
  plot(E,type="n",xlab="x", ylab="y", asp=1)
  points(E,pch=20)
  E <- rbind(t(E),rep(1,nrow(E)))
  
  All_tangant <- polar(E,C)
  apply(All_tangant, 2, addLine, col="blue")
  
} 
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
