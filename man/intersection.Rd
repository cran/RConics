\name{Intersection with conics}
\alias{intersectConicLine}
\alias{intersectConicConic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Intersections with conics
}
\description{
Point(s) of intersection between a conic and a line, and between two conics in homogeneous coordinates.
}
\usage{
intersectConicLine(C, l)
intersectConicConic(C1,C2)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{C, C1, C2}{
    \eqn{(3 \times 3)} matrix representation of conics.
  }
  \item{l}{
    a \eqn{(3 \times 3)} vector of the homogeneous representation of a line.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  The homogeneous coordinates of the intersection points. If there are two points of intersection, it returns a \eqn{(3 \times 2)} matrix whose columns correspond to the homogeneous coordinates of the intersection points. If there is only one point, a \eqn{(3 \times 1)} vector of the homogeneous coordinates of the intersection point is returned. If there is no intersection, \code{NULL} is returned.
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
  # Ellipse with semi-axes a=8, b=2, centered in (0,0), with orientation angle = -pi/3
  C <- ellipseToConicMatrix(c(8,2),c(0,0),-pi/3)
  
  # Ellipse with semi-axes a=5, b=2, centered in (1,-2), with orientation angle = pi/5
  C2 <- ellipseToConicMatrix(c(5,2),c(1,-2),pi/5)
  
  
  # line
  l <- c(0.25,0.85,-3)
  
  # intersection conic C with line l:
  p_Cl <- intersectConicLine(C,l)
  
  # intersection conic C with conic C2
  p_CC2 <- intersectConicConic(C,C2)
  
  # plot
  plot(ellipse(c(8,2),c(0,0),-pi/3),type="l",asp=1)
  lines(ellipse(c(5,2),c(1,-2),pi/5), col="blue")
  addLine(l,col="red")
  points(t(p_Cl), pch=20,col="red")
  points(t(p_CC2), pch=20,col="blue")
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
