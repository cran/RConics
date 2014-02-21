\name{ellipseToConicMatrix}
\alias{ellipseToConicMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transformation of the ellipse parameters into the matrix representation
}
\description{
Transformation of the ellipse parameters (Cartesian coordinates of the ellipse center, length of the semi-axes and angle of rotation) into the \eqn{(3 \times 3)} into the matrix representation of conics.
}
\usage{
ellipseToConicMatrix(saxes = c(1, 1), loc = c(0, 0), theta = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{saxes}{
    a \eqn{(2 \times 1)} vector of the length of the ellipse semi-axes.
  }
  \item{loc}{
   a \eqn{(2 \times 1)} vector of the Cartesian coordinates of the ellipse center.
  }
  \item{theta}{
    the angle of rotation of the ellipse (in radians).
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A \eqn{(3 \times 3)} matrix that represents the ellipse.
}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%Emanuel Huber
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{conicMatrixToEllipse}}
}
\examples{
  # Ellipse parameters
  saxes <- c(5,2)
  loc <- c(0,0)
  theta <- pi/4
  # Matrix representation of the ellipse
  C <- ellipseToConicMatrix(saxes,loc,theta)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
