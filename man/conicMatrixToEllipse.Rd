\name{conicMatrixToEllipse}
\alias{conicMatrixToEllipse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Transformation of the matrix representation of an ellipse into the ellipse parameters
}
\description{
Ellipses can be represented by a \eqn{(3 \times 3)} matrix \eqn{A}, such that for each point \eqn{x} on the ellipse \eqn{x^T A x = 0}. The function \code{conicMatrixToEllipse} transforms the matrix \eqn{A} into the ellipse parameters: center location, semi-axes length and angle of rotation.
}
\usage{
conicMatrixToEllipse(A)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
    a \eqn{(3 \times 3)} matrix representation of an ellipse.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{loc }{  a \eqn{(2 \times 1)} vector of the Cartesian coordinates of the ellipse center.}
  \item{saxes }{a \eqn{(2 \times 1)} vector of the length of the ellipse semi-axes.}
  \item{theta }{the angle of rotation of the ellipse (in radians).}
}
\references{
    Wolfram, Mathworld (\url{http://mathworld.wolfram.com/}).
}
%\author{
%Emanuel Huber
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ellipseToConicMatrix}}
}
\examples{
# ellipse parameter
saxes <- c(5,2)
loc <- c(0,0)
theta <- pi/4
# matrix representation of the ellipse
C <- ellipseToConicMatrix(saxes,loc,theta)
C
# back to the ellipse parameters
conicMatrixToEllipse(C)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
