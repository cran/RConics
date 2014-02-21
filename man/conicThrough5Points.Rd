\name{conicThrough5Points}
\alias{conicThrough5Points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Compute the conic that passes through 5 points
}
\description{
Return the matrix representation of the conic that passes through exactly 5 points.
}
\usage{
conicThrough5Points(p1, p2, p3, p4, p5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p1, p2, p3, p4, p5}{
    \eqn{(3 \times 1)} vectors of the homogeneous coordinates of each of the five points.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A \eqn{(3 \times 3)} matrix representation of the conic passing through the 5 points.
}
\references{
Richter-Gebert, Jürgen (2011). \emph{Perspectives on Projective Geometry - A Guided Tour Through Real and Complex Geometry}, Springer, Berlin, ISBN: 978-3-642-17285-4
}
%\author{
%Emanuel Huber
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
# five points
p1 <- c(-4.13, 6.24, 1)
p2 <- c(-8.36, 1.17, 1)
p3 <- c(-2.03, -4.61, 1)
p4 <- c(9.70, -3.49, 1)
p5 <- c(8.02, 3.34, 1)

# matrix representation of the conic passing
# through the five points
C5 <- conicThrough5Points(p1,p2,p3,p4,p5)

# plot
plot(rbind(p1,p2,p3,p4,p5),xlim=c(-10,10), ylim=c(-10,10), asp=1) 
# from matrix to ellipse parameters
E5 <- conicMatrixToEllipse(C5)
lines(ellipse(E5$saxes, E5$loc, E5$theta, n=500))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
