\name{colinear}
\alias{colinear}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test for colinearity
}
\description{
Tests if three points are colinear. The coordinates of the points have to be in homogeneous coordinates.
}
\usage{
colinear(p1, p2, p3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p1}{
    \eqn{(3 \times 1)} vector of the homogeneous coordinates of point \eqn{1}.
  }
  \item{p2}{
    \eqn{(3 \times 1)} vector of the homogeneous coordinates of point \eqn{2}.
  }
  \item{p3}{
    \eqn{(3 \times 1)} vector of the homogeneous coordinates of point \eqn{3}.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\code{TRUE} if the three points are colinear, else \code{FALSE}.
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
# points: homogeneous coordinates
p1 <- c(3,1,1)
p2 <- c(0,2,1)
p3 <- c(1.5,-2,1)
p4 <- c(1,3,1)

# homogeneous line passing through p1 and p2
l1 <- join(p1,p2)

# homogeneous line passing through p3 and p3
l2 <- join(p3,p4)

# homogeneous points formed by the intersection of the lines
p5 <- meet(l1,l2)

# test for colinearity
colinear(p1, p2, p3)
colinear(p1, p2, p5)
colinear(p3, p4, p5)

# plot
plot(rbind(p1,p2,p3,p4),xlim=c(-5,5),ylim=c(-5,5),asp=1)
abline(h=0,v=0,col="grey",lty=3)
addLine(l1,col="red")
addLine(l2,col="blue")
points(t(p5),cex=1.5,pch=20,col="blue")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
