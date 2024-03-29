% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colinear.R
\name{colinear}
\alias{colinear}
\title{Test for colinearity}
\source{
Richter-Gebert, Jürgen (2011). 
\emph{Perspectives on Projective Geometry - A Guided Tour Through Real
and Complex Geometry}, Springer, Berlin, ISBN: 978-3-642-17285-4
}
\usage{
colinear(p1, p2, p3)
}
\arguments{
\item{p1}{\eqn{(3 \times 1)} vector of the homogeneous coordinates of point \eqn{1}.}

\item{p2}{\eqn{(3 \times 1)} vector of the homogeneous coordinates of point \eqn{2}.}

\item{p3}{\eqn{(3 \times 1)} vector of the homogeneous coordinates of point \eqn{3}.}
}
\value{
\code{TRUE} if the three points are colinear, else \code{FALSE}.
}
\description{
Tests if three points are colinear. The coordinates of the points have to be in homogeneous coordinates.
}
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
