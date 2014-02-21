\name{Join, meet and parallel}
\alias{meet}
\alias{join}
\alias{parallel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The join and meet of two points and the parallel
}
\description{
The join operation of two points is the cross-product of these two points and represents the line passing through them. The meet operation of two lines is the cross-product of these two lines and represents their intersection. The line parallel to a line \eqn{l} and passing through the point \eqn{p} corresponds to the join of \eqn{p} with the meet of \eqn{l} and the line at infinity.
}
\usage{
join(p,q)
meet(l, m)
parallel(p,l)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p, q}{
    \eqn{(3 \times 1)} vectors of the homogeneous coordinates of a point.
  }
  \item{l, m}{
    \eqn{(3 \times 1)} vectors of the homogeneous representation of a line.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A \eqn{(3 \times 1)} vector of either the homogeneous coordinates of the meet of two lines (a point), the homogeneous representation of the join of two points (line), or the homogeneous representation of the parallel line. The vector has the form \eqn{(x,y,1)}.
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
  p <- c(3,1,1)
  q <- c(0,2,1)
  l <- c(0.75,0.25,1)
  
  # m is the line passin through p and q
  m <- join(p,q)
  
  # intersection point of m and l
  ml <- meet(l,m)
  
  # line parallel to l and through p
  lp <- parallel(p,l)
  
  # plot
  plot(rbind(p,q),xlim=c(-5,5),ylim=c(-5,5))
  abline(h=0,v=0,col="grey",lty=3)
  addLine(l,col="red")
  addLine(m,col="blue")
  points(t(ml),cex=1.5,pch=20,col="blue")
  addLine(lp,col="green")
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
