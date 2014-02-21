\name{addLine}
\alias{addLine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add a "homogeneous" line to a plot.
}
\description{
Add a homogeneous line to a plot. The line parameters must be in homogeneous coordinates, e.g. \eqn{(a,b,c)}.
}
\usage{
addLine(l, ...)
}

\arguments{
  \item{l}{
    a \eqn{3 \times 1} vector of the homogeneous representation of a line.
  }
  \item{\dots}{
  \link{graphical parameters} such as
    \code{col}, \code{lty} and  \code{lwd}.
  }
}
\details{
  \code{addLine} is based on \code{abline}.
}
%\value{
%}
%\references{

%}
%\author{
%Emanuel Huber
%}
%\note{

%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{

%}
\examples{
# two points in homogeneous coordinates
p1 <- c(3,1,1)
p2 <- c(0,2,1)

# homogeneous line joining p1 and p2
l_12 <- join(p1,p2)
l_12

# plot
plot(0,0,type="n", xlim=c(-2,5),ylim=c(-2,5),asp=1)
points(t(p1))
points(t(p2))
addLine(l_12,col="red",lwd=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~aplot }
