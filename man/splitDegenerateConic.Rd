\name{splitDegenerateConic}
\alias{splitDegenerateConic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split degenerate conic
}
\description{
Split a degenerate conic into two lines.
}
\usage{
splitDegenerateConic(C)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{C}{
    a \eqn{(3 \times 3)} matrix representation of a degenerate conic.
}
}

\value{
  A \eqn{(3 \times 2)} matrix whose columns correspond to the homongeneous representation of two lines (real or complex).
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
  # tw0 lines
  g <- c(0.75,0.25,3)
  h <- c(0.5,-0.25,2)
 
  # a degenerate conic 
  D <- g \%*\% t(h) + h \%*\% t(g)
  
  # split the degenerate conic into 2 lines
  L <- splitDegenerateConic(D)
  
  # plot
  plot(0,0,xlim=c(-10,5),ylim=c(-10,10),type="n")
  addLine(L[,1],col="red")
  addLine(L[,2],col="green")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
