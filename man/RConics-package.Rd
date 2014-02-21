\name{RConics-package}
\alias{RConics-package}
\alias{RConics}
\docType{package}
\title{
Computations on conics
}
\description{
Solve some conic related problems (intersection of conics 
with lines and conics, arc length of an ellipse, polar lines, etc.).
}
\details{
\tabular{ll}{
Package: \tab RConics\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-02-21\cr
License: \tab GPL 2\cr
}

}
\author{
Emanuel Huber

Maintainer: Emanuel Huber <emanuel.huber@unibas.ch>

}


\references{
Richter-Gebert, Jürgen (2011). \emph{Perspectives on Projective Geometry - A Guided Tour Through Real and Complex Geometry}, Springer, Berlin, ISBN: 978-3-642-17285-4
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation ~~
%~~ directory ~~

\note{
    Some of the functions are based on the \emph{projective} geometry. 
    In projective geometry parallel lines meet at an infinite point and all infinite points are incident to a line at infinity. Points and lines of a projective plane are represented by \emph{homogeneous} coordinates, that means by 3D vectors: \eqn{(x, y, z)} for the points and \eqn{(a, b, c)} such that \eqn{ax + by + c = 0} for the lines. The Euclidian points correspond to \eqn{(x, y, 1)}, the infinite points to \eqn{(x, y, 0)}, the Euclidian lines to \eqn{(a, b, c)} with \eqn{a \neq 0} or \eqn{b \neq 0}, the line at infinity to \eqn{(0, 0, 1)}.
    
    \strong{Advice}: to plot conics use the package \code{conics} from Bernard Desgraupes.
    
    This work was funded by the Swiss National Science Foundation within the ENSEMBLE project (grant no. CRSI_132249). 
}

\keyword{ conic }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{
%# some examples
%}
