\name{adjoint}
\alias{adjoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adjoint matrix
}
\description{
Compute the classical adjoint (also called adjugate) of a square matrix. The adjoint is the transpose of the cofactor matrix.
}
\usage{
adjoint(A)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
  a square matrix.
}
}
%\details{

%}
\value{
The adjoint matrix of A (square matrix with the same dimension as A).
}
%\references{
%
%}
%\author{
%Emanuel Huber
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{cofactor}}, \code{\link{minor}}
}
\examples{
A <- matrix(c(1,4,5,3,7,2,2,8,3),nrow=3,ncol=3)
A
B <- adjoint(A)
B
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~array }
%\keyword{ ~algebra }

