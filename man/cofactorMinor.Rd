\name{(i,j)-cofactor and (i,j)-minor}
\alias{cofactor}
\alias{minor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\eqn{(i,j)}-cofactor and \eqn{(i,j)}-minor of a matrix
}
\description{
 Compute the \eqn{(i,j)}-cofactor, respectively the \eqn{(i,j)}-minor of the matrix \eqn{A}. The \eqn{(i,j)}-cofactor is obtained by multiplying the \eqn{(i,j)}-minor by \eqn{(-1)^{i+j}}. The \eqn{(i,j)}-minor of \eqn{A}, is the determinant of the \eqn{(n - 1) \times (n - 1)} matrix that results by deleting the \eqn{i}-th row and the \eqn{j}-th column of \eqn{A}.
 
}
\usage{
cofactor(A, i, j)
minor(A, i, j)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
    a square matrix.
}
  \item{i}{
    the \eqn{i}-th row.
}
  \item{j}{
    the \eqn{j}-th column.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The \eqn{(i,j)}-minor/cofactor of the matrix \eqn{A} (single value).
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
  \code{\link{adjoint}}
}
\examples{
  A <- matrix(c(1,4,5,3,7,2,2,8,3),nrow=3,ncol=3)
  A
  minor(A,2,3)
  cofactor(A,2,3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
