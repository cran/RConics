\name{quadraticFormToMatrix}
\alias{quadraticFormToMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transformation of the quadratic conic representation into the matrix representation.
}
\description{
Transformation of the quadratic conic representation into the matrix representation.
}
\usage{
quadraticFormToMatrix(v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{
    a \eqn{(6 \times 1)} vector of the parameters \eqn{(a, b, c, d, e, f)} of the quadratic form \eqn{ax^2 + bxy + cy^2 + dxz + eyz + fz^2 = 0}.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A \eqn{(3 \times 3)} matrix representation of the conic (symmetric matrix).
}
%\references{
%% ~put references to the literature/web site here ~
%}
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
  v <- c(2,2,-2,-20,20,10)
  quadraticFormToMatrix(v)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
