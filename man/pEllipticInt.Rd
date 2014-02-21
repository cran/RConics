\name{pEllipticInt}
\alias{pEllipticInt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Partial elliptic integral
}
\description{
Partial elliptic integral
}
\usage{
pEllipticInt(x, saxes, n = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    the \eqn{x}-coordinate.
  }
  \item{saxes}{
    a \eqn{(2 \times 1)} vector of the length of the ellipse semi-axes.
}
  \item{n}{
    the number of iterations.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  Return the partial elliptic integral.
}
\references{
Van de Vel, H. (1969). \emph{On the series expansion method for Computing incomplete elliptic integrals of the first and second kinds}, Math. Comp. 23, 61-69.
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{arcLengthEllipse}}
}
\examples{
# Ellipse with semi-axes: a = 5, b= 2
saxes <- c(5,2)

# 1 iteration
pEllipticInt(3,saxes,n=1)
# 5 iterations
pEllipticInt(3,saxes,n=5)
# 10 iterations
pEllipticInt(3,saxes,n=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
