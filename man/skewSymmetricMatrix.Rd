\name{skewSymmetricMatrix}
\alias{skewSymmetricMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\eqn{(3 \times 3)} skew symmetric matrix
}
\description{
Return a \eqn{(3 \times 3)} skew symmetric matrix from three parameters \eqn{(\lambda, \mu, \tau)}.
}
\usage{
skewSymmetricMatrix(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
    a \eqn{(3 \times 1)} vector \eqn{(\lambda, \mu, \tau)}.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A \eqn{(3 \times 3)} skew symmetric matrix, with :
  \itemize{
    \item \eqn{A_{1,2}  = -A_{2,1} = \tau}
    \item \eqn{-A_{1,3} =  A_{3,1} = \mu}
    \item \eqn{A_{3,2}  = -A_{2,3} = \lambda}
  }
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
p <- c(3,7,11)
skewSymmetricMatrix(p)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
