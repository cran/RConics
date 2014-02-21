\name{cubic}
\alias{cubic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Roots of the cubic equation.
}
\description{
  Return the roots of a cubic equation of the form \eqn{ax^3 + bx^2 + cx + d=0}.
}
\usage{
cubic(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
  a \eqn{(4 \times 1)} vector of the four parameters \eqn{(a, b, c, d)} of the cubic equation.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector corresponding to the roots of the cubic equation.
}
\references{
W. H. Press, S.A. Teukolsky, W.T. Vetterling, B.P. Flannery (2007). \emph{NUMERICAL RECIPES - the art of scientific computing}. Cambridge, University Press, chap 5.6, p. 227-229.
}
%\author{
%Emanuel Huber
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
# cubic equation x^3 - 6x^2 + 11x - 6 = 0
# parameter
b <- c(1,-6, 11, -6)

# roots
x0 <- cubic(b)

# plot
x <- seq(0,4, by=0.001)
y <- b[1]*x^3 + b[2]*x^2 + b[3]*x + b[4]

# plot
plot(x,y,type="l")
abline(h=0,v=0)
points(cbind(x0,c(0,0,0)), pch=20,col="red",cex=1.8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
