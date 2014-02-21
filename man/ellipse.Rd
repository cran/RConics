\name{ellipse}
\alias{ellipse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return ellipse points
}
\description{
Return ellipse points. Usefull for ploting ellipses.
}
\usage{
ellipse(saxes = c(1, 1), loc = c(0, 0), theta = 0, n = 201, 
        method = c("default", "angle", "distance"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{saxes}{
    a \eqn{(2 \times 1)} vector of the length of the ellipse semi-axes.
}
  \item{loc}{
     a \eqn{(2 \times 1)} vector of the Cartesian coordinates of the ellipse center.
}
  \item{theta}{
   the angle of rotation of the elllipse (in radians).
}
  \item{n}{
    the number of points returned by the function.
}
  \item{method}{
    \code{"default"} returns points according to the polar equation;
    \code{"angle"} returns points radially equidistant;
    \code{"distance"} returns points that are equidistant on the ellipse arc.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A \eqn{(n \times 2)} matrix whose columns correspond to the Cartesian coordinates of the points lying on the ellipse.
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
# Ellipse parameters
saxes <- c(5,2)
loc <- c(0,0)
theta <- pi/4

# Plot
plot(ellipse(saxes, loc, theta, n=500),type="l")
points(ellipse(saxes, loc, theta, n=30),pch=20,col="red")
points(ellipse(saxes, loc, theta, n=30, method="angle"),pch=20,col="blue")
points(ellipse(saxes, loc, theta, n=30, method="distance"),pch=20,col="green")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
