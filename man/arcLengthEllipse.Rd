\name{arcLengthEllipse}
\alias{arcLengthEllipse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Arc length of an ellipse
}
\description{
This function computes the arc length of an ellipse centered in \eqn{(0,0)} with the semi-axes aligned with the \eqn{x}- and \eqn{y}-axes. The arc length is defined by the points \eqn{1} and \eqn{2}. These two points do not need to lie exactly on the ellipse: the \eqn{x}-coordinate of the points and the quadrant where they lie define the positions on the ellipse used to compute the arc length.  
}
\usage{
arcLengthEllipse(p1, p2 = NULL, saxes, n = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p1}{
    a \eqn{(2 \times 1)} vector of the Cartesian coordinates of point \eqn{1}.
  }
  \item{p2}{
    a \eqn{(2 \times 1)} vector of the Cartesian coordinates of point \eqn{2} (optional).
  }
  \item{saxes}{
    a \eqn{(2 \times 1)} vector of length of the semi-axes of the ellipse.
  }
  \item{n}{
    the number of iterations used in the numerical approximation of the incomplete elliptic integral of the second kind.
  }
}
\details{
  If the coordinates \code{p2} of the point \eqn{2} are omitted the function \code{arcLengthEllipse} computes the arc length between the point \eqn{1} and the point defined by \eqn{(0,b)}, \eqn{b} beeing the minor semi-axis.
}
\value{
The length of the shortest arc of the ellipse defined by the points 1 and 2.
}
\references{
Van de Vel, H. (1969). \emph{On the series expansion method for Computing incomplete elliptic integrals of the first and second kinds}, Math. Comp. 23, 61-69.
}
%\author{
%Emanuel Huber 
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pEllipticInt}}
}
\examples{
p1 <- c(3,1)
p2 <- c(0,2)

# Ellipse with semi-axes: a = 5, b= 2
saxes <- c(5,2)

# 1 iteration
arcLengthEllipse(p1,p2,saxes,n=1)

# 5 iterations
arcLengthEllipse(p1,p2,saxes,n=5)

# 10 iterations
arcLengthEllipse(p1,p2,saxes,n=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
