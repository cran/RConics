\name{Affine planar transformations matrix}
\alias{affine planar transformations matrix}
\alias{reflection}
\alias{rotation}
\alias{scaling}
\alias{translation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Affine planar transformation matrix
}
\description{
\eqn{(3 \times 3)} affine planar transformation matrix corresponding to reflection, rotation, scaling and translation in projective geometry. To transform a point \eqn{p} multiply the transformation matrix \eqn{A} with the homogeneous coordinates \eqn{(x,y,z)} of \eqn{p} (e.g. \eqn{p_{transformed} = Ap}).
}
\usage{
reflection(alpha)
rotation(theta, pt=NULL)
scaling(s)
translation(v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha}{
    the angle made by the line of reflection (in radian). 
  }
  \item{theta}{
    the angle of the rotation (in radian).
  }
  \item{pt}{
    the homogeneous coordinates of the rotation center (optional). 
  }
  \item{s}{
    the \eqn{(2 \times 1)} scaling vector in direction \eqn{x} and \eqn{y}. 
  }
  \item{v}{
    the \eqn{(2 \times 1)} translation vector in direction \eqn{x} and \eqn{y}. 
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%Form of the 3x3 affine transformation matrix: 
%}
\value{
A \eqn{(3 \times 3)} affine transformation matrix.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
p1 <- c(2,5,1)  # homogeneous coordinate

# rotation
r_p1 <- rotation(4.5) \%*\% p1

# rotation centered in (3,1)
rt_p1 <- rotation(4.5, pt=c(3,1,1)) \%*\% p1

# translation
t_p1 <- translation(c(2,-4)) \%*\% p1

# scaling
s_p1 <- scaling(c(-3,1)) \%*\% p1

# plot
plot(t(p1),xlab="x",ylab="y", xlim=c(-5,5),ylim=c(-5,5),asp=1)
abline(v=0,h=0, col="grey",lty=1)
abline(v=3,h=1, col="grey",lty=3)
points(3,1,pch=4)
points(t(r_p1),col="red",pch=20)
points(t(rt_p1),col="blue",pch=20)
points(t(t_p1),col="green",pch=20)
points(t(s_p1),col="black",pch=20)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
