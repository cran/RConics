#' RConics: Computations on conics
#' 
#' A package to solve some conic related problems 
#' (intersection of conics with lines and conics, arc length of an ellipse, 
#' polar lines, etc.). 
#'
#' Some of the functions are based on the \emph{projective} geometry. 
#' In projective geometry parallel lines meet at an infinite point and 
#' all infinite points are incident to a line at infinity. 
#' Points and lines of a projective plane are represented by \emph{homogeneous}
#'  coordinates, that means by 3D vectors: \eqn{(x, y, z)} for the points and 
#'  \eqn{(a, b, c)} such that \eqn{ax + by + c = 0} for the lines. 
#'  The Euclidian points correspond to \eqn{(x, y, 1)}, 
#'  the infinite points to \eqn{(x, y, 0)}, the Euclidean lines to 
#'  \eqn{(a, b, c)} with \eqn{a \neq 0} or \eqn{b \neq 0}, the line at 
#'  infinity to \eqn{(0, 0, 1)}.
#'  
#'  \strong{Advice}: to plot conics use the package \code{conics} 
#'  from Bernard Desgraupes.
#'  
#'  This work was funded by the Swiss National Science Foundation within the 
#'  ENSEMBLE project (grant no. CRSI_132249). 
#'
#' @import graphics
#' @import stats
#' @import utils 
#' @references 
#' Richter-Gebert, Jürgen (2011). 
#' \emph{Perspectives on Projective Geometry - A Guided Tour Through Real and Complex Geometry}, 
#' Springer, Berlin, ISBN: 978-3-642-17285-4
"_PACKAGE"
#> [1] "_PACKAGE"
