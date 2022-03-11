# \name{Join, meet and parallel}
# \alias{meet}
# \alias{join}
# \alias{parallel}

# \usage{
#   join(p,q)
#   meet(l, m)
#   parallel(p,l)
# }

#' The join and meet of two points and the parallel
#'
#' The join operation of two points is the cross-product of these two points 
#' and represents the line passing through them. The meet operation of two lines 
#' is the cross-product of these two lines and represents their intersection. 
#' The line parallel to a line \eqn{l} and passing through the point \eqn{p} 
#' corresponds to the join of \eqn{p} with the meet of \eqn{l} and the line 
#' at infinity.
#' 
#' @param p \eqn{(3 \times 1)} vectors of the homogeneous coordinates of a point.
#' @param q \eqn{(3 \times 1)} vectors of the homogeneous coordinates of a point.
#' @param l \eqn{(3 \times 1)} vectors of the homogeneous representation of a line.
#' @param m \eqn{(3 \times 1)} vectors of the homogeneous representation of a line.
#' @return A \eqn{(3 \times 1)} vector of either the homogeneous coordinates of
#'  the meet of two lines (a point), the homogeneous representation of the 
#'  join of two points (line), or the homogeneous representation of the 
#'  parallel line. The vector has the form \eqn{(x,y,1)}.
#' @source  Richter-Gebert, Jürgen (2011). 
#' \emph{Perspectives on Projective Geometry - A Guided Tour Through Real
#'and Complex Geometry}, Springer, Berlin, ISBN: 978-3-642-17285-4
#' @examples 
#' p <- c(3,1,1)
#' q <- c(0,2,1)
#' l <- c(0.75,0.25,1)
#' 
#' # m is the line passin through p and q
#' m <- join(p,q)
#' 
#' # intersection point of m and l
#' ml <- meet(l,m)
#' 
#' # line parallel to l and through p
#' lp <- parallel(p,l)
#' 
#' # plot
#' plot(rbind(p,q),xlim=c(-5,5),ylim=c(-5,5))
#' abline(h=0,v=0,col="grey",lty=3)
#' addLine(l,col="red")
#' addLine(m,col="blue")
#' points(t(ml),cex=1.5,pch=20,col="blue")
#' addLine(lp,col="green")
#' @export
#' @name join
#' @rdname join

join <- function(p,q){
  if(!(is.vector(p, mode="numeric") && is.vector(q, mode="numeric")
       && length(p)==3 && length(q)==3)) 
    stop("p or q are either not a vector or have less than 3 elements\n")
  p <- as.numeric(p)
  q <- as.numeric(q)
  l <- c( p[2]*q[3] - p[3]*q[2],
          -p[1]*q[3] + p[3]*q[1],
          p[1]*q[2] - p[2]*q[1])
  l[abs(l)<.Machine$double.eps^0.75] <- 0
  if(l[3]!=0) l<-l/l[3]
  return(l)
}

#' @export
#' @name meet
#' @rdname join
meet <- function(l,m){
  if(!(is.vector(l, mode="numeric") && is.vector(m, mode="numeric")
       && length(l)==3 && length(m)==3)) 
    stop("l or m are either not a vector or have less than 3 elements\n")
  p <- c(  l[2]*m[3] - l[3]*m[2],
           -l[1]*m[3] + l[3]*m[1],
           l[1]*m[2] - l[2]*m[1])
  if(p[3]!=0) p <- p/p[3]
  return(p)
}

#' @export
#' @name parallel
#' @rdname join
parallel <- function(p,l){
  if(!(is.vector(p, mode="numeric") && is.vector(l, mode="numeric")
       && length(p)==3 && length(l)==3))
    stop("p or/and l is/are not a vector and/or have less than 3 elements\n")
  join(p,meet(l,c(0,0,1)))
  
}
