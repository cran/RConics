# \name{addLine}
# \alias{addLine}
# %- Also NEED an '\alias' for EACH other topic documented here.
# \title{
  
#' Plot a "homogeneous" line to a plot.
#' 
#' Add a homogeneous line to a plot. The line parameters must be in homogeneous coordinates, e.g. \eqn{(a,b,c)}.
#' @param l A \eqn{3 \times 1} vector of the homogeneous representation of a line.
#' @param \dots  \link{graphical parameters} such as \code{col}, \code{lty} and  \code{lwd}.
#' @examples
#' # two points in homogeneous coordinates
#' p1 <- c(3,1,1)
#' p2 <- c(0,2,1)
#' 
#' # homogeneous line joining p1 and p2
#' l_12 <- join(p1,p2)
#' l_12
#' 
#' # plot
#' plot(0,0,type="n", xlim=c(-2,5),ylim=c(-2,5),asp=1)
#' points(t(p1))
#' points(t(p2))
#' addLine(l_12,col="red",lwd=2)
#' @export
#' @name addLine
#' @rdname plotHLine

addLine <- function(l,...){
  if(!is.numeric(l) && length(l)<3) stop("arg l is either not numeric or has less than 3 elements")
  if(l[1]==0) abline(h=-l[3]/l[2],...)  	# cas 1. y = y0
  if(l[2]==0) abline(v=-l[3]/l[1],...)		# cas 2. x = x0
  if(l[1]!=0 && l[2]!=0) abline(a=-l[3]/l[2],b=-l[1]/l[2],...)	# cas general
}

#' @export
#' @name plotHLine
#' @rdname plotHLine
plotHLine <- function(l,...){
  if(!is.numeric(l) && length(l)<3) stop("arg l is either not numeric or has less than 3 elements")
  if(l[1]==0) abline(h=-l[3]/l[2],...)  	# cas 1. y = y0
  if(l[2]==0) abline(v=-l[3]/l[1],...)		# cas 2. x = x0
  if(l[1]!=0 && l[2]!=0) abline(a=-l[3]/l[2],b=-l[1]/l[2],...)	# cas general
}
