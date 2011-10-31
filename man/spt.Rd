% This is a template R document file
% Distributed under GPL 3 or later

\name{spt}
\alias{spt}
\alias{F_PTChild} 
\alias{F_SPTChild2} 
\alias{F_SPTChild3} 
\alias{F_SptDimFP} 
\alias{F_stri}

\title{Sierpinski Pedal Triangle and Sierpinski Triangle}
\description{
 To draw Sierpinski pedal triangles (SPTs) and Sierpinski triangles (PTs).
}
\usage{
  spt(A,B,x0,y0,iter, spt=TRUE,chaosgame=FALSE,main=NULL,tol=0.0001,...)
}
\arguments{
  \item{A,B}{The degrees of two of the three angles of a triangle.}
  \item{x0,y0}{The initial position of the point if "chaosgame=TRUE".}
  \item{iter}{Iteration number in drawing the SPTs/PTs.}
  \item{spt}{SPT ("spt=TRUE") or PT ("spt=FALSE").}
  \item{chaosgame}{To draw the SPTs/PTs using regular method or via a chaos game. }
  \item{main}{Specify the title of the plot.}
  \item{tol}{A stopping creiteria to draw the sub-SPT/PT.  Default value 0.0001.}
  \item{...}{control }
}
\details{
  When the original triangle is an acute triangle, 
  the area of the smallest SPT/PT to be drawn is determined by (tol * S), 
  where S is the total area for plotting.  No restriction is applied to \code{iter}.

  If the original triangle is an obtuse triangle and \code{spt=TRUE},
  the largest value of \code{iter} is 12.

  If the SPTs/PTs are drawn via a chaos game, \code{iter=10000} by default.
}
\value{
  The dimension of the SPT will be returned if the original triangle 
  is an acute triangle.
}

\references{
  Zhang, XM., Hitt, R. Wang, B. and Ding, J. (2008). 
  Sierpinski Pedal Triangle. Fractals. 16(2): 141-150.
}

\author{
  B. Wang  \email{bwang@jaguar1.usouthal.edu}
}
\examples{

 spt(45,55,spt=FALSE)
 spt(45,55,spt=TRUE)
 spt(120,30,iter=15)
 spt(10,10,iter=10)

}
\keyword{stats}

