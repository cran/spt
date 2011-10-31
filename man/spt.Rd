% This is a template R document file
% Distributed under GPL 3 or later

\name{spt}
\alias{spt}
\alias{plot.spt}
\alias{print.spt}
\title{Sierpinski Pedal Triangle}
\description{
 To initial, plot and show a Sierpinski pedal triangles.
}
\usage{
  spt(A,B)
}
\arguments{
  \item{A,B}{The degrees of two of the three angles of a triangle.}
}
\details{
  When the original triangle is an acute triangle, 
  the area of the smallest SPT/PT to be drawn is determined by (tol * S), 
  where S is the total area for plotting.  No restriction is applied to \code{iter}.

  If the original triangle is an obtuse triangle,
  the largest value of \code{iter} is 12.

  tol: A stopping creiteria to draw the sub-SPT.  Default value 0.0001.


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

(abc = spt(50,60))
plot(abc, iter=10)

}
\keyword{stats}

