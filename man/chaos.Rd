% This is a template R document file
% Distributed under GPL 3 or later

\name{chaos}
\alias{chaos}
\alias{chaos.st}
\alias{chaos.spt}
\alias{chaos.default}

\title{Chaos Games for Sierpinski (Pedal) Triangle}
\description{
 To construct SPT/ST via Chaos games.
}
\usage{
  chaos(abc,...)
}
\arguments{
  \item{abc}{An R object of class 'st' or 'spt'.}
  \item{...}{Controls. }
}
\details{
  If 'abc' is an acute triangle or obtuse 'st' triangle, the algorithms works.  For obtuse 'spt' triangle, we need think of something else to measure the dimention.
}
\value{
  Iteration number should be large (say 10000).
}

\references{
  Zhang, XM., Hitt, R. Wang, B. and Ding, J. (2008). 
  Sierpinski Pedal Triangle. Fractals. 16(2): 141-150.
}

\author{
  B. Wang  \email{bwang@jaguar1.usouthal.edu}
}
\examples{

(abc1 = st(50,60))
chaos(abc1, iter=2000)
(abc2 = spt(50,60))
chaos(abc2,iter=1000)

}
\keyword{stats}

