\name{findcol}
\alias{findcol}
\title{
An internal utility that identifies that identifies the appropriate column of the Whittle correction matrix.
}
\description{
This is an internal utility function for identifying a column index value in the Whittle correction matrix and returns it to the calling function for use in further processing.  This function is not intended for use directly by users.
}
\usage{
findcol(prop = 0.32, DIFFMAT=data$DIFF50, VERBOSE=FALSE)
}
\arguments{
  \item{prop}{
This is a numeric argument providing the proportion of white pixels in the input binary map.
}
  \item{DIFFMAT}{
This is a matrix object containing the Whittle estimations (corrections) for spatial autocorrelation and generally constructed by \code{build.lut}.
}
  \item{VERBOSE}{
A Boolean argument that indicates whether the function should run in verbose mode or not.
}
}
\details{
This tool is only used internally.
}
\value{
The result is a an index to be used as a pointer.
}
\references{
No references currently.
}
\author{
Tarmo K. Remmel
}
\note{
This function is not to be used directly by users of this package.
}
\seealso{
Currently none.
}
\examples{
# No example.
}
\keyword{misc}
