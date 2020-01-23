\name{doubleplotter}
\alias{doubleplotter}
\title{
A comparison of two maps based on their empirical (null) distributions for a class-focused pattern metric
}
\description{
This function produces boxplots comparing the expected values (empirical, null distributions) for a given class-focused pattern metric between two maps.
}
\usage{
doubleplotter(data1 = data$result1, 
  data2 = data$result2,  
  metric = 5)
}
\arguments{
  \item{data1}{
This is the result object for the first map to be compared, where the output is returned by the function \code{singlemap}.
}
  \item{data2}{
This is the result object for the second map to be compared, where the output is returned by the function \code{singlemap}.
}
  \item{metric}{
This is an integer (1-110) indicating which metric the comparison boxplots will be drawn for.  A list of these metrics is given in the header of the function's source code.
}
}
\details{
No additional details at this time.
}
\value{
The output is a double boxplot drawn on the graphics device.
}
\references{
No references at this time.
}
\author{
Tarmo K. Remmel
}
\note{
No additional notes at this time.
}
\seealso{
See Also \code{\link{singlemap}}, \code{\link{singleplotter}}, and \code{\link{imaks}}.
}
\examples{
# EXAMPLE USES PREVIOUSLY PRODUCED RESULTS TO SPEED-UP THE EXAMPLE,
# BUT THE EXAMPLE FROM singlemap() SHOULD BE CALLED FIRST, ONCE FOR EACH
# MAP TO BE COMPARED (FOR EXAMPLE)
#result1 <- singlemap(IMG = data$demoimage1, VERBOSE = TRUE, reps = 5, LEVEL=6) 
#result2 <- singlemap(IMG = data$demoimage2, VERBOSE = TRUE, reps = 5, LEVEL=6)
doubleplotter(data1 = data$result1, data2 = data$result2, metric = 5) 
}
\keyword{ distribution }
