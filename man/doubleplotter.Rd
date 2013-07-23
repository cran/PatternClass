\name{doubleplotter}
\alias{doubleplotter}
\title{
A comparison of two maps based on their empirical (null) distributions for a class-focused pattern metric
}
\description{
This function produces boxplots comparing the expected values (empirical, null distributions) for a given class-focused pattern metric between two maps.
}
\usage{
doubleplotter(data1 = ClassPatternData$result1, data2 = ClassPatternData$result2, 
img1 = ClassPatternData$demoimage1, img2 = ClassPatternData$demoimage2, metric = 5)
}
\arguments{
  \item{data1}{
This is the result object for the first map to be compared, where the output is returned by the function \code{singlemap}.
}
  \item{data2}{
This is the result object for the second map to be compared, where the output is returned by the function \code{singlemap}.
}
  \item{img1}{
This is the binary matrix object that is the first map to be compared.
}
  \item{img2}{
This is the binary matrix object that is the second map to be compared.
}
  \item{metric}{
This is an integer indicating which metric the comparison boxplots will be drawn for.  A list of these metrics is given in the header of the function's source code.
}
}
\details{
No additional details at this time.
}
\value{
The output is a boxplot drawn on the graphics device.
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
result1 <- singlemap(IMG = ClassPatternData$demoimage1, VERBOSE = TRUE, reps = 10, LEVEL=6) 
result2 <- singlemap(IMG = ClassPatternData$demoimage2, VERBOSE = TRUE, reps = 10, LEVEL=6)
doubleplotter(data1 = result1, data2 = result2, img1 = ClassPatternData$demoimage1, 
img2 = ClassPatternData$demoimage2, metric = 5) 
}
\keyword{ distribution }
