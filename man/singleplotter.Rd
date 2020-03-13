\name{singleplotter}
\alias{singleplotter}
\title{
Depict the empirical (null) distribution for class-focused pattern metrics.
}
\description{
Produce a series of one or more boxplots depicting the empirical (null) distribuitons of class-focused pattern metrics, as computed for a single landscape map.  The code has been updated as of October 24 2016 to permit boxplots to be drawn even if landscape metrics have NA in them (those values are simply ignored).  As of 21 January 2020, the code now relies on landscapemetrics and raster packages rather than the diminished SDMTools.
}
\usage{
singleplotter(dat = data$result1, 
  img = data$demoimage1, 
  metrics = c(1, 5, 10), 
  rows = 1, 
  cols = 3, 
  addactual = TRUE, 
  colour = TRUE)
}
\arguments{
  \item{dat}{
This is the output object produced by \code{singlemap}.  It contains the empirical (null) distributions for class-focused pattern metrics computed for a single binary landscape map.
}
  \item{img}{
This is the binary integer matrix landscape map that corresponds to the \code{data} argument.
}
  \item{metrics}{
A vector listing the integers, corresponding to specific metrics, that you want to plot.  There are 55 unique class-level metrics computed for each of 2 classes; thus, the specific metric values in the specified vector can range from 1-110.
}
  \item{rows}{
This argument controls the number of rows (of plots) on the output graphics device.  The total number of graphs that can be displayed simultaneously will be (\code{rows} * \code{cols}).  These values should be assigned to correspond with the number of \code{metrics} that you want to view.  Trying to display too many will make them very small and not very informative.  It may be more effective to simply call this function multiple times with fewer metrics.
}
  \item{cols}{
This argument controls the number of columns (of plots) on the output graphics device. The total number of graphs that can be displayed simultaneously will be (\code{rows} * \code{cols}).  These values should be assigned to correspond with the number of \code{metrics} that you want to view.  Trying to display too many will make them very small and not very informative.  It may be more effective to simply call this function multiple times with fewer metrics.
}
  \item{addactual}{
If TRUE, then the landscape's actual class-focused pattern metric will be added to the plot to depict its relative position within the empirical (null) distribution.
}
  \item{colour}{
If TRUE, the actual landscape's class-focused pattern metric, if added, will be drawn in red.  If FALSE, the plot will be without colour (hollow circle). 
}
}
\details{
This function requires as input, both the original image object (\code{img}) and output object from \code{singlemap} (\code{data}) such that the proper metric values can be plotted along with their expectations (distributions).  This is the work-horse plotting function for metric values and distributions for any specific landscape object.  Currently, implementaiton is for 64x64 pixel subset landscape maps.  Future development will expand on this limiatation; however, our work has shown that this extent is sufficient for adequate pattern characterization and comparison purposes.
}
\value{
The output is graphical.
}
\references{
Remmel, T.K. and M.-J. Fortin. 2013. Categorical class map patterns: characterization and comparison. Landscape Ecology. DOI: 10.1007/s/10980-013-9905-x.

Remmel, T.K. and M.-J. Fortin. What constitutes a significant difference in landscape pattern? (using R). 2016. In Gergel, S.E. and M.G. Turner. Learning landscape ecology: concepts and techniques for a sustainable world (2nd ed.). New York: Springer. 
}
\author{
Tarmo K. Remmel
}
\note{
No further notes at this time.
}
\seealso{
See Also \code{\link{singlemap}}, and \code{\link{doubleplotter}}.
}
\examples{
# EXAMPLE USES PREVIOUSLY PRODUCED RESULTS TO SPEED-UP THE EXAMPLE,
# BUT THE EXAMPLE FROM singlemap() SHOULD BE CALLED FIRST
singleplotter(dat = data$result1, img = data$demoimage1, metrics = c(1, 5, 10))
}
\keyword{ distributions }
