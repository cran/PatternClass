\name{wtest.run}
\alias{wtest.run}
\title{
Whittle estimation for binary map
}
\description{
This function is the workhorse for estimating the Whittle correction for a binary map. This function is generally called by a wrapping function to facilitate its use (e.g., \code{singlemap}).}
\usage{
wtest.run(LEVEL=6, REPSIM=20, RHO=0.2499999, CPROP=0.5, RAJZ=T, CIM="CIM", ENV="data")
}
\arguments{
  \item{LEVEL}{
This is a numeric, binary matrix that represents a raster landscape.  In future updates, this argumnent may migrate to be a raster object, but for now, it must be a two-valued matrix.  There is no implementation for the spatial resolution of each cell as the computed metrics do not require this value.  It assumes that the spatial resolution is consistent in both dimensions and across the entire scene.  The image is also assumed to be a graphic representation resulting from a stationary spatial process.
}
  \item{REPSIM}{
This is a numeric matrix that provides the correction factors based on Whittle's estimation. The defalut matrix DIFF50 is provided in the data environment that is supplied with this package. However, if image sizes differ (i.e., not the 64x64 demos that are provided), this matrix needs to be recreated using \code{build.lut} and the result provided here.
}
  \item{RHO}{
This is a numeric argument recording the spatial autocorrelation parameter. Note that this is divided into 4 and thus 0.2499999 approaches the theoretical maximum value of 1. 
}
  \item{CPROP}{
This argument controls the proportion of white to black pixels.
}
  \item{RAJZ}{
This Boolean argument controls whether outputs of the simulation are drawn on the screen.
}
  \item{CIM}{
A parameter that can control the naming of outputs.
}
  \item{ENV}{
A parameter that can controls the name of the environment where temporary objects are stored.
}
}
\details{
This code is generally not called directly by users.
}
\value{
Not for users.
}
\references{
Remmel, T.K. and F. Csillag. 2003. When are two landscape pattern indices significantly different? Journal of Geographical Systems 5(4):331-351

Remmel, T.K. and M.-J. Fortin. 2013. Categorical class map patterns: characterization and comparison. Landscape Ecology. DOI: 10.1007/s/10980-013-9905-x.

Remmel, T.K. and M.-J. Fortin. What constitutes a significant difference in landscape pattern? (using R). 2016. In Gergel, S.E. and M.G. Turner. Learning landscape ecology: concepts and techniques for a sustainable world (2nd ed.). New York: Springer. 
}
\author{
Tarmo K. Remmel
}
\note{
This is for internal use. Use \code{singlemap} instead.
}
\seealso{
See Also \code{\link{singlemap}}.
}
\examples{
# No example.
}
\keyword{ misc }
