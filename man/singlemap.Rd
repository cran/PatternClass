\name{singlemap}
\alias{singlemap}
\title{
Process a single binary map
}
\description{
This function estimates the composition and configuration parameters for a binary map having 64x64 pixel extent.  The 64x64 extent is a current limitation and will be expanded upon in subsequent updates.  The parameter estimates are corrected for bias introduced by the Whittle estimation and then these parameters are used to simulate a series of statistically similar landscapes.  Each of these simulated landscapes are processed with SDMTools to compute class-focused pattern metrics that are saved to produce empirical (null) distributions for each metric, given the specified level of spatial autocorrelation (configuration) and class proportion (composition). The results can be saved to a new object that is later used for producing graphical and statistical outputs.
}
\usage{
singlemap(IMG = data$demoimage1, 
  CORRECTIONMAT = data$DIFF50,
  VERBOSE = TRUE,
  reps = 1,
  LEVEL=6)
}
\arguments{
  \item{IMG}{
This is a numeric, binary matrix that represents a raster landscape.  Internally, for computing landscape metrics, this is handled as a raster class object, but input must be a two-valued matrix.  There is no implementation for the spatial resolution of each cell as the computed metrics do not require this value.  It assumes that the spatial resolution is consistent in both dimensions and across the entire scene.  The image is also assumed to be a graphic representation resulting from a stationary spatial process that is 64x64 pixels in extent.  The extent limitation will be expanded in a future release but for stationary and isotropic landscape patterns, this is deemed sufficient for now and could be used in a landscape sampling context.
}
  \item{CORRECTIONMAT}{
This is the name of a numeric matrix that provides the correction factors based on Whittle's estimation.  The defalut matrix DIFF50 is provided in the WhittleData environment that is supplied with this package.  However, if image sizes differ (i.e., not the 64x64 demos that are provided), this matrix needs to be recreated using \code{build.lut} and the result provided here.  The code, by default, will search for this name in the data environment unless you specifiy a custom matrix that you have created.
}
  \item{VERBOSE}{
This argument is set to TRUE if you desire extensive on-screen feedback as to what the program is doing.  If set to FALSE, much less information is written to screen.  It is useful to set to TRUE if \code{reps} is large, since processing times can be long; this gives an idea as to how much time is remaining to complete processing the data. 
}
  \item{reps}{
This argument controls the number of replicate landscapes that will be simulated and used to produce the empirical (null) distributions for the class-focused pattern metrics.  The default value is set at 1 to permit a quick check that the code is loaded and working correctly, but should be run with a value of 100-1000 or larger.
}
  \item{LEVEL}{
This is a measure of the image size being processed.  It is necessary for preparing the necessary arrays used for extimating rho.  The value must be an integer conforming to the image size, such that LEVEL = N, where N is 2^N x 2^N for the image dimensions.  Thus, for a 64x64 pixel map, LEVEL = 6.
}
}
\details{
When analyzing multiple maps, this function should be called independently for each map, with the results stored to unique objects that become input to either \code{singleplotter} or \code{doubleplotter}.
}
\value{
This returns an object containing the empirical (null) distributions for a suite of class-focused pattern metrics. The rows are observations for each replicate and the columns are the class metrics. Column names identify the actual metrics that are defined in the landscapemetrics package.
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
Implementation is currently for 64x64 binary landscapes, provided as arguments in integer matrix format. The size limitation will be relaxed in a future release when some of the internal coding structure is streamlined.  The result of this function call should be saved to an object that can subsequently be used in calls by either \code{singleplotter} of \code{doubleplotter}.
}
\seealso{
See Also \code{\link{singleplotter}}, and \code{\link{doubleplotter}}.
}
\examples{
result1 <- singlemap(IMG = data$demoimage1, CORRECTIONMAT = data$DIFF50, 
  VERBOSE = TRUE, reps = 1, LEVEL=6) 
}
\keyword{ distribution }
\keyword{ classes }
