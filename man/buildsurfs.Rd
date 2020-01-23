\name{buildsurfs}
\alias{buildsurfs}
\title{
Build an array of surfaces across compositional and configuration analysis space.
}
\description{
This function iteratively cycles through combinations of 9 binary proportion intervals and 11 spatial autocorrelation intervals and at each simulates a specified number of statistically similar binary landscape patterns. Each landscape is processed by landscapemetrics to produce empirical (null) distributions for class-focused pattern metrics. These surfaces can then be plotted or assessed for their mean values at any point (along with their variability). 
}
\usage{
buildsurfs(reps = 1000, verbose = TRUE)
}
\arguments{
  \item{reps}{
An argument that defines the number of simulated maps that are produced for each combination of composition and configuration across the parameter space. The default value, 1000, is appropriate, but will take a long time to compute. The example below uses 2 replicates to demonstrate the process; however, the result should be used only for demonstrating that the code is working. Using 1000 simulations ensures that a valid empirical (null) distribution is constructed at each point on this surface.
}
  \item{verbose}{
When this argument is set to TRUE, additional information is provided during processing on screen. When FALSE, the code runs in relatively quiet mode, with little information provided on screen. When set to TRUE, it is easier to assess the time required to complete the processing.
}
}
\details{
This is a simulation-intensive procedure and takes a long time.  It is only necessary for advanced users who wish to assess the infulence of composition and configuration (jointly) on specific pattern metrics.  Once this function is run and the surfaces object is created, all further analysis can be performed quickly.  This lookup reference set requires a lot of up-front processing, but simplifies all future analyses.  The produced reference set is also necessary for asking specific questions regarding the expectation and variability of pattern metrics within the context of composition and configuration values (i.e., distributions at all pair-wise combinations of compostion and configuration parameterizations).
}
\value{
The returned value is an array called surfaces having dimensions [metrics, proportions, configurations, reps].
}
\references{
Remmel, T.K. and M.-J. Fortin. What constitutes a significant difference in landscape pattern? (using R). 2016. In Gergel, S.E. and M.G. Turner. Learning landscape ecology: concepts and techniques for a sustainable world (2nd ed.). New York: Springer. 
}
\author{
Tarmo K. Remmel
}
\note{
The enclosed code is currently limited to 64x64 pixel subsets; however, future developments will expand the applicability to incorporate other dimensions. Since this code requires the simulation and storage of numerous landscape (raster) maps, the processing time (up-front) can be quite time consuming; however, once complete, plotting and analysis of mean and variance for any combination of composition and configuration and class-focused pattern metric are nearly instantaneous.
}
\seealso{
See Also \code{\link{singlemap}}, \code{\link{singleplotter}}, and \code{\link{doubleplotter}} 
}
\examples{
surfaces <- data$surfaces
# COMPUTE SURFACES
\donttest{surfaceexample <- buildsurfs(reps = 2, verbose = TRUE) }
# COMPUTE MEDIAN SURFACE FOR A SINGLE METRIC
tempmed <- apply(surfaces[9,,,], MARGIN=c(1,2), median)# PRODUCE A PERSPECTIVE PLOT OF MEDIAN VALUES RESULTS FOR THE SELECTED METRICpersp(tempmed, ticktype="detailed", cex.axis=0.7, zlab="Metric", 
ylab="Proportion", xlab="Rho", theta=-45)# COMPUTE VARIANCE SURFACE FOR A SINGLE METRICtempvar <- apply(surfaces[9,,,], MARGIN=c(1,2), var)
# PRODUCE A PERSPECTIVE PLOT OF VARIANCE VALUES RESULTS FOR THE SELECTED METRICpersp(tempvar, ticktype="detailed", cex.axis=0.7, zlab="Metric", 
ylab="Proportion", xlab="Rho", theta=-45)
}
\keyword{ distribution }
