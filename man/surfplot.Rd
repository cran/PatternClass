\name{surfplot}
\alias{surfplot}
\title{
Produce graphic plots providing class-focused pattern metric context for a landcape map 
}
\description{
This function produces three separate plots on a 1 row by 3 column output view.  The first plot is the expected mean surface for a selected class-focused pattern metric across the possible range of composition and configuration.  A dropped marker can be added to the plot that depicts the position of any combination of composition or configuration parameterization.  The second and thrid plots provide series of boxplots representing orthogonal cross-sections of the surface in the first plot, intersecting at the specified location.  This provides a sense of how the selected metric's expected value and variability will change as the composition and/or configuration parameter are altered.
}
\usage{
surfplot(metric = 9, prop = 0.7, rho = 0.2, colour = TRUE, drop = TRUE,
cross = TRUE, dat=data$surfaces)
}
\arguments{
  \item{metric}{
An integer identifying the class-focused pattern metric that the plots will represent.  These integers are listed in the source code for \code{singleplotter}.
}
  \item{prop}{
A real value between zero (0) and one (1) that defines the proportion of the focal land cover category.  This value along with \code{rho} define where the marker will be drawn on the first plot and through which the orthogonal cross-sections will be drawn.
}
  \item{rho}{
A real value between zero (0) and one (1) that defines the level of spatial autocorrelation for the focal land cover category.  This value along with \code{prop} define where the marker will be drawn on the first plot and through which the orthogonal cross-sections will be drawn.
}
  \item{colour}{
A Boolean indication as to whether the plot should contain colour (TRUE) or not (FALSE).
}
  \item{drop}{
A Boolean indication as to whether the specified point based on composition (\code{prop}) and configuration (\code{rho}) should be added to the plot (TRUE) or not (FALSE)
}
  \item{cross}{
A Boolean indication as to whether the cross-sectional boxplots should be drawn (TRUE) or not (FALSE).
}
  \item{dat}{
A numeric array object created by \code{buildsurfs} that has dimensions [38,9,11,replicates].  The version provided is produced with only 5 replicates to save space; it is highly recommended to produce a \code{dat} object, such as \code{surfaces} that is produced with 1000 replicates.
}
}
\details{
This function requires a valid result from the function \code{buildsurfs} to exist.  That result is an array of simulated maps for a series of replicates produced for all pair-wise paramterizations of composition and configuration.  This object can be quite large; however, needs to only be produced once.  All polts thereafter can be produced (for any class-focused metric) from this stored object.  To save processing time, the PatternClass package is provided with these reference (lookup) surfaces already produced for 64x64 images.  Future developments will permit these layers to be produced at a wider range of extents.
}
\value{
This function returns a graphic plot.
}
\references{
Remmel, T.K. and M.-J. Fortin. What constitutes a significant difference in landscape pattern? (using R). 2016. In Gergel, S.E. and M.G. Turner. Learning landscape ecology: concepts and techniques for a sustainable world (2nd ed.). New York: Springer. 
}
\author{
Tarmo K. Remmel
}
\note{
No further notes at this time.
}
\seealso{
See Also \code{\link{singlemap}}, \code{\link{singleplotter}}, \code{\link{buildsurfs}}, and \code{\link{doubleplotter}}. 
}
\examples{
surfplot(metric = 9, prop = 0.7, rho = 0.2, colour = TRUE, drop = TRUE,
cross = TRUE, dat=data$surfaces)
}
\keyword{ distributions }
