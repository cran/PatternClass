\name{imaks}
\alias{imaks}
\title{
Draws a matrix in the proper orientation, as it it were a raster landscape.
}
\description{
A simple drawing function for matrices that are actually representing raster landcape maps.  This function also controls the use of colour for nominal maps.  This is a generic function for drawing any raster image that should not be drawn as a matrix (where the origin is not at the lower-left corner, but rather the upper-left corner.  This function draws a matrix as a map using the proper positioning of the origin and gridded values.
}
\usage{
imaks(BE = data$demoimage1, numcol = NULL, LENG = 4, colour = FALSE)
}
\arguments{
  \item{BE}{
The input landscape map as a matrix object.
}
  \item{numcol}{
A numeric value indicating the number of total colours on the map.
}
  \item{LENG}{
An argument that is depreciating.  Do not adjust this value.
}
  \item{colour}{
Should a special colour scheme be applied (TRUE) or not (FALSE)
}
}
\details{
In a future release, this function may become obsolete as we migrate to using the raster library and raster objects.
}
\value{
The result is a graphic plot of a raster landscape as read from a matrix representation.
}
\references{
No references currently.
}
\author{
Sandor Kabos (modified by Tarmo K. Remmel)
}
\note{
This function was originally released in the package: hdeco
}
\seealso{
Currently none.
}
\examples{
data(demoimage1)
imaks(BE = data$demoimage1, numcol = NULL, LENG = 4, colour = FALSE) 
}
\keyword{ hplot }
