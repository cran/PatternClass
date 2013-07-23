\name{ClassPatternData}
\alias{ClassPatternData}
\docType{data}
\title{
A data environment for this package.
}
\description{
Specific data objects and temporary objects are written to this environment.
}
\usage{data(ClassPatternData)}
\format{
  The format is:
<environment: 0x0000000005686de0> 
}
\details{
\code{demoimage1}{
This is a simulated binary landscape (64x64 pixels), having 40% black (1) and 60% white pixels (2).  The spatial autocorrelation is 0, or random.  This is used for demonstrating the use of the functions and is one of the first two maps used in Remmel and Fortin (Pending 2014).
}
\code{demoimage2}{
This is a simulated binary landscape (64x64 pixels), having 40% black (1) and 60% white pixels (2).  The spatial autocorrelation is ~1, or highly spatially autocorrelated.  This is used for demonstrating the use of the functions and is one of the first two maps used in Remmel and Fortin (Pending 2014).
}
\code{DIFF50}{
This is a lookup table that has been generated in advance and provided with the package.  It provides the bias correction values for estimating spatial autocorrelation from a categorical map using the Whittle method.  The lookup table is a matrix of 10 rows (configuration) and 9 columns (composition).  Each bias correction value is based on the mean of 50 replicates.  This matrix can be rebuilt using more replicates and for data sets with extents different from 64x64 cells by calling the \code{build.lut} function.  Warning: rebuilding this lookup tables takes a lot of time.
}
\code{surfaces}{
This is a pre-constructed array containing the 38 class-focused pattern metrics for one colour computed for 1000 replicates of each combination of 9 composition and 11 configuration parameterizations of simulated 64x64 cell binary maps.  This array is used by the \code{surfplot} function to produce numerous permutations of summary plots that facilitate understanding and comparison of metrics.  However, the production of this object takes considerable time using \code{buildsurfs}; thus it is provided here.  Note: if plots, summaries, and comparisons are desired for maps having a different extent, then this array needs to be rebuilt.  However, our work shows that this size of landscape is reasonable for this type of analysis.
}
\code{result1}{
This is the result of running \code{singlemap} on \code{demoimage1} and is used to demonstrate plotting output.  Users will produce their own versions of this object when processing their own images.
}
\code{result2}{
This is the result of running \code{singlemap} on \code{demoimage2} and is used to demonstrate plotting output.  Users will produce their own versions of this object when processing their own images.
}
\code{WIBI}{
This is a temporary object shared among functions and does not need to be accessed by users.
}
\code{WITRUNC}{
This is a temporary object shared among functions and does not need to be accessed by users.
}
}
\source{
The data are all simulated using functions available in the \code{PatternClass} package.
}
\references{
Remmel, T.K. and M.-J. Fortin. 2013. Categorical class map patterns: characterization and comparison. Landscape Ecology. DOI: 10.1007/s/10980-013-9905-x.

Remmel, T.K. and M.-J. Fortin. What constitutes a significant difference in landscape pattern? (using R) (Pending 2014). In Gergel, S.E. and M.G. Turner. Learning landscape ecology: concepts and techniques for a sustainable world (2nd ed.). New York: Springer. 
}
\examples{
data(ClassPatternData)
str(ClassPatternData)
}
\keyword{datasets}
