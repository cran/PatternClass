\name{PatternClass-package}
\alias{PatternClass-package}
\alias{PatternClass}
\docType{package}
\title{
Class-focused pattern metric comparisons
}
\description{
Permits the estimation of composition and configuration parameters from a binary landscape map (provided as a numeric matrix).  These values are then used to simulate a series of statistically similar maps from which a suite of class-focused pattern metrics are computed and stored to produce empirical (null) distributions for each pattern metric.  The package also produces probabilities and allows the testing of whether metric values measured for a map could have been produced by random chance.  Numerous graphic outputs are also produced to facilitate the analysis and comparison of spatial patterns at the class-focus.
}
\details{
\tabular{ll}{
Package: \tab PatternClass\cr
Type: \tab Package\cr
Version: \tab 2.0.0\cr
Date: \tab 2020-01-21\cr
License: \tab WGPS >= 3.0\cr
}
singlemap, singleplotter, and doubleplotter are the workhorses of this package.
}
\author{
Tarmo K. Remmel
Maintainer: Tarmo K. Remmel <remmelt@yorku.ca>
}
\references{
Remmel, T.K. and M.-J. Fortin. 2013. Categorical class map patterns: characterization and comparison. Landscape Ecology. DOI: 10.1007/s/10980-013-9905-x.

Remmel, T.K. and M.-J. Fortin. What constitutes a significant difference in landscape pattern? (using R). 2016. In Gergel, S.E. and M.G. Turner. Learning landscape ecology: concepts and techniques for a sustainable world (2nd ed.). New York: Springer. http://sarahgergel.net/lel/learning-landscape-ecology/
}
\keyword{ package }
\seealso{
It would be prudent to also observe the landscapemetrics package.
}
\examples{
# Specific examples are provided on the individual function manual pages.
# The order of function calls should be: singlemap(), then singleplotter()
# or
# singlemap(), singlemap(), doubleplotter()
}
