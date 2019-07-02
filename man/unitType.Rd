% File src/library/grid/man/unit.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{unitType}
\alias{unitType}
\title{Get the unit names of a unit object}
\description{
  This function is used to query the types of units in a unit object.
}
\usage{
unitType(x)
}
\arguments{
  \item{x}{A unit object.}
}
\value{
  A character vector of unit types
}
\author{Thomas Lin Pedersen}
\seealso{
  \code{\link{unit}}
}

\examples{
u <- unit(1:5, c("cm", "mm", "in", "px", "null"))
unitType(u)
}
