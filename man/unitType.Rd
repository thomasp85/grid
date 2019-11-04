<<<<<<< HEAD
% File src/library/grid/man/unit.Rd
=======
% File src/library/grid/man/unitType.Rd
>>>>>>> master
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{unitType}
\alias{unitType}
<<<<<<< HEAD
\title{Get the unit names of a unit object}
\description{
  This function is used to query the types of units in a unit object.
=======
\title{Return the Units of a Unit Object}
\description{
  This function returns the units of a unit object.
>>>>>>> master
}
\usage{
unitType(x)
}
\arguments{
  \item{x}{A unit object.}
}
\value{
<<<<<<< HEAD
  A character vector of unit types
}
\author{Thomas Lin Pedersen}
=======
  For simple units, this will be just a vector of coordinate systems,
  like \code{"inches"} or \code{"npc"}.

  More complex units that involve
  an operation on units return
  an operator, like \code{"sum"}, \code{"min"}, or \code{"max"}.
}
\author{Paul Murrell}
>>>>>>> master
\seealso{
  \code{\link{unit}}
}

\examples{
<<<<<<< HEAD
u <- unit(1:5, c("cm", "mm", "in", "px", "null"))
unitType(u)
}
=======
unitType(unit(1, "npc"))
unitType(unit(1:3/4, "npc"))
unitType(unit(1:3/4, "npc") + unit(1, "inches"))
unitType(min(unit(0.5, "npc"), unit(1, "inches")))
unitType(unit.c(unit(0.5, "npc"), unit(2, "inches") + unit(1:3/4, "npc"),
                unit(1, "strwidth", "hi there")))
}
\keyword{dplot}
>>>>>>> master
