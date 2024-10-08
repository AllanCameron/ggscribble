% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_scribblewidth.R
\name{scale_scribblewidth}
\alias{scale_scribblewidth}
\alias{scale_scribblewidth_continuous}
\alias{scale_scribblewidth_identity}
\alias{scale_scribblewidth_manual}
\alias{scale_scribblewidth_ordinal}
\alias{scale_scribblewidth_discrete}
\title{Scribble linewidth scales}
\usage{
scale_scribblewidth(
  name = ggplot2::waiver(),
  breaks = ggplot2::waiver(),
  labels = ggplot2::waiver(),
  limits = NULL,
  range = c(1, 6),
  guide = "legend"
)

scale_scribblewidth_continuous(
  name = ggplot2::waiver(),
  breaks = ggplot2::waiver(),
  labels = ggplot2::waiver(),
  limits = NULL,
  range = c(1, 6),
  guide = "legend"
)

scale_scribblewidth_identity(name = ggplot2::waiver(), ..., guide = "none")

scale_scribblewidth_manual(
  ...,
  values,
  breaks = ggplot2::waiver(),
  na.value = NA
)

scale_scribblewidth_ordinal(name = ggplot2::waiver(), ..., range = c(1, 6))

scale_scribblewidth_discrete(...)
}
\arguments{
\item{breaks}{One of:
- `NULL` for no breaks
- `waiver()` for the default breaks
- A numeric vector of positions
- A function that takes the limits as input and returns breaks
  as output (e.g., a function returned by [scales::extended_breaks()]).
  Also accepts rlang [lambda][rlang::as_function()] function notation.}

\item{range}{Output range of linewidth, defaults to `c(1, 6)`}

\item{...}{Other arguments passed on to `continuous_scale()`, `binned_scale`,
or `discrete_scale()` as appropriate, to control name, limits,
breaks, labels and so forth.}

\item{values}{a set of aesthetic values to map data values to. The values
will be matched in order (usually alphabetical) with the limits of the
scale, or with breaks if provided. If this is a named vector, then the
values will be matched based on the names instead. Data values that
don't match will be given na.value.}

\item{na.value}{Missing values will be replaced with this value.}
}
\value{
A `Scale` ggproto object that can be added to a plot.
}
\description{
These functions allow numeric or categorical variables to be passed to
the aesthetic mapping of a plot layer to change the width of the scribble
fill lines. These scales are directly analogous to the scale functions
in ggplot.
}
\examples{
ggplot2::ggplot(iris,
                ggplot2::aes(Species, scribblecolour = Species,
                             colour = Species, scribblewidth = Species)) +
 geom_scribblebar() +
 scale_scribblewidth_manual(values = c(1, 3, 5))
}
