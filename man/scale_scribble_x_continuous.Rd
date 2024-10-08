% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/axis_scales.R
\name{scale_scribble_x_continuous}
\alias{scale_scribble_x_continuous}
\alias{scale_scribble_x_binned}
\alias{scale_scribble_x_date}
\alias{scale_scribble_x_datetime}
\alias{scale_scribble_x_discrete}
\alias{scale_scribble_x_log10}
\alias{scale_scribble_x_reverse}
\alias{scale_scribble_x_sqrt}
\alias{scale_scribble_x_time}
\alias{scale_scribble_y_binned}
\alias{scale_scribble_y_continuous}
\alias{scale_scribble_y_date}
\alias{scale_scribble_y_datetime}
\alias{scale_scribble_y_discrete}
\alias{scale_scribble_y_log10}
\alias{scale_scribble_y_reverse}
\alias{scale_scribble_y_sqrt}
\alias{scale_scribble_y_time}
\title{Scribbled axis scales}
\usage{
scale_scribble_x_continuous(..., wibbliness = 1, res = 200)

scale_scribble_x_binned(..., wibbliness = 1, res = 200)

scale_scribble_x_date(..., wibbliness = 1, res = 200)

scale_scribble_x_datetime(..., wibbliness = 1, res = 200)

scale_scribble_x_discrete(..., wibbliness = 1, res = 200)

scale_scribble_x_log10(..., wibbliness = 1, res = 200)

scale_scribble_x_reverse(..., wibbliness = 1, res = 200)

scale_scribble_x_sqrt(..., wibbliness = 1, res = 200)

scale_scribble_x_time(..., wibbliness = 1, res = 200)

scale_scribble_y_binned(..., wibbliness = 1, res = 200)

scale_scribble_y_continuous(..., wibbliness = 1, res = 200)

scale_scribble_y_date(..., wibbliness = 1, res = 200)

scale_scribble_y_datetime(..., wibbliness = 1, res = 200)

scale_scribble_y_discrete(..., wibbliness = 1, res = 200)

scale_scribble_y_log10(..., wibbliness = 1, res = 200)

scale_scribble_y_reverse(..., wibbliness = 1, res = 200)

scale_scribble_y_sqrt(..., wibbliness = 1, res = 200)

scale_scribble_y_time(..., wibbliness = 1, res = 200)
}
\arguments{
\item{...}{Arguments passed to ggplot's `scale_x_` or `scale_y_` functions}

\item{wibbliness}{Controls how much the axis line deviates from straight.
A value of 0 is perfectly straight, and the default of 1 is set to look
carefully hand-drawn.}

\item{res}{This controls the resolution of the noise used to wibble the axis
line. Essentially it controls the frequency of the wibble. The default of
200 emulates a fairly steady hand. Higher values will look shakier and lower
values give smoother deviations.}
}
\value{
A ggplot scale object which can be added to a plot
}
\description{
The wobbly axis lines in ggscribble are actually achieved via ggplot's guide
system rather than via its scales, but since specifying guides requires
more coding and may not be familiar to less advanced users, there are
scribble versions of all the axis scales included as convenience functions.
These essentially just use the `wibbliness` and `res` parameters to generate
the correct guide, which is then passed on to the equivalent ggplot scale.
}
\examples{
ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Width)) +
  geom_scribbleboxplot(ggplot2::aes(scribblecolour = Species),
                       staplewidth = 0.4) +
  ggplot2::theme_classic(16) +
  scale_scribble_y_continuous() +
  scale_scribble_x_discrete()
}
