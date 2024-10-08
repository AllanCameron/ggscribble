% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_scribblepath.R
\name{geom_scribblepath}
\alias{geom_scribblepath}
\title{Create a ggplot layer containing scribbled paths}
\usage{
geom_scribblepath(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = NULL,
  na.rm = FALSE,
  show.legend = NA,
  res = 200,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{lineend}{Line end style (round, butt, square).}

\item{linejoin}{Line join style (round, mitre, bevel).}

\item{linemitre}{Line mitre limit (number greater than 1).}

\item{arrow}{Arrow specification, as created by \code{\link[grid:arrow]{grid::arrow()}}.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
A `Layer` ggproto object that can be added to a plot.
}
\description{
Create a ggplot layer containing scribbled paths
}
\section{Aesthetics}{

\code{geom_scribblepath()} understands the following aesthetics (required aesthetics are in bold):
\itemize{
  \item \strong{\code{x}}
  \item \strong{\code{y}}
  \item \code{alpha}
  \item \code{colour}
  \item \code{group}
  \item \code{linetype}
  \item \code{linewidth}
  \item \code{wibbliness}
  \item \code{wonkiness}
}


Learn more about setting these aesthetics 
in \code{vignette("ggplot2-specs")}.
}

\examples{
t <- seq(5, -1, length.out = 1000) * pi

spiral <- data.frame(x = sin(t) * 1:1000, y = cos(t) * 1:1000)

ggplot2::ggplot(spiral, ggplot2::aes(x, y)) +
  geom_scribblepath(wonkiness = 1.5, wibbliness = 0.5) +
  ggplot2::coord_equal(xlim = c(-1500, 1500), ylim = c(-1500, 1500))
}
