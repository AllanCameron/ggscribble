% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_scribbleabline.R, R/geom_scribblehline.R,
%   R/geom_scribblevline.R
\name{Scribbled-reference-lines}
\alias{Scribbled-reference-lines}
\alias{geom_scribbleabline}
\alias{geom_scribblehline}
\alias{geom_scribblevline}
\title{Scribbled reference lines: horizontal, vertical, and diagonal}
\usage{
geom_scribbleabline(
  mapping = NULL,
  data = NULL,
  ...,
  slope,
  intercept,
  na.rm = FALSE,
  show.legend = NA,
  res = 100
)

geom_scribblehline(
  mapping = NULL,
  data = NULL,
  ...,
  yintercept,
  na.rm = FALSE,
  res = 100,
  show.legend = NA
)

geom_scribblevline(
  mapping = NULL,
  data = NULL,
  ...,
  xintercept,
  na.rm = FALSE,
  show.legend = NA,
  res = 100
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by
\link[ggplot2]{aes}.}

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

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{res}{The number of points into which the line will be "wibbled"}

\item{xintercept, yintercept, slope, intercept}{Parameters that control the
position of the line. If these are set, `data`, `mapping` and
`show.legend` are overridden.}
}
\description{
These geoms add reference lines (sometimes called rules) to a plot, either
horizontal, vertical, or diagonal (specified by slope and intercept).
These are useful for annotating plots.
}
\details{
These geoms act slightly differently from other geoms. You can supply the
parameters in two ways: either as arguments to the layer function,
or via aesthetics. If you use arguments, e.g.
`geom_abline(intercept = 0, slope = 1)`, then behind the scenes
the geom makes a new data frame containing just the data you've supplied.
That means that the lines will be the same in all facets; if you want them
to vary across facets, construct the data frame yourself and use aesthetics.

Unlike most other geoms, these geoms do not inherit aesthetics from the plot
default, because they do not understand x and y aesthetics which are
commonly set in the plot. They also do not affect the x and y scales.
}
\section{Aesthetics}{

These geoms are drawn using \code{\link{geom_scribbleline}} so they support
the same aesthetics: \code{alpha}, \code{colour}, \code{linetype} and
\code{linewidth}. They also each have aesthetics that control the position of
the line:

  - \code{geom_scribblevline()}: \code{xintercept}

  - \code{geom_scribblehline()}: \code{yintercept}

  - \code{geom_scribbleabline()}: \code{slope} and \code{intercept}
}

\examples{
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()

# Fixed values
p + geom_scribblevline(xintercept = 5)
p + geom_scribblevline(xintercept = 1:5)
p + geom_scribblehline(yintercept = 20)

p + geom_scribbleabline() # Can't see it - outside the range of the data
p + geom_scribbleabline(intercept = 20)

# Calculate slope and intercept of line of best fit
coef(lm(mpg ~ wt, data = mtcars))
p + geom_scribbleabline(intercept = 37, slope = -5)

# To show different lines in different facets, use aesthetics
p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ cyl)

mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
p + geom_scribblehline(ggplot2::aes(yintercept = wt), mean_wt)

# You can also control other aesthetics
ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt, colour = wt)) +
  ggplot2::geom_point() +
  geom_scribblehline(ggplot2::aes(yintercept = wt, colour = wt), mean_wt) +
  ggplot2::facet_wrap(~ cyl)
}
\seealso{
See \code{\link{geom_scribblesegment}} for a more general approach
to adding scribbled line segments to a plot.
}
