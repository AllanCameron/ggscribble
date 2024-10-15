
#' Scribble linewidth scales
#'
#' These functions allow numeric or categorical variables to be passed to
#' the aesthetic mapping of a plot layer to change the width of the scribble
#' fill lines. These scales are directly analogous to the scale functions
#' in ggplot.
#'
#' @inheritParams ggplot2::continuous_scale
#' @param range The output range of the scribbled linewidth
#' @param values a set of aesthetic values to map data values to. The values
#' will be matched in order (usually alphabetical) with the limits of the scale,
#' or with breaks if provided. If this is a named vector, then the values will
#' be matched based on the names instead. Data values that don't match will be
#' given na.value
#' @param ... Other arguments to be passed to the relevant scale generator
#' @return A `Scale` ggproto object that can be added to a plot.
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(Species, scribblecolour = Species,
#'        colour = Species, scribblewidth = Species)) +
#'  geom_scribblebar() +
#'  scale_scribblewidth_manual(values = c(1, 3, 5))

scale_scribblewidth <- function (name = waiver(),
                                 breaks = waiver(),
                                 labels = waiver(),
                                 limits = NULL,
                                 range = c(1, 6),
                                 guide = "legend") {

  ggplot2::continuous_scale("scribblewidth",
                            palette = scales::pal_rescale(range),
                            name = name, breaks = breaks, labels = labels,
                            limits = limits, guide = guide)
}


#' @rdname scale_scribblewidth
#' @export

scale_scribblewidth_continuous <- scale_scribblewidth


#' @rdname scale_scribblewidth
#' @export

scale_scribblewidth_identity <- function(name = waiver(),
                                 ..., guide = "none") {

  ggplot2::continuous_scale(
    "scribblewidth", name = name,
    palette = scales::pal_identity(), ..., guide = guide,
    super = ggplot2::ScaleContinuousIdentity
  )
}


#' @rdname scale_scribblewidth
#' @export

scale_scribblewidth_manual <- function (..., values, breaks = waiver(),
                                na.value = NA) {

  manual_scale("scribblewidth", values, breaks, ..., na.value = na.value)
}


#' @rdname scale_scribblewidth
#' @export

scale_scribblewidth_ordinal <- function(name = waiver(), ..., range = c(1, 6)) {

  force(range)
  ggplot2::discrete_scale("scribblewidth", name = name,
                          palette = function(n) seq(range[1], range[2],
                                                    length.out = n), ...)
}


#' @rdname scale_scribblewidth
#' @export

scale_scribblewidth_discrete <- function (...) {
  args <- list2(...)
  args$call <- if(is.null(args$call)) rlang::current_call() else args$call
  rlang::exec(scale_scribblewidth_ordinal, !!!args)
}
