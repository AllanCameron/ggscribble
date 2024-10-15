
#' Scribble density scales
#'
#' These functions allow numeric or categorical variables to be passed to
#' the aesthetic mapping of a plot layer to change the density of the scribble
#' fill lines. These scales are directly analogous to the scale functions
#' in ggplot.
#'
#' @inheritParams ggplot2::continuous_scale
#' @param range The output range of the line density
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
#'                  colour = Species, scribbledensity = Species)) +
#'  geom_scribblebar() +
#'  scale_scribbledensity_manual(values = c(45, 135, 10))

scale_scribbledensity <- function (name   = waiver(),
                                   breaks = waiver(),
                                   labels = waiver(),
                                   limits = NULL,
                                   range  = c(50, 400),
                                   guide  = "legend") {

  ggplot2::continuous_scale("scribbledensity",
                            palette = scales::pal_rescale(range),
                            name = name, breaks = breaks, labels = labels,
                            limits = limits, guide = guide)
}

#' @rdname scale_scribbledensity
#' @export
scale_scribbledensity_continuous <- scale_scribbledensity


#' @rdname scale_scribbledensity
#' @export
scale_scribbledensity_identity <- function(name = waiver(),
                                           ..., guide = "none") {
  ggplot2::continuous_scale(
    "scribbledensity", name = name,
    palette = scales::pal_identity(), ..., guide = guide,
    super = ggplot2::ScaleContinuousIdentity
  )
}

#' @rdname scale_scribbledensity
#' @export

scale_scribbledensity_manual <- function (..., values, breaks = waiver(),
                                          na.value = NA) {

  manual_scale("scribbledensity", values, breaks, ..., na.value = na.value)
}


#' @rdname scale_scribbledensity
#' @export

scale_scribbledensity_ordinal <- function (name = waiver(), ...,
                                           range = c(50, 400)) {
  force(range)
  ggplot2::discrete_scale("scribbledensity", name = name,
                          palette = function(n) seq(range[1], range[2],
                                                    length.out = n), ...)
}

#' @rdname scale_scribbledensity
#' @export

scale_scribbledensity_discrete <- function (...) {

  args <- list2(...)
  args$call <- if(is.null(args$call)) rlang::current_call() else args$call
  rlang::exec(scale_scribbledensity_ordinal, !!!args)
}
