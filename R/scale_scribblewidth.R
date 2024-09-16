
#' Scribble linewidth scales
#'
#' These functions allow numeric or categorical variables to be passed to
#' the aesthetic mapping of a plot layer to change the width of the scribble
#' fill lines. These scales are directly analogous to the scale functions
#' in ggplot.
#'
#' @param ... Other arguments passed on to `continuous_scale()`, `binned_scale`,
#'   or `discrete_scale()` as appropriate, to control name, limits,
#'   breaks, labels and so forth.
#' @param range Output range of linewidth, defaults to `c(1, 6)`
#' @param na.value Missing values will be replaced with this value.
#' @param values a set of aesthetic values to map data values to. The values
#'   will be matched in order (usually alphabetical) with the limits of the
#'   scale, or with breaks if provided. If this is a named vector, then the
#'   values will be matched based on the names instead. Data values that
#'   don't match will be given na.value.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks
#'   - A numeric vector of positions
#'   - A function that takes the limits as input and returns breaks
#'     as output (e.g., a function returned by [scales::extended_breaks()]).
#'     Also accepts rlang [lambda][rlang::as_function()] function notation.
#' @return A `Scale` ggproto object that can be added to a plot.
#' @export
#' @examples
#' ggplot2::ggplot(iris,
#'                 ggplot2::aes(Species, scribblecolour = Species,
#'                              colour = Species, scribblewidth = Species)) +
#'  geom_scribblebar() +
#'  scale_scribblewidth_manual(values = c(1, 3, 5))

scale_scribblewidth <- function (name = ggplot2::waiver(),
                         breaks = waiver(), labels = waiver(),
                         limits = NULL, range = c(1, 6),
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
scale_scribblewidth_identity <- function(name = ggplot2::waiver(),
                                 ..., guide = "none") {

  ggplot2::continuous_scale(
    "scribblewidth", name = name,
    palette = scales::pal_identity(), ..., guide = guide,
    super = ggplot2::ScaleContinuousIdentity
  )
}

#' @rdname scale_scribblewidth
#' @export
scale_scribblewidth_manual <- function (..., values, breaks = ggplot2::waiver(),
                                na.value = NA) {

  manual_scale("scribblewidth", values, breaks, ..., na.value = na.value)
}

#' @rdname scale_scribblewidth
#' @export
scale_scribblewidth_ordinal <- function (name = ggplot2::waiver(), ...,
                                 range = c(1, 6)) {
  force(range)
  ggplot2::discrete_scale("scribblewidth", name = name,
                          palette = function(n) seq(range[1], range[2],
                                                    length.out = n),
                          ...)
}

#' @rdname scale_scribblewidth
#' @export
scale_scribblewidth_discrete <- function (...) {
  args <- rlang::list2(...)
  args$call <- if(is.null(args$call)) rlang::current_call() else args$call
  rlang::exec(scale_scribblewidth_ordinal, !!!args)
}
