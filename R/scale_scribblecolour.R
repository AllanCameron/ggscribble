
#' Scribble colour scales
#'
#' These functions allow numeric or categorical variables to be passed to
#' the aesthetic mapping of a plot layer to change the colour of the scribble
#' fill lines. These scales are directly analogous to the scale functions
#' in ggplot.
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams ggplot2::scale_colour_gradient
#' @inheritParams ggplot2::scale_colour_discrete
#' @inheritParams scales::hue_pal
#' @return A `Scale` ggproto object that can be added to a plot.
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(Species, scribblecolour = Species, colour = Species)) +
#'   geom_scribblebar() +
#'   scale_scribblecolour_manual(values = c("green4", "navy", "red3")) +
#'   scale_colour_manual(values = c("green4", "navy", "red3"))

scale_scribblecolour_gradient <- function (name = waiver(), ...,
    low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50",
    guide = "colourbar", aesthetics = "scribblecolour") {

    ggplot2::continuous_scale(aesthetics, name = name,
                        palette = scales::pal_seq_gradient(low, high, space),
                        na.value = na.value, guide = guide, ...)
}

#' @rdname scale_scribblecolour_gradient
#' @export
scale_scribblecolour_continuous <- scale_scribblecolour_gradient


#' @rdname scale_scribblecolour_gradient
#' @export
scale_scribblecolour_identity <- function (name = waiver(), ...,
                                           guide = "none",
                                           aesthetics = "scribblecolour") {

    ggplot2::discrete_scale(aesthetics, name = name,
                   palette = scales::pal_identity(),
                   ..., guide = guide, super = ggplot2::ScaleDiscreteIdentity)
}

#' @rdname scale_scribblecolour_gradient
#' @export
scale_scribblecolour_manual <- function (...,
                                         values,
                                         aesthetics = "scribblecolour",
                                         breaks = ggplot2::waiver(),
                                         na.value = "grey50") {

  manual_scale(aesthetics, values, breaks, ..., na.value = na.value)
}

#' @rdname scale_scribblecolour_gradient
#' @export
scale_scribblecolour_discrete <- function (name = waiver(), ...,
    h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1,
    na.value = "grey50", aesthetics = "scribblecolour") {

    ggplot2::discrete_scale(aesthetics, name = name,
              palette = scales::pal_hue(h, c, l, h.start, direction),
              na.value = na.value, ...)
}
