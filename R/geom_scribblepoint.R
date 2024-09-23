#' Create a ggplot layer containing scribbled points
#'
#' @inheritParams ggplot2::geom_point
#' @eval rd_aesthetics("geom", "scribblepoint")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(Species, Petal.Length, colour = Species)) +
#'   geom_scribblepoint(position = ggplot2::position_jitter(0.5))
geom_scribblepoint <- function (mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", ..., na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblepoint, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(na.rm = na.rm, ...))
}

#' The ggproto object that powers scribbled points
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export

GeomScribblepoint <- ggplot2::ggproto("GeomScribblepoint",

  ggplot2::GeomPoint,

  default_aes = ggplot2::aes(colour = "black", alpha = NA, size = 1.5),

  draw_panel = function (self, data, panel_params, coord, na.rm = FALSE) {

            coords <- coord$transform(data, panel_params)

            ggname("geom_point", scribble_points(coords$x, coords$y,
                size = coords$size,
                colour = ggplot2::alpha(coords$colour, coords$alpha)))
  }
)
