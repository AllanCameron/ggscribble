#' Create a ggplot layer containing scribbled points
#'
#' @inheritParams ggplot2::geom_point
#' @eval rd_aesthetics("geom", "scribblepoint")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' ggplot(iris, aes(Species, Petal.Length, colour = Species)) +
#'   geom_scribblepoint(position = ggplot2::position_jitter(0.5))

geom_scribblepoint <- function (mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", ..., na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE) {

  layer(data = data, mapping = mapping, stat = stat,
        geom = GeomScribblepoint, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list2(na.rm = na.rm, ...))
}


#' @rdname ggscribble-ggproto
#' @usage NULL
#' @format NULL
#' @export

GeomScribblepoint <- ggproto("GeomScribblepoint",

  ggplot2::GeomPoint,

  default_aes = aes(colour = "black", alpha = NA, size = 1.5),

  draw_panel = function (self, data, panel_params, coord, na.rm = FALSE) {

            coords <- coord$transform(data, panel_params)

            ggname("geom_point", scribble_points(coords$x, coords$y,
                size = coords$size,
                colour = ggplot2::alpha(coords$colour, coords$alpha)))
  }
)
