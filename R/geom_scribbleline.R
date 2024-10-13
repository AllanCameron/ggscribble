

#' Create a ggplot layer containing scribbled lines
#'
#' @inheritParams ggplot2::geom_line
#' @eval rd_aesthetics("geom", "scribbleline")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' ggplot(data = data.frame(x = rep(c(1, 2, 3, 4, 5), 2),
#'                                   y = c(5, 2, 0, 5, 3, 4, 1, 3, 2, 4),
#'                                   g = rep(c("A", "B"), each = 5)),
#'                 mapping = aes(x, y, colour = g)) +
#'   geom_scribbleline(res = 200, wibbliness = 2)

geom_scribbleline <- function (mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", na.rm = FALSE,
                               orientation = NA, res = 200,
                               show.legend = NA, inherit.aes = TRUE, ...) {

  layer(data = data, mapping = mapping, stat = stat,
        geom = GeomScribbleline,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(na.rm = na.rm, res = res,
                       orientation = orientation, ...))
}


#' @rdname ggscribble-ggproto
#' @usage NULL
#' @format NULL
#' @export

GeomScribbleline <- ggproto("Scribbleline", ggplot2::GeomLine,

  default_aes = aes(colour = "black", linewidth = 1,
                             linetype = 1, alpha = NA,
                             wonkiness = 0, wibbliness = 1),

  draw_panel = function (self, data, panel_params, coord, arrow = NULL,
                         lineend = "butt", linejoin = "round",
                         linemitre = 10, na.rm = FALSE, res = 200) {

    grobs <- ggplot2::GeomLine$draw_panel(data, panel_params, coord, arrow,
                                          lineend, linejoin, linemitre)

    first_rows <- get_first_rows(data)

    grobs <- wonkify(grobs, first_rows$wonkiness)
    wibblify(grobs, first_rows$wibbliness, res = res)
  })
