#' Create a ggplot layer containing scribble-filled tiles
#'
#' @inheritParams ggplot2::geom_tile
#' @eval rd_aesthetics("geom", "scribblepolygon")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' d <- data.frame(x = rep(letters[1:5], 5),
#'                 y = rep(LETTERS[1:5], each = 5), z = 1:25)
#'
#' ggplot(d, aes(x, y, scribbledensity = z)) +
#'   geom_scribbletile()

geom_scribbletile <- function (mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", ..., linejoin = "mitre",
                               na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE, res = 200) {

  layer(data = data, mapping = mapping, stat = stat,
        geom = GeomScribbletile, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list2(linejoin = linejoin, na.rm = na.rm, res = res, ...))
}


#' @rdname ggscribble-ggproto
#' @usage NULL
#' @format NULL
#' @export

GeomScribbletile <- ggproto("GeomScribbletile",

  ggplot2::GeomTile,

  default_aes = aes(colour = "black", fill = NA, linewidth = 1,
                             linetype = 1, alpha = NA, subgroup = NULL,
                             scribblecolour = "black", scribblewidth = 1,
                             wonkiness = 1, wibbliness = 1, randomness = 1,
                             sloppiness = 1, scribbledensity = 200, angle = 45),

  draw_panel = function (self, data, panel_params, coord, lineend = "butt",
                         linejoin = "mitre", res = 200) {

    aesthetics <- setdiff(names(data), c("x", "y", "xmin",
                                         "xmax", "ymin", "ymax"))
    index <- rep(seq_len(nrow(data)), each = 4)
    new <- data[index, aesthetics, drop = FALSE]
    new$x <- vctrs::vec_interleave(data$xmin, data$xmax,
                                   data$xmax, data$xmin)
    new$y <- vctrs::vec_interleave(data$ymax, data$ymax,
                                   data$ymin, data$ymin)
    new$group <- index
    ggname("geom_rect", GeomScribblepolygon$draw_panel(new, panel_params,
        coord, lineend = lineend, linejoin = linejoin, res = res))
  })
