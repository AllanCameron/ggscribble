
#' Create a ggplot layer containing scribble-filled rectangles
#'
#' @inheritParams ggplot2::geom_rect
#' @eval rd_aesthetics("geom", "scribblerect")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' ggplot2::ggplot(data = data.frame(xmin = c(5, 10), xmax = c(15, 20),
#'                                   ymin = c(5, 10), ymax = c(10, 20),
#'                                   g = 1:2),
#'                 mapping = ggplot2::aes(xmin = xmin, xmax = xmax,
#'                                        ymin = ymin, ymax = ymax,
#'                                        group = g)) +
#'  geom_scribblerect()
#'
geom_scribblerect <- function (mapping = NULL, data = NULL, stat = "identity",
          position = "identity", ..., linejoin = "mitre",
          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblerect, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(linejoin = linejoin, na.rm = na.rm, ...))
}

#' The ggproto object that powers scribble-filled rectangles.
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribblerect <- ggplot2::ggproto("GeomScribblerect", ggplot2::GeomRect,
      default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 1,
                      linetype = 1, alpha = NA, subgroup = NULL,
                      scribblecolour = "black", scribblewidth = 1,
                      wonkiness = 1, wibbliness = 1, randomness = 1,
                      sloppiness = 1, density = 200, angle = 46),
  draw_panel = function (self, data, panel_params, coord, lineend = "butt",
                         linejoin = "mitre") {

    if (is.null(data$linewidth) && !is.null(data$size)) {
      data$linewidth <- data$size
    }
    aesthetics <- setdiff(names(data), c("x", "y", "xmin",
        "xmax", "ymin", "ymax"))
    index <- rep(seq_len(nrow(data)), each = 5)
    new <- data[index, aesthetics, drop = FALSE]
    new$x <- vctrs::vec_interleave(data$xmin, data$xmax, data$xmax,
        data$xmin, data$xmin)
    new$y <- vctrs::vec_interleave(data$ymax, data$ymax, data$ymin,
        data$ymin, data$ymax)
    new$group <- index
    grob <- GeomScribblepolygon$draw_panel(new, panel_params,
        coord, lineend = lineend, linejoin = linejoin)
    grob$name <- grid::grobName(grob, "geom_scribblerect")
    grob
})
