#' Create a ggplot layer containing scribbled marginal rug segments
#'
#' @inheritParams ggplot2::geom_line
#' @eval rd_aesthetics("geom", "scribblerug")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   geom_scribblerug()

geom_scribblerug <- function (mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", ..., outside = FALSE,
                              sides = "bl", length = unit(0.03, "npc"),
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, res = 20) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblerug, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(outside = outside, sides = sides,
                                       length = length, na.rm = na.rm, ...))
}


#' The ggproto object that powers scribbled marginal rug lines
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribblerug <- ggplot2::ggproto("GeomScribblerug", ggplot2::GeomRug,

  default_aes = ggplot2::aes(colour = "black", linewidth = 1,
                           linetype = 1, alpha = NA,
                           wonkiness = 6, wibbliness = 0.5),

  draw_panel = function (self, data, panel_params, coord,
                         lineend = "butt", sides = "bl", outside = FALSE,
                         length = unit(0.03, "npc"), res = 10) {

    if (is.null(data$linewidth) && !is.null(data$size)) {
      data$linewidth <- data$size
    }

    if(!inherits(length, "unit")) {
      stop("geom_scribblerug lengths must be given in grid units")
    }

    length <- grid::convertY(length, "npc")
    rugs <- list()
    data <- coord$transform(data, panel_params)
    if (inherits(coord, "CoordFlip")) {
      sides <- chartr("tblr", "rlbt", sides)
    }
    rug_length <- if (!outside) {
      list(min = length, max = unit(1, "npc") - length)
    }
    else {
      list(min = -1 * length, max = unit(1, "npc") + length)
    }
    gp <- grid::gpar(col = alpha(data$colour, data$alpha), lty = data$linetype,
                     lwd = data$linewidth * ggplot2::.pt, lineend = lineend)
    if (!is.null(data$x)) {
      if (grepl("b", sides)) {
        rugs$x_b <- grid::segmentsGrob(x0 = unit(data$x, "npc"),
                                       x1 = unit(data$x, "npc"),
                                       y0 = unit(0, "npc"),
                                       y1 = rug_length$min, gp = gp)
        rugs$x_b <- wonkify(rugs$x_b, data$wonkiness, "npc")
        rugs$x_b <- wibblify(rugs$x_b, data$wibbliness, res, "npc")
      }
      if (grepl("t", sides)) {
        rugs$x_t <- grid::segmentsGrob(x0 = unit(data$x, "npc"),
                                       x1 = unit(data$x, "npc"),
                                       y0 = unit(1, "npc"),
                                       y1 = rug_length$max, gp = gp)
        rugs$x_t <- wonkify(rugs$x_t, data$wonkiness, "npc")
        rugs$x_t <- wibblify(rugs$x_t, data$wibbliness, res, "npc")
      }
    }
    if (!is.null(data$y)) {
      if (grepl("l", sides)) {
        rugs$y_l <- grid::segmentsGrob(y0 = unit(data$y, "npc"),
                                       y1 = unit(data$y, "npc"),
                                       x0 = unit(0, "npc"),
                                       x1 = rug_length$min, gp = gp)
        rugs$y_l <- wonkify(rugs$y_l, data$wonkiness, "npc")
        rugs$y_l <- wibblify(rugs$y_l, data$wibbliness, res, "npc")
      }
      if (grepl("r", sides)) {
        rugs$y_r <- grid::segmentsGrob(y0 = unit(data$y, "npc"),
                                       y1 = unit(data$y, "npc"),
                                       x0 = unit(1, "npc"),
                                       x1 = rug_length$max, gp = gp)
        rugs$y_r <- wonkify(rugs$y_r, data$wonkiness, "npc")
        rugs$y_r <- wibblify(rugs$y_r, data$wibbliness, res, "npc")
      }
    }
    grid::gTree(children = rlang::inject(grid::gList(!!!rugs)))
  })
