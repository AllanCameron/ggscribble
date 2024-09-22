# geom_scribblesegment and its ggproto -----------------------------------------

#' Create a ggplot layer containing scribbled line segments
#'
#' @inheritParams ggplot2::geom_segment
#' @eval rd_aesthetics("geom", "scribblesegment")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' ggplot2::ggplot(data = data.frame(x = c(1, 3), x1 = c(2, 2),
#'                                   y = c(5, 2), y1 = c(3, 4),
#'                                   g = c("A", "B")),
#'                 mapping = ggplot2::aes(x, y, colour = g)) +
#'   geom_scribblesegment(ggplot2::aes(xend = x1, yend = y1),
#'                        res = 200, wibbliness = 2)

geom_scribblesegment <- function(mapping = NULL, data = NULL, stat = "identity",
                                 position = "identity", na.rm = FALSE,
                                 res = 200, show.legend = NA,
                                 inherit.aes = TRUE, ...) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblesegment,
                 position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = rlang::list2(na.rm = na.rm, res = res, ...))
}

#' The ggproto object that powers scribbled line segments
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribblesegment <- ggplot2::ggproto("GeomScribblesegment",
                                        ggplot2::GeomSegment,

  default_aes = ggplot2::aes(colour = "black", linewidth = 1,
                             linetype = 1, alpha = NA,
                             wonkiness = 0, wibbliness = 1),

  draw_panel = function (self, data, panel_params, coord, arrow = NULL,
                         arrow.fill = NULL, lineend = "butt", res = 200,
                         linejoin = "round", na.rm = FALSE) {

    data$xend <- data$xend %||% data$x
    data$yend <- data$yend %||% data$y
    if (is.null(data$linewidth) && !is.null(data$size)) {
      data$linewidth <- data$size
    }
    data <- ggplot2::remove_missing(data, na.rm = na.rm, c("x", "y", "xend",
        "yend", "linetype", "linewidth"), name = "geom_segment")
    if (empty(data)) return(ggplot2::zeroGrob())

    first_rows <- get_first_rows(data)

    if (coord$is_linear()) {
        coord <- coord$transform(data, panel_params)
        arrow.fill <- arrow.fill %||% coord$colour
        grobs <- grid::segmentsGrob(coord$x, coord$y, coord$xend, coord$yend,
                default.units = "native",
                gp = grid::gpar(col = ggplot2::alpha(coord$colour,
                coord$alpha), fill = ggplot2::alpha(arrow.fill, coord$alpha),
                lwd = coord$linewidth, lty = coord$linetype,
                lineend = lineend, linejoin = linejoin), arrow = arrow)
        grobs <- wonkify(grobs, wonkiness = first_rows$wonkiness)
        return(wibblify(grobs, wibbliness = first_rows$wibbliness))
    }
    data$group <- 1:nrow(data)
    starts <- subset(data, select = c(-xend, -yend))
    ends <- rename(subset(data, select = c(-x, -y)), c(xend = "x",
        yend = "y"))
    pieces <- vctrs::vec_rbind(starts, ends)
    pieces <- pieces[order(pieces$group), ]
    GeomScribblepath$draw_panel(pieces, panel_params, coord, arrow = arrow,
        lineend = lineend)
  }
)


