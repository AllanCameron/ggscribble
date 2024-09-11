#' The ggproto object that powers scribbled error bars
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribbleerrorbar <- ggplot2::ggproto("GeomScribbleerrorbar",
    ggplot2::GeomErrorbar,
    default_aes = ggplot2::aes(colour = "black", linewidth = 1,
                               linetype = 1, alpha = NA,
                               wonkiness = 0.5, wibbliness = 1),
    draw_panel = function (self, data, panel_params, coord, lineend = "butt",
                           width = NULL, flipped_aes = FALSE, res = 20) {
      grobs <- ggplot2::GeomErrorbar$draw_panel(data, panel_params, coord,
                                                lineend, width, flipped_aes)

      grobs <- wonkify(grobs, data$wonkiness)
      wibblify(grobs, data$wibbliness)
    }
)

#' Create a ggplot layer containing scribbled error bars
#'
#' @inheritParams ggplot2::geom_line
#' @eval rd_aesthetics("geom", "scribbleerrorbar")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   trt = factor(c(1, 1, 2, 2)),
#'   resp = c(1, 5, 3, 4),
#'   group = factor(c(1, 2, 1, 2)),
#'   upper = c(1.1, 5.3, 3.3, 4.2),
#'   lower = c(0.8, 4.6, 2.4, 3.6)
#' )
#'
#' p <- ggplot2::ggplot(df, ggplot2::aes(trt, resp, colour = group))
#' p + geom_scribbleerrorbar(aes(ymin = lower, ymax = upper), width = 0.2)

geom_scribbleerrorbar <- function (mapping = NULL, data = NULL,
                                   stat = "identity", position = "identity",
                                   ..., na.rm = FALSE, orientation = NA,
                                   show.legend = NA, inherit.aes = TRUE,
                                   res = 20) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribbleerrorbar, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(na.rm = na.rm, orientation = orientation,
                                       res = res, ...))
}
