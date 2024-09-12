#' The ggproto object that powers scribbled horizontal error bars
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribbleerrorbarh <- ggplot2::ggproto("GeomScribbleerrorbarh",
  ggplot2::GeomErrorbarh,
  default_aes = ggplot2::aes(colour = "black", linewidth = 1,
                             linetype = 1, alpha = NA,
                             wonkiness = 0.5, wibbliness = 1),
  draw_panel = function (self, data, panel_params, coord, height = NULL,
                         lineend = "butt", res = 20) {
    grobs <- ggplot2::GeomErrorbarh$draw_panel(data, panel_params, coord,
                                               height, lineend)
    grobs <- wonkify(grobs, data$wonkiness)
    wibblify(grobs, data$wibbliness, res = res)
  })


#' Create a ggplot layer containing scribbled horizontal error bars
#'
#' @inheritParams ggplot2::geom_line
#' @eval rd_aesthetics("geom", "scribbleerrorbarh")
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
#' p <- ggplot2::ggplot(df, ggplot2::aes(resp, trt, colour = group))
#' p + geom_scribbleerrorbarh(aes(xmin = lower, xmax = upper), height = 0.2)

geom_scribbleerrorbarh <- function (mapping = NULL, data = NULL,
                                   stat = "identity", position = "identity",
                                   ..., na.rm = FALSE,
                                   show.legend = NA, inherit.aes = TRUE,
                                   res = 20) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribbleerrorbarh, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(na.rm = na.rm, res = res, ...))
}
