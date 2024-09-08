#' The ggproto object that powers scribbled steps
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribblestep <- ggplot2::ggproto("GeomScribblestep", ggplot2::GeomStep,

  default_aes = ggplot2::aes(colour = "black", linewidth = 1,
                             linetype = 1, alpha = NA,
                             wonkiness = 0, wibbliness = 1),

  draw_panel = function (data, panel_params, coord, direction = "hv") {
      data <- dapply(data, "group", stairstep, direction = direction)
      GeomScribblepath$draw_panel(data, panel_params, coord)
  }
)

#' Create a ggplot layer containing scribbled steps
#'
#' @inheritParams ggplot2::geom_line
#' @eval rd_aesthetics("geom", "scribblestep")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#'
#' ggplot2::economics[ggplot2::economics$date > as.Date("2013-01-01"), ] |>
#'   ggplot2::ggplot(ggplot2::aes(date, unemploy)) +
#'   geom_scribblestep()

geom_scribblestep <- function (mapping = NULL, data = NULL, stat = "identity",
          position = "identity", direction = "hv", na.rm = FALSE,
          show.legend = NA, inherit.aes = TRUE, ...) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblestep, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(direction = direction,
                                       na.rm = na.rm, ...))
}
