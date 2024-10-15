#' Create a ggplot layer containing scribbled steps
#'
#' @inheritParams ggplot2::geom_step
#' @inheritParams geom_scribblearea
#' @eval rd_aesthetics("geom", "scribblestep")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(economics[economics$date > as.Date("2013-01-01"), ],
#'        aes(date, unemploy)) +
#'   geom_scribblestep()

geom_scribblestep <- function (mapping = NULL, data = NULL, stat = "identity",
          position = "identity", direction = "hv", na.rm = FALSE,
          show.legend = NA, inherit.aes = TRUE, ...) {

  layer(data = data, mapping = mapping, stat = stat,
        geom = GeomScribblestep, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list2(direction = direction, na.rm = na.rm, ...))
}


#' @rdname ggscribble-ggproto
#' @usage NULL
#' @format NULL
#' @export

GeomScribblestep <- ggproto("GeomScribblestep", ggplot2::GeomStep,

  default_aes = aes(colour = "black", linewidth = 1,
                             linetype = 1, alpha = NA,
                             wonkiness = 0, wibbliness = 1),

  draw_panel = function (data, panel_params, coord, direction = "hv") {
      data <- dapply(data, "group", stairstep, direction = direction)
      GeomScribblepath$draw_panel(data, panel_params, coord)
  }
)
