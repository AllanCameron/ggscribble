#' The ggproto object that powers scribbled function paths
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export

GeomScribblefunction <- ggplot2::ggproto("GeomScribblefunction",
 ggplot2::GeomFunction,

 default_aes = ggplot2::aes(colour = "black", linewidth = 1,
                            linetype = 1, alpha = NA,
                            wonkiness = 0, wibbliness = 1),

 draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                       lineend = "butt", linejoin = "round", linemitre = 10,
                       na.rm = FALSE) {
   groups <- unique0(data$group)
   if (length(groups) > 1) {
     cli::cli_warn(c(paste("Multiple drawing groups in",
                           "{.fn {snake_class(self)}}"),
                     i = paste("Did you use the correct {.field group},",
                               "{.field colour}, or {.field fill}",
                               "aesthetics?")))
   }
   GeomScribblepath$draw_panel(data, panel_params, coord, arrow,
                               lineend, linejoin, linemitre, na.rm)
 })


#' Create a ggplot layer containing scribbled function paths
#'
#' @inheritParams ggplot2::geom_line
#' @eval rd_aesthetics("geom", "scribblefunction")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#' @examples
#' ggplot2::ggplot() +
#'   geom_scribblefunction(fun = dnorm, colour = "red") +
#'   ggplot2::xlim(-5, 5)
geom_scribblefunction <- function (mapping = NULL, data = NULL,
          stat = "function", position = "identity", ..., na.rm = FALSE,
          show.legend = NA, inherit.aes = TRUE, res = 200) {

  if (is.null(data)) {
    data <- ensure_nonempty_data
  }
  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblefunction, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(na.rm = na.rm, ...))
}
