#' Create a ggplot layer containing scribbled spokes
#'
#' @inheritParams ggplot2::geom_spoke
#' @inheritParams geom_scribblearea
#' @eval rd_aesthetics("geom", "scribblespoke")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <- expand.grid(x = 1:10, y = 1:10)
#'
#' set.seed(1)
#' df$angle <- runif(100, 0, 2*pi)
#' df$speed <- runif(100, 0, sqrt(0.1 * df$x))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_scribblespoke(aes(angle = angle, radius = speed))

geom_scribblespoke <- function (mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", ..., na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE,
                                res = 200) {

  layer(data = data, mapping = mapping, geom = GeomScribblespoke,
        stat = stat, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(na.rm = na.rm, res = res, ...))
}


#' @rdname ggscribble-ggproto
#' @usage NULL
#' @format NULL
#' @export

GeomScribblespoke <- ggproto("GeomScribblespoke",
                                      ggplot2::GeomSpoke,

  default_aes = aes(colour = "black", linewidth = 1,
                             linetype = 1, alpha = NA,
                             wonkiness = 0, wibbliness = 1),

  draw_panel = function (self, data, panel_params, coord, arrow = NULL,
                         arrow.fill = NULL, lineend = "butt",
                         linejoin = "round", na.rm = FALSE, res = 200) {

    if(!is.null(arrow)) {
      arrow <- NULL
      cli::cli_warn("Arrows are not implemented in scribbled paths")
    }

    grobs <- ggplot2::GeomSpoke$draw_panel(data, panel_params, coord,
                                           arrow, arrow.fill, lineend, linejoin,
                                           na.rm)
    wibblify(wonkify(grobs, data$wonkiness), data$wibbliness, res = res)
})
