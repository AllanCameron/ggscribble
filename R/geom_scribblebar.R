#' Create a ggplot layer containing scribble-filled bars
#'
#' @inheritParams ggplot2::geom_bar
#' @eval rd_aesthetics("geom", "scribblebar")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' ggplot2::ggplot(iris[c(1, 51, 101), ],
#'                 ggplot2::aes(Species, Sepal.Length, colour = Species)) +
#'   geom_scribblecol(ggplot2::aes(scribblecolour = Species),
#'                    scribblewidth = 2) +
#'   ggplot2::theme_classic(20)
#'
geom_scribblebar <- function (mapping = NULL, data = NULL, stat = "count",
                              position = "stack", ..., just = 0.5, width = NULL,
                              na.rm = FALSE, orientation = NA, show.legend = NA,
                              inherit.aes = TRUE, res = 200) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblebar, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(just = just, width = width,
                                       na.rm = na.rm, res = res,
                                       orientation = orientation, ...))
}

#' @rdname geom_scribblebar
#' @export
geom_scribblecol <- function (mapping = NULL, data = NULL, stat = "identity",
                              position = "stack", ..., just = 0.5, width = NULL,
                              na.rm = FALSE, orientation = NA, show.legend = NA,
                              inherit.aes = TRUE, res = 200) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblecol, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(just = just, width = width,
                                       na.rm = na.rm, res = res,
                                       orientation = orientation, ...))
}



#' The ggproto object that powers scribble-filled bar plots.
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribblebar <- ggplot2::ggproto("GeomScribblebar", ggplot2::GeomBar,

  default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 1,
                linetype = 1, alpha = NA, subgroup = NULL,
                scribblecolour = "black", scribblewidth = 1,
                wonkiness = 1, wibbliness = 1, randomness = 1,
                sloppiness = 1, density = 200, angle = 45),

  draw_panel = function (self, data, panel_params, coord, lineend = "butt",
                         linejoin = "mitre", width = NULL,
                         flipped_aes = FALSE, res = res) {

  ggplot2::ggproto_parent(GeomScribblerect, self)$draw_panel(data, panel_params,
      coord, lineend = lineend, linejoin = linejoin, res = res)

  }
)

#' The ggproto object that powers scribble-filled bar plots.
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribblecol <- ggplot2::ggproto("GeomScribblecol",
                                    GeomScribblebar,

  default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 1,
                linetype = 1, alpha = NA, subgroup = NULL,
                scribblecolour = "black", scribblewidth = 1,
                wonkiness = 1, wibbliness = 1, randomness = 1,
                sloppiness = 1, density = 200, angle = 45),

  draw_panel = function (self, data, panel_params, coord, lineend = "butt",
                         linejoin = "mitre", width = NULL,
                         flipped_aes = FALSE, res) {

    ggplot2::ggproto_parent(GeomScribblebar, self)$draw_panel(data,
                    panel_params, coord, lineend = lineend, linejoin = linejoin,
                    res = res)

  }
)
