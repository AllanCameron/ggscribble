
#' Create a ggplot layer containing scribble-filled polygons
#'
#' @inheritParams ggplot2::geom_polygon
#' @eval rd_aesthetics("geom", "scribblepolygon")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' ggplot2::ggplot(data = data.frame(x = c(5, 10, 7.5, 10, 15, 12.5),
#'                                   y = c(5, 5, 10, 5, 5, 10),
#'                                   g = rep(1:2, each = 3)),
#'                 mapping = ggplot2::aes(x, y, group = g)) +
#'  geom_scribblepolygon()
#'
geom_scribblepolygon <- function (mapping = NULL, data = NULL,
                                  stat = "identity", position = "identity", ...,
                                  linejoin = "mitre", na.rm = FALSE,
                                  show.legend = NA, inherit.aes = TRUE,
                                  res = 200) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblepolygon,
                 position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = rlang::list2(linejoin = linejoin, na.rm = na.rm,
                                       res = res, ...))
}



#' The ggproto object that powers scribble-filled shapes.
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export

GeomScribblepolygon <- ggplot2::ggproto("GeomScribblepolygon",
    ggplot2::GeomPolygon,

    default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 1,
                      linetype = 1, alpha = NA, subgroup = NULL,
                      scribblecolour = "black", scribblewidth = 1,
                      wonkiness = 1, wibbliness = 1, randomness = 1,
                      sloppiness = 1, density = 200, angle = 45),

    draw_panel = function (self, data, panel_params, coord,
                           lineend = "butt", linejoin = "round",
                           linemitre = 10, res = res) {

      if (is.null(data$linewidth) && !is.null(data$size)) {
        data$linewidth <- data$size
      }
      n <- nrow(data)
      if (n == 1) return(ggplot2::zeroGrob())

      munched <- ggplot2::coord_munch(coord, data, panel_params,
                                      is_closed = TRUE)

      if (is.null(munched$subgroup)) {
        munched <- munched[order(munched$group), ]
        first_idx <- !duplicated(munched$group)
        first_rows <- munched[first_idx, ]

      scribbleGrob(x = munched$x, y = munched$y,
          default.units = "npc", pathId = munched$group,
          scribblecolour = first_rows$scribblecolour,
          scribblewidth = first_rows$scribblewidth,
          density = first_rows$density,
          wonkiness = first_rows$wonkiness,
          wibbliness = first_rows$wibbliness,
          sloppiness = first_rows$sloppiness,
          randomness = first_rows$randomness,
          angle = first_rows$angle,
          gp = grid::gpar(col = first_rows$colour,
              fill = ggplot2::fill_alpha(first_rows$fill, first_rows$alpha),
              lwd = first_rows$linewidth * ggplot2::.pt,
              lty = first_rows$linetype,
              lineend = lineend, linejoin = linejoin, linemitre = linemitre))
      } else {
        munched <- munched[order(munched$group, munched$subgroup), ]
        id <- match(munched$subgroup, unique0(munched$subgroup))
        first_idx <- !duplicated(munched$group)
        first_rows <- munched[first_idx, ]

        scribbleGrob(x = munched$x, y = munched$y,
          default.units = "npc", id = id, pathId = munched$group,
          scribblecolour = first_rows$scribblecolour,
          scribblewidth = first_rows$scribblewidth,
          density = first_rows$density,
          wonkiness = first_rows$wonkiness,
          wibbliness = first_rows$wibbliness,
          sloppiness = first_rows$sloppiness,
          randomness = first_rows$randomness,
          angle = first_rows$angle,
          res = res,
          gp = grid::gpar(col = first_rows$colour,
              fill = ggplot2::fill_alpha(first_rows$fill, first_rows$alpha),
              lwd = first_rows$linewidth * ggplot2::.pt,
              lty = first_rows$linetype,
              lineend = lineend, linejoin = linejoin, linemitre = linemitre))
      }
    }
)
