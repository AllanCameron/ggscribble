#' Create a ggplot layer containing scribble-filled density contours
#'
#' @inheritParams ggplot2::geom_density2d_filled
#' @eval rd_aesthetics("geom", "scribbledensity2d_filled")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' set.seed(1)
#'
#' d <- data.frame(x = rnorm(1000), y = rnorm(1000))
#'
#' ggplot2::ggplot(d, ggplot2::aes(x, y)) +
#'   geom_scribbledensity2d_filled(
#'     ggplot2::aes(scribbledensity = ggplot2::after_stat(level)),
#'     fill = NA, colour = "blue4", scribblecolour = "blue4") +
#'   ggplot2::theme_classic(16)

geom_scribbledensity2d_filled <- function(mapping = NULL, data = NULL,
    stat = "density_2d_filled", position = "identity", ...,
    contour_var = "density", na.rm = FALSE, show.legend = NA,
    inherit.aes = TRUE, res = 200) {

    ggplot2::layer(data = data, mapping = mapping, stat = stat,
                   geom = GeomScribbledensity2dFilled, position = position,
                   show.legend = show.legend, inherit.aes = inherit.aes,
                   params = rlang::list2(na.rm = na.rm, contour = TRUE,
                                         contour_var = contour_var,
                                         res = res, ...))
}

#' The ggproto object that powers scribble-filled density contours
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export

GeomScribbledensity2dFilled <- ggplot2::ggproto("GeomScribbledensity2dFilled",

  ggplot2::GeomDensity2dFilled,

  default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 1,
                             linetype = 1, alpha = NA, subgroup = NULL,
                             scribblecolour = "black", scribblewidth = 1,
                             wonkiness = 0, wibbliness = 1, randomness = 1,
                             sloppiness = 1, scribbledensity = 200, angle = 45),

  draw_panel =  function (self, data, panel_params, coord, rule = "evenodd",
                          lineend = "butt", linejoin = "round",
                          linemitre = 10, res = 200) {

    if (is.null(data$linewidth) && !is.null(data$size)) {
      data$linewidth <- data$size
    }

    n <- nrow(data)

    if (n == 1) return(ggplot2::zeroGrob())

    munched <- ggplot2::coord_munch(coord, data, panel_params, is_closed = TRUE)

    if (is.null(munched$subgroup)) {
        munched <- munched[order(munched$group), ]
        first_idx <- !duplicated(munched$group)
        first_rows <- munched[first_idx, ]
        ggname("geom_scribbledensity2d_filled",
          scribbleGrob(x = munched$x, y = munched$y,
            default.units = "npc", pathId = munched$group,
            scribblecolour = first_rows$scribblecolour,
            scribblewidth = first_rows$scribblewidth,
            scribbledensity = first_rows$scribbledensity,
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
                lineend = lineend, linejoin = linejoin, linemitre = linemitre)))

    } else {
        munched <- munched[order(munched$group, munched$subgroup),]
        id <- match(munched$subgroup, unique0(munched$subgroup))
        first_idx <- !duplicated(munched$group)
        first_rows <- munched[first_idx, ]

        ggname("geom_scribbledensity2d_filled",
            scribbleGrob(munched$x, munched$y, default.units = "npc",
              id = id, pathId = munched$group,
              scribblecolour = first_rows$scribblecolour,
              scribblewidth = first_rows$scribblewidth,
              scribbledensity = first_rows$scribbledensity,
              wonkiness = first_rows$wonkiness,
              wibbliness = first_rows$wibbliness,
              sloppiness = first_rows$sloppiness,
              randomness = first_rows$randomness,
              angle = first_rows$angle,
              res = res,
              gp = grid::gpar(col = first_rows$colour,
                              fill = ggplot2::fill_alpha(first_rows$fill,
                                                         first_rows$alpha),
                              lwd = first_rows$linewidth * ggplot2::.pt,
                              lty = first_rows$linetype,
                              lineend = lineend, linejoin = linejoin,
                              linemitre = linemitre)))
      }
    }
  )
