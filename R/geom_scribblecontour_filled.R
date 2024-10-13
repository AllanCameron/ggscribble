#' Create a ggplot layer containing scribble-filled contour lines
#'
#' @inheritParams ggplot2::geom_contour_filled
#' @eval rd_aesthetics("geom", "scribblecontour_filled")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' v <- cbind(expand.grid(x = 1:87, y = 1:61), z = as.vector(volcano))
#'
#' ggplot(v, aes(x, y, z = z)) +
#'  geom_scribblecontour_filled(bins = 6, color = NA, fill = NA,
#'    scribbledensity = 400,
#'    mapping = aes(scribblecolour = after_stat(level))) +
#'  theme_classic(16)

geom_scribblecontour_filled <- function (mapping = NULL, data = NULL,
                                         stat = "contour_filled",
                                         position = "identity", ...,
                                         bins = NULL, binwidth = NULL,
                                         breaks = NULL, na.rm = FALSE,
                                         show.legend = NA, inherit.aes = TRUE,
                                         res = 200) {

  layer(data = data, mapping = mapping, stat = stat,
        geom = GeomScribblecontourFilled, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list2(bins = bins, binwidth = binwidth,
                       breaks = breaks, na.rm = na.rm, res = res, ...))
}


#' @rdname ggscribble-ggproto
#' @usage NULL
#' @format NULL
#' @export

GeomScribblecontourFilled <- ggproto("GeomScribblecontourFilled",

  ggplot2::GeomContourFilled,

  default_aes = aes(colour = "black", fill = NA, linewidth = 1,
                             linetype = 1, alpha = NA, subgroup = NULL,
                             scribblecolour = "black", scribblewidth = 1,
                             wonkiness = 0, wibbliness = 1, randomness = 1,
                             sloppiness = 1, scribbledensity = 200, angle = 45),

  draw_panel = function (self, data, panel_params, coord, rule = "evenodd",
                         lineend = "butt", linejoin = "round", linemitre = 10,
                         res = 200) {

    if (is.null(data$linewidth) && !is.null(data$size)) {
      data$linewidth <- data$size
    }
    n <- nrow(data)
    if (n == 1) return(ggplot2::zeroGrob())

    munched <- coord_munch(coord, data, panel_params,
                                    is_closed = TRUE)
      if (is.null(munched$subgroup)) {
          munched    <- munched[order(munched$group), ]
          first_idx  <- !duplicated(munched$group)
          first_rows <- munched[first_idx, ]

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
            gp = gpar(col = first_rows$colour,
                fill = fill_alpha(first_rows$fill, first_rows$alpha),
                lwd = first_rows$linewidth * .pt,
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
            scribbledensity = first_rows$scribbledensity,
            wonkiness = first_rows$wonkiness,
            wibbliness = first_rows$wibbliness,
            sloppiness = first_rows$sloppiness,
            randomness = first_rows$randomness,
            angle = first_rows$angle,
            res = res,
            gp = gpar(col = first_rows$colour,
                fill = fill_alpha(first_rows$fill, first_rows$alpha),
                lwd = first_rows$linewidth * .pt,
                lty = first_rows$linetype,
                lineend = lineend, linejoin = linejoin, linemitre = linemitre))
        }
  })
