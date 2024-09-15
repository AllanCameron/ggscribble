#' Create a ggplot layer containing scribble-filled polygonal areas
#'
#' @inheritParams ggplot2::geom_polygon
#' @eval rd_aesthetics("geom", "scribblepolygon")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' ggplot2::ggplot(data.frame(x = seq(0, pi, 0.1), y = sin(seq(0, pi, 0.1))),
#'                 ggplot2::aes(x, y)) +
#'  geom_scribblearea()

geom_scribblearea <- function (mapping = NULL, data = NULL, stat = "align",
                               position = "stack", na.rm = FALSE,
                               orientation = NA, show.legend = NA, res = 100,
                               inherit.aes = TRUE, ...,
                               outline.type = "upper") {

  outline.type <- rlang::arg_match0(outline.type, c("both", "upper",
                                                    "lower", "full"))

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblearea, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(na.rm = na.rm, orientation = orientation,
                                       outline.type = outline.type, ...,
                                       res = res))
}

#' The ggproto object that powers scribble-filled area polygons.
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export

GeomScribblearea <- ggplot2::ggproto("GeomScribblearea",
  ggplot2::GeomArea,

  default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 1,
                             linetype = 1, alpha = NA, subgroup = NULL,
                             scribblecolour = "black", scribblewidth = 1,
                             wonkiness = 0.1, wibbliness = 1, randomness = 1,
                             sloppiness = 1, density = 200, angle = 45),

  draw_group = function (self, data, panel_params, coord, lineend = "butt",
                         linejoin = "round", linemitre = 10, na.rm = FALSE,
                         flipped_aes = FALSE, outline.type = "both",
                         res = 100) {

    if (is.null(data$linewidth) && !is.null(data$size)) {
      data$linewidth <- data$size
    }
    data <- ggplot2::flip_data(data, flipped_aes)
    if (na.rm)
        data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
    data <- data[order(data$group), ]
    aes <- unique0(data[names(data) %in% c("colour", "fill",
        "linewidth", "linetype", "alpha", "wonkiness", "wibbliness",
        "angle", "density", "scribblecolour", "scribblewidth",
        "randomness", "sloppiness")])
    if (nrow(aes) > 1) {
        cli::cli_abort("Aesthetics can not vary along a ribbon.")
    }
    aes <- as.list(aes)
    missing_pos <- !stats::complete.cases(data[c("x", "ymin",
        "ymax")])
    ids <- cumsum(missing_pos) + 1
    ids[missing_pos] <- NA
    data <- unclass(data)
    upper_keep <- if (is.null(data$align_padding))
        TRUE
    else !data$align_padding
    positions_upper <- data_frame0(x  = data$x[upper_keep],
                                   y  = data$ymax[upper_keep],
                                   id = ids[upper_keep])
    positions_lower <- data_frame0(x  = rev(data$x),
                                   y  = rev(data$ymin),
                                   id = rev(ids))
    positions_upper <- ggplot2::flip_data(positions_upper, flipped_aes)
    positions_lower <- ggplot2::flip_data(positions_lower, flipped_aes)
    munched_upper <- ggplot2::coord_munch(coord, positions_upper, panel_params)
    munched_lower <- ggplot2::coord_munch(coord, positions_lower, panel_params)
    munched_poly <- vctrs::vec_rbind(munched_upper, munched_lower)
    is_full_outline <- identical(outline.type, "full")
    scrib_gp <- grid::gpar(fill = ggplot2::fill_alpha(aes$fill, aes$alpha),
                           col = if (is_full_outline) aes$colour else NA,
                           lwd = if (is_full_outline)
                                      aes$linewidth * ggplot2::.pt else 0,
                           lty = if (is_full_outline)
                                      aes$linetype else 1,
                           lineend = lineend, linejoin = linejoin,
                           linemitre = linemitre)

    g_poly <- scribbleGrob(x = munched_poly$x,
                 y = munched_poly$y,
                 default.units = "npc",
                 id = munched_poly$id,
                 scribblecolour = aes$scribblecolour,
                 scribblewidth = aes$scribblewidth,
                 density = aes$density,
                 wonkiness = aes$wonkiness,
                 wibbliness = aes$wibbliness,
                 sloppiness = aes$sloppiness,
                 angle = aes$angle,
                 res = res,
                 gp = scrib_gp)

    if (is_full_outline) {
        return(ggname("geom_scribbleribbon", g_poly))
    }

    munched_lower$id <- munched_lower$id + max(ids, na.rm = TRUE)
    rlang::arg_match0(outline.type, c("both", "upper", "lower"))
    munched_lines <- switch(outline.type, both = vctrs::vec_rbind(munched_upper,
        munched_lower), upper = munched_upper, lower = munched_lower)
    g_lines <- grid::polylineGrob(munched_lines$x, munched_lines$y,
        id = munched_lines$id, default.units = "native",
        gp = grid::gpar(col = aes$colour, lwd = aes$linewidth * ggplot2::.pt,
                        lty = aes$linetype, lineend = lineend,
                        linejoin = linejoin, linemitre = linemitre))
    g_lines <- wonkify(g_lines, aes$wonkiness)
    g_lines <- wibblify(g_lines, aes$wibbliness, res)
    ggname("geom_ribbon", grid::grobTree(g_poly, g_lines))
})
