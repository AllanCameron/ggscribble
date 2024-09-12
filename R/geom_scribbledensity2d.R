#' The ggproto object that powers scribbled 2d density lines
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribbledensity2d <- ggplot2::ggproto("GeomScribbledensity2d",
   ggplot2::GeomDensity2d,

   default_aes = ggplot2::aes(colour = "black", linewidth = 1,
                              linetype = 1, alpha = NA,
                              wonkiness = 0, wibbliness = 0.5),

   draw_panel = function (self, data, panel_params, coord, arrow = NULL,
                          lineend = "butt", linejoin = "round", linemitre = 10,
                          na.rm = FALSE, res = 5) {

     if(!is.null(arrow)) {
       arrow <- NULL
       cli::cli_warn("Arrows are not implemented in scribbled paths")
     }

     if (is.null(data$linewidth) && !is.null(data$size)) {
       data$linewidth <- data$size
     }

     if (!anyDuplicated(data$group)) {
       cli::cli_inform(paste0(c("{.fn {snake_class(self)}}: ",
                                "Each group consists of only one observation."),
                    i = "Do you need to adjust the {.field group} aesthetic?"))
     }

    data <- data[order(data$group), , drop = FALSE]
    munched <- ggplot2::coord_munch(coord, data, panel_params)
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(zeroGrob())
    attr <- dapply(munched, "group", function(df) {
        linetype <- unique0(df$linetype)
        data_frame0(solid = identical(linetype, 1) || identical(linetype,
            "solid"), constant = nrow(unique0(df[, names(df) %in%
            c("alpha", "colour", "linewidth", "linetype")])) ==
            1, .size = 1)
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
        cli::cli_abort(paste("{.fn {snake_class(self)}}",
        "can't have varying {.field colour},",
        "{.field linewidth}, and/or {.field alpha}",
        "along the line when {.field linetype} isn't solid."))
    }
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <- c(group_diff, TRUE)
    if (!constant) {
        arrow <- repair_segment_arrow(arrow, munched$group)
        grobs <- grid::segmentsGrob(munched$x[!end], munched$y[!end],
                                    munched$x[!start],
            munched$y[!start], default.units = "native", arrow = arrow,
            gp = grid::gpar(col = ggplot2::alpha(munched$colour,
                                                 munched$alpha)[!end],
                fill = ggplot2::alpha(munched$colour, munched$alpha)[!end],
                lwd = munched$linewidth[!end] * ggplot2::.pt,
                lty = munched$linetype[!end],
                lineend = lineend, linejoin = linejoin, linemitre = linemitre))

        grobs <- wonkify(grobs, munched$wonkiness[!end])
        wibblify(grobs, munched$wibbliness[!end], res = res)
    }
    else {
        id <- match(munched$group, unique0(munched$group))
        grobs <- grid::polylineGrob(munched$x, munched$y, id = id,
                           default.units = "native",
            arrow = arrow, gp = grid::gpar(col = ggplot2::alpha(munched$colour,
                munched$alpha)[start], fill = ggplot2::alpha(munched$colour,
                munched$alpha)[start], lwd = munched$linewidth[start] *
                ggplot2::.pt, lty = munched$linetype[start], lineend = lineend,
                linejoin = linejoin, linemitre = linemitre))
        grobs <- wonkify(grobs, munched$wonkiness[start])
        wibblify(grobs, munched$wibbliness[start], res = res)
    }
   }
)

#' Create a ggplot layer containing scribbled marginal rug segments
#'
#' @inheritParams ggplot2::geom_line
#' @eval rd_aesthetics("geom", "scribbledensity2d")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' set.seed(1)
#'
#' ggplot2::ggplot(data.frame(x = rnorm(50), y = rnorm(50)),
#'                 ggplot2::aes(x, y)) +
#'  geom_scribbledensity2d()
geom_scribbledensity2d <- function (mapping = NULL, data = NULL,
                                    stat = "density_2d", position = "identity",
                                    ..., contour_var = "density",
                                    lineend = "butt", linejoin = "round",
                                    linemitre = 10, na.rm = FALSE, res = 10,
                                    show.legend = NA, inherit.aes = TRUE) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribbledensity2d, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(lineend = lineend, linejoin = linejoin,
                                       linemitre = linemitre, contour = TRUE,
                                       contour_var = contour_var,
                                       na.rm = na.rm, ..., res = res))
}
