#' Create a ggplot layer containing scribble-filled hex bins
#'
#' @inheritParams ggplot2::geom_hex
#' @eval rd_aesthetics("geom", "scribblehex")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' set.seed(1)
#'
#' d <- data.frame(x = rnorm(1000), y = rnorm(1000))
#'
#' ggplot2::ggplot(d, ggplot2::aes(x, y)) +
#'   geom_scribblehex(ggplot2::aes(scribbledensity = ggplot2::after_stat(count)),
#'                    bins = 10, fill = NA, colour = "blue4",
#'                    scribblecolour = "blue4") +
#'   ggplot2::theme_classic(16)
geom_scribblehex <- function (mapping = NULL, data = NULL, stat = "binhex",
                              position = "identity", ..., na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, res = 200) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblehex, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(na.rm = na.rm, res = res, ...))
}

#' The ggproto object that powers scribble-filled hex bins.
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribblehex <- ggplot2::ggproto("GeomScribblehex", ggplot2::GeomHex,

  default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 1,
                  linetype = 1, alpha = NA, subgroup = NULL,
                  scribblecolour = "black", scribblewidth = 1,
                  wonkiness = 1, wibbliness = 1, randomness = 1,
                  sloppiness = 1, scribbledensity = 200, angle = 45),

  draw_group = function (self, data, panel_params, coord, lineend = "butt",
                         linejoin = "mitre", linemitre = 10, res = 200) {

    if (is.null(data$linewidth) && !is.null(data$size)) {
      data$linewidth <- data$size
    }

    n <- nrow(data)
    if (n == 1) return(ggplot2::zeroGrob())

    if (!is.null(data$width)) {
        dx <- data$width[1]/2
    }
    else {
        dx <- ggplot2::resolution(data$x, FALSE)
    }
    if (!is.null(data$height)) {
        dy <- data$height[1]/sqrt(3)/2
    }
    else {
        dy <- ggplot2::resolution(data$y, FALSE)/sqrt(3)/2 * 1.15
    }
    hexC <- hexbin::hexcoords(dx, dy, n = 1)
    n <- nrow(data)
    hexdata <- data[rep(seq_len(n), each = 6), c("x", "y")]
    hexdata$x <- rep.int(hexC$x, n) + hexdata$x
    hexdata$y <- rep.int(hexC$y, n) + hexdata$y
    coords <- coord$transform(hexdata, panel_params)
    ggname("geom_hex",
           scribbleGrob(coords$x, coords$y,
              gp = grid::gpar(col = data$colour,
                        fill = ggplot2::fill_alpha(data$fill, data$alpha),
                        lwd = data$linewidth * ggplot2::.pt,
                        lty = data$linetype,
                        lineend = lineend, linejoin = linejoin,
                        linemitre = linemitre),
              scribblecolour = data$scribblecolour,
              scribblewidth = data$scribblewidth,
              scribbledensity = data$scribbledensity,
              wonkiness = data$wonkiness,
              wibbliness = data$wibbliness,
              sloppiness = data$sloppiness,
              randomness = data$randomness,
              angle = data$angle,
              res = res, default.units = "npc",
           pathId = rep(seq(n), each = 6)))
  })
