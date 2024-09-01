# geom_scribblelinerange and its ggproto ---------------------------------------

#' Create a ggplot layer containing scribbled line ranges
#'
#' @inheritParams ggplot2::geom_linerange
#' @eval rd_aesthetics("geom", "scribblelinerange")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' ggplot2::ggplot(data = data.frame(x = c(1, 3), x1 = c(2, 5), y = c(5, 2),
#'                                   g = c("A", "B")),
#'                 mapping = ggplot2::aes(x, y, colour = g)) +
#'   geom_scribblelinerange(aes(xmin = x, xmax = x1), wibbliness = 2)

geom_scribblelinerange <- function(mapping = NULL, data = NULL,
                                   stat = "identity", orientation = NA,
                                   position = "identity", na.rm = FALSE,
                                   res = 200, show.legend = NA,
                                   inherit.aes = TRUE, ...) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblelinerange,
                 position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = rlang::list2(na.rm = na.rm, res = res,
                                       orientation = orientation, ...))
}

#' The ggproto object that powers scribbled lineranges
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribblelinerange <- ggplot2::ggproto("GeomScribblelinerange",
                                          ggplot2::GeomLinerange,

    default_aes = ggplot2::aes(colour = "black", linewidth = 1,
                             linetype = 1, alpha = NA,
                             wonkiness = 0, wibbliness = 1),

    draw_panel = function (data, panel_params, coord, lineend = "butt",
                           flipped_aes = FALSE, na.rm = FALSE, res = 200) {
        data <- ggplot2::flip_data(data, flipped_aes)
        data <- transform(data, xend = x, y = ymin, yend = ymax)
        data <- ggplot2::flip_data(data, flipped_aes)
        ggname("geom_scribblelinerange", GeomScribblesegment$draw_panel(data,
            panel_params, coord, lineend = lineend, na.rm = na.rm, res = res))
    }
)

