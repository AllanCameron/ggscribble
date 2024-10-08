#' Create a ggplot layer containing scribbled point ranges
#'
#' @inheritParams ggplot2::geom_pointrange
#' @eval rd_aesthetics("geom", "scribblepointrange")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' d <- data.frame(x = c("A", "B", "C"),
#'                 y = 1:3, ymin = 0:2, ymax = 2:4)
#'
#' ggplot2::ggplot(d, ggplot2::aes(x, y, ymin = ymin, ymax = ymax)) +
#'   geom_scribblepointrange() +
#'   ggplot2::theme_classic(16)

geom_scribblepointrange <- function (mapping = NULL, data = NULL,
                                     stat = "identity", position = "identity",
                                     ..., fatten = 2, na.rm = FALSE,
                                     orientation = NA, show.legend = NA,
                                     inherit.aes = TRUE, res = 200) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblepointrange, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(fatten = fatten, na.rm = na.rm,
                                       orientation = orientation,
                                       res = res, ...))
}


#' The ggproto object that powers scribble-filled pointranges
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export

GeomScribblepointrange <- ggplot2::ggproto("GeomScribblepointrange",

  ggplot2::GeomPointrange,

  default_aes = ggplot2::aes(colour = "black",
                              linewidth = 1,
                              linetype = 1,
                              alpha = NA,
                              wonkiness = 0,
                              wibbliness = 1,
                              size = 1.5),

  draw_panel = function (data, panel_params, coord, lineend = "butt",
                         fatten = 2, flipped_aes = FALSE, na.rm = FALSE,
                         res = 200) {

      line_grob <- GeomScribblelinerange$draw_panel(data, panel_params,
          coord, lineend = lineend, flipped_aes = flipped_aes,
          na.rm = na.rm, res = res)

      if (is.null(data[[ggplot2::flipped_names(flipped_aes)$y]]))
        return(line_grob)

      ggname("geom_scribblepointrange",
             grid::gTree(children = grid::gList(line_grob,
          GeomScribblepoint$draw_panel(transform(data, size = size * fatten),
              panel_params, coord, na.rm = na.rm))))
  }
)
