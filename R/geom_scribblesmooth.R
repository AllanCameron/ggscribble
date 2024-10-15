#' Create a ggplot layer containing scribble-filled conditional means
#'
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams geom_scribblearea
#' @eval rd_aesthetics("geom", "scribbleviolin")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' d <- data.frame(y = 1:10,
#'                 x = c(1.43, 1.95, -0.33, -0.74, 2.48,
#'                       3.9, 3.79, 6.39, 5.11, 8.09))
#'
#' ggplot(d, aes(x, y)) +
#'   geom_scribblesmooth(angle = 120, color = "gray50", scribblecolor = "gray",
#'   randomness = 2) +
#'   geom_point() +
#'   theme_classic(16)

geom_scribblesmooth <- function (mapping = NULL, data = NULL, stat = "smooth",
                                 position = "identity", ..., method = NULL,
                                 formula = NULL, se = TRUE, na.rm = FALSE,
                                 orientation = NA, show.legend = NA,
                                 inherit.aes = TRUE, res = 200) {

  params <- list2(na.rm = na.rm, orientation = orientation,
                  se = se, res = res, ...)

  if (identical(stat, "smooth")) {
    params$method <- method
    params$formula <- formula
  }

  layer(data = data, mapping = mapping, stat = stat,
        geom = GeomScribblesmooth, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = params)
}


#' @rdname ggscribble-ggproto
#' @usage NULL
#' @format NULL
#' @export

GeomScribblesmooth <- ggproto("GeomScribblesmooth",

  ggplot2::GeomSmooth,

  default_aes = aes(colour = "black", fill = NA, linewidth = 1,
                             linetype = 1, alpha = NA, subgroup = NULL,
                             scribblecolour = "black", scribblewidth = 1,
                             wonkiness = 0, wibbliness = 1, randomness = 1,
                             sloppiness = 1, scribbledensity = 200, angle = 45),

  draw_panel = function (self, data, panel_params, coord, ..., res = 200) {
    groups <- split(data, factor(data$group))
    grobs <- lapply(groups, function(group) {
      self$draw_group(group, panel_params, coord, ..., res = res)
    })
    ggname("geom_scribblesmooth",
           grid::gTree(children = rlang::inject(grid::gList(!!!grobs))))
  },

  draw_group = function (data, panel_params, coord, lineend = "butt",
                         linejoin = "round", linemitre = 10, se = FALSE,
                         flipped_aes = FALSE, res = 200) {

    ribbon <- transform(data, colour = NA)
    path <- transform(data, alpha = NA)
    ymin = ggplot2::flipped_names(flipped_aes)$ymin
    ymax = ggplot2::flipped_names(flipped_aes)$ymax
    has_ribbon <- se && !is.null(data[[ymax]]) && !is.null(data[[ymin]])
    grid::gList(if (has_ribbon)
      GeomScribbleribbon$draw_group(ribbon, panel_params, coord,
                                    flipped_aes = flipped_aes, res = res),
      GeomScribbleline$draw_panel(path, panel_params, coord, lineend = lineend,
                                  linejoin = linejoin, linemitre = linemitre,
                                  res = res))
  })
