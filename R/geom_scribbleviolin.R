#' Create a ggplot layer containing scribble-filled violin plots
#'
#' @inheritParams ggplot2::geom_violin
#' @inheritParams geom_scribblearea
#' @eval rd_aesthetics("geom", "scribbleviolin")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(Species, Sepal.Length)) +
#'   geom_scribbleviolin() +
#'   theme_classic(16)

geom_scribbleviolin <- function (mapping = NULL, data = NULL, stat = "ydensity",
                                 position = "dodge", ..., draw_quantiles = NULL,
                                 trim = TRUE, bounds = c(-Inf, Inf),
                                 scale = "area", na.rm = FALSE,
                                 orientation = NA, show.legend = NA,
                                 inherit.aes = TRUE, res = 200) {

  layer(data = data, mapping = mapping, stat = stat,
        geom = GeomScribbleviolin, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list2(trim = trim, scale = scale,
                       draw_quantiles = draw_quantiles,
                       na.rm = na.rm, orientation = orientation,
                       bounds = bounds, res = res, ...))
}


#' @rdname ggscribble-ggproto
#' @usage NULL
#' @format NULL
#' @export

GeomScribbleviolin <- ggproto("GeomScribbleviolin",

  ggplot2::GeomViolin,

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
    ggname("geom_scribbleviolin",
           grid::gTree(children = rlang::inject(grid::gList(!!!grobs))))
  },

  draw_group = function (self, data, ..., res = 200, draw_quantiles = NULL,
                         flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    data <- transform(data, xminv = x - violinwidth * (x - xmin),
                      xmaxv = x + violinwidth * (xmax - x))
    newdata <- vctrs::vec_rbind(transform(data, x = xminv)[order(data$y), ],
                                transform(data, x = xmaxv)[order(data$y,
                                                           decreasing = TRUE),])
    newdata <- vctrs::vec_rbind(newdata, newdata[1, ])
    newdata <- flip_data(newdata, flipped_aes)
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      if (!(all(draw_quantiles >= 0) && all(draw_quantiles <= 1))) {
        cli::cli_abort("{.arg draw_quantiles} must be between 0 and 1.")
      }
      quantiles <- create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)),
                         setdiff(names(data),
                                 c("x", "y", "group")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- vctrs::vec_cbind(quantiles, aesthetics)
      both <- both[!is.na(both$group), , drop = FALSE]
      both <- flip_data(both, flipped_aes)
      quantile_grob <- if (nrow(both) == 0) {
        zeroGrob()
      } else {
        GeomScribblepath$draw_panel(both, ..., res = res)
      }

      ggname("geom_violin",
        grid::grobTree(GeomScribblepolygon$draw_panel(newdata, ..., res = res),
        quantile_grob))
    } else {
      ggname("geom_violin",
             GeomScribblepolygon$draw_panel(newdata, ..., res = res))
    }
  })
