#' Create a ggplot layer containing scribble-filled boxplots
#'
#' @inheritParams ggplot2::geom_boxplot
#' @eval rd_aesthetics("geom", "scribbleboxplot")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Width)) +
#'   geom_scribbleboxplot(ggplot2::aes(scribblecolour = Species),
#'   staplewidth = 0.4) +
#'   ggplot2::theme_classic(16)
geom_scribbleboxplot <- function (mapping = NULL, data = NULL, stat = "boxplot",
                                  position = "dodge2", ..., outliers = TRUE,
                                  outlier.colour = NULL, outlier.color = NULL,
                                  outlier.fill = NULL, outlier.shape = 19,
                                  outlier.size = 1.5, outlier.stroke = 0.5,
                                  outlier.alpha = NULL, notch = FALSE,
                                  notchwidth = 0.5, staplewidth = 0,
                                  varwidth = FALSE, na.rm = FALSE,
                                  orientation = NA, show.legend = NA,
                                  inherit.aes = TRUE, res = 200) {

  if (is.character(position)) {
    if (varwidth == TRUE)
      position <- ggplot2::position_dodge2(preserve = "single")
  }
  else {
    if (identical(position$preserve, "total") & varwidth ==
        TRUE) {
      cli::cli_warn("Can't preserve total widths when {.code varwidth = TRUE}.")
      position$preserve <- "single"
    }
  }
  if(!is.numeric(staplewidth))
    stop("Argument \"staplewidth\" must be numeric")
  if(!is.logical(outliers))
    stop("Argument \"outliers\" must be TRUE or FALSE")
  ouliers <- outliers[1]

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribbleboxplot, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(outliers = outliers,
                                       outlier.colour = outlier.color %||%
                                                        outlier.colour,
                                       outlier.fill = outlier.fill,
                                       outlier.shape = outlier.shape,
                                       outlier.size = outlier.size,
                                       outlier.stroke = outlier.stroke,
                                       outlier.alpha = outlier.alpha,
                                       notch = notch, res = res,
                                       notchwidth = notchwidth,
                                       staplewidth = staplewidth,
                                       varwidth = varwidth, na.rm = na.rm,
                                       orientation = orientation, ...))
}

#' The ggproto object that powers scribbled boxplots
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribbleboxplot <- ggplot2::ggproto("GeomScribbleboxplot",

  ggplot2::GeomBoxplot,

  default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 0.5,
                             weight = 1,
                             alpha = NA, shape = 19, size = NULL,
                             scribblecolour = "black", scribblewidth = 1,
                             wonkiness = 1, wibbliness = 1, randomness = 1,
                             sloppiness = 1, scribbledensity = 200, angle = 45),

  draw_group = function (self, data, panel_params, coord, lineend = "round",
                         linejoin = "mitre", fatten = 2, outlier.colour = NULL,
                         outlier.fill = NULL, outlier.shape = 19,
                         outlier.size = 1.5, outlier.stroke = 0.5,
                         outlier.alpha = NULL, notch = FALSE, notchwidth = 0.5,
                         staplewidth = 0, varwidth = FALSE,
                         flipped_aes = FALSE, res = 200) {

    if (is.null(data$linewidth) && !is.null(data$size)) {
      data$linewidth <- data$size
    }

    data <- ggplot2::flip_data(data, flipped_aes)
    if (nrow(data) != 1) {
        cli::cli_abort(c("Can only draw one boxplot per group.",
            i = "Did you forget {.code aes(group = ...)}?"))
    }
    common <- list(colour = data$colour,
                   linewidth = data$linewidth,
                   linetype = data$linetype,
                   wonkiness = data$wonkiness,
                   fill = ggplot2::fill_alpha(data$fill, data$alpha),
                   group = data$group)

    whiskers <- data_frame0(x = c(data$x, data$x),
                            xend = c(data$x, data$x),
                            y = c(data$upper, data$lower),
                            yend = c(data$ymax, data$ymin),
                            alpha = c(NA_real_, NA_real_),
                            wibbliness = data$wibbliness * 2,
                            !!!common,
                            .size = 2)
    whiskers <- ggplot2::flip_data(whiskers, flipped_aes)
    box <- data_frame0(xmin = data$xmin, xmax = data$xmax, ymin = data$lower,
                       y = data$middle, ymax = data$upper,
                       ynotchlower = ifelse(notch, data$notchlower, NA),
                       ynotchupper = ifelse(notch, data$notchupper, NA),
                       notchwidth = notchwidth, alpha = data$alpha,
                       scribblewidth = data$scribblewidth,
                       angle = data$angle,
                       wibbliness = data$wibbliness,
                       scribbledensity = data$scribbledensity,
                       scribblecolour = data$scribblecolour,
                       randomness = data$randomness,
                       sloppiness = data$sloppiness,
                       !!!common)
    box <- ggplot2::flip_data(box, flipped_aes)

    if (!is.null(data$outliers) && length(data$outliers[[1]]) >= 1) {
        outliers <- data_frame0(y = data$outliers[[1]],
                                x = data$x[1],
                                colour = outlier.colour %||% data$colour[1],
                                size = outlier.size %||% data$size[1],
                                alpha = outlier.alpha %||% data$alpha[1],
                                .size = length(data$outliers[[1]]))
        outliers <- ggplot2::flip_data(outliers, flipped_aes)
        outliers_grob <- GeomScribblepoint$draw_panel(outliers, panel_params,
                            coord)
    }
    else {
        outliers_grob <- NULL
    }
    if (staplewidth != 0) {
        staples <- data_frame0(x = rep((data$xmin - data$x) *
                                        staplewidth + data$x, 2),
                               xend = rep((data$xmax - data$x) *
                                            staplewidth + data$x, 2),
                               y = c(data$ymax, data$ymin),
                               yend = c(data$ymax, data$ymin),
                               alpha = c(NA_real_, NA_real_),
                               wibbliness = data$wibbliness * 2,
                               !!!common,
                               .size = 2)
        staples <- ggplot2::flip_data(staples, flipped_aes)
        staples$wonkiness <- staples$wonkiness * 4
        staple_grob <- GeomScribblesegment$draw_panel(staples, panel_params,
                          coord, lineend = lineend, res = res * 2)
    }
    else {
        staple_grob <- NULL
    }
    ggname("geom_boxplot", grid::grobTree(outliers_grob, staple_grob,
        GeomScribblesegment$draw_panel(whiskers, panel_params, coord,
            lineend = lineend, res = res * 2),
            GeomScribblecrossbar$draw_panel(box,
            fatten = fatten, panel_params, coord, lineend = lineend,
            linejoin = linejoin, flipped_aes = flipped_aes, res = res)))
})
