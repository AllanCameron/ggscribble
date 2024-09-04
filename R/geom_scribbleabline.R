#' Scribbled reference lines: horizontal, vertical, and diagonal
#'
#' These geoms add reference lines (sometimes called rules) to a plot, either
#' horizontal, vertical, or diagonal (specified by slope and intercept).
#' These are useful for annotating plots.
#'
#' These geoms act slightly differently from other geoms. You can supply the
#' parameters in two ways: either as arguments to the layer function,
#' or via aesthetics. If you use arguments, e.g.
#' `geom_abline(intercept = 0, slope = 1)`, then behind the scenes
#' the geom makes a new data frame containing just the data you've supplied.
#' That means that the lines will be the same in all facets; if you want them
#' to vary across facets, construct the data frame yourself and use aesthetics.
#'
#' Unlike most other geoms, these geoms do not inherit aesthetics from the plot
#' default, because they do not understand x and y aesthetics which are
#' commonly set in the plot. They also do not affect the x and y scales.
#'
#' @section Aesthetics:
#' These geoms are drawn using \code{\link{geom_scribbleline}} so they support
#' the same aesthetics: \code{alpha}, \code{colour}, \code{linetype} and
#' \code{linewidth}. They also each have aesthetics that control the position of
#' the line:
#'
#'   - \code{geom_scribblevline()}: \code{xintercept}
#'
#'   - \code{geom_scribblehline()}: \code{yintercept}
#'
#'   - \code{geom_scribbleabline()}: \code{slope} and \code{intercept}
#'
#' @seealso See \code{\link{geom_scribblesegment}} for a more general approach
#' to adding scribbled line segments to a plot.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @name Scribbled-reference-lines
#' @param mapping Set of aesthetic mappings created by
#' \link[ggplot2]{aes}.
#' @param xintercept,yintercept,slope,intercept Parameters that control the
#'   position of the line. If these are set, `data`, `mapping` and
#'   `show.legend` are overridden.
#' @param res The number of points into which the line will be "wibbled"
#' @export
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#'
#' # Fixed values
#' p + geom_scribblevline(xintercept = 5)
#' p + geom_scribblevline(xintercept = 1:5)
#' p + geom_scribblehline(yintercept = 20)
#'
#' p + geom_scribbleabline() # Can't see it - outside the range of the data
#' p + geom_scribbleabline(intercept = 20)
#'
#' # Calculate slope and intercept of line of best fit
#' coef(lm(mpg ~ wt, data = mtcars))
#' p + geom_scribbleabline(intercept = 37, slope = -5)
#'
#' # To show different lines in different facets, use aesthetics
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
#'   ggplot2::geom_point() +
#'   ggplot2::facet_wrap(~ cyl)
#'
#' mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
#' p + geom_scribblehline(ggplot2::aes(yintercept = wt), mean_wt)
#'
#' # You can also control other aesthetics
#' ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt, colour = wt)) +
#'   ggplot2::geom_point() +
#'   geom_scribblehline(ggplot2::aes(yintercept = wt, colour = wt), mean_wt) +
#'   ggplot2::facet_wrap(~ cyl)

geom_scribbleabline <- function (mapping = NULL, data = NULL, ..., slope,
                                 intercept,  na.rm = FALSE, show.legend = NA,
                                 res = 100) {

  if (is.null(mapping) && missing(slope) && missing(intercept)) {
    slope <- 1
    intercept <- 0
  }
  if (!missing(slope) || !missing(intercept)) {
    if (!is.null(mapping)) {
      cli::cli_warn(paste0("{.fn geom_scribbleabline}: Ignoring {.arg mapping}",
                           " because {.arg slope} and/or {.arg intercept}",
                           " were provided."))
    }
    if (!is.null(data)) {
      cli::cli_warn(paste0("{.fn geom_scribbleabline}: Ignoring {.arg data} ",
                           "because {.arg slope} and/or {.arg intercept} ",
                           "were provided."))
    }
    if (missing(slope)) slope <- 1
    if (missing(intercept)) intercept <- 0
    n_slopes <- max(length(slope), length(intercept))
    data <- data_frame0(intercept = intercept, slope = slope,
                        .size = n_slopes)
    mapping <- aes(intercept = intercept, slope = slope)
    show.legend <- FALSE
  }
  ggplot2::layer(data = data, mapping = mapping, stat = StatIdentity,
        geom = GeomScribbleabline, position = PositionIdentity,
        show.legend = show.legend, inherit.aes = FALSE,
        params = rlang::list2(na.rm = na.rm, res = res, ...))
}

#' The ggproto object that powers scribbled AB lines
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export

GeomScribbleabline <- ggplot2::ggproto("GeomScribbleabline",
                                       ggplot2::GeomAbline,

  default_aes = ggplot2::aes(colour = "black", linewidth = 1,
                         linetype = 1, alpha = NA,
                         wonkiness = 0, wibbliness = 1),

  draw_panel = function (data, panel_params, coord,
                         lineend = "butt", res = 100) {

    ranges <- coord$backtransform_range(panel_params)
    if (coord$clip == "on" && coord$is_linear()) {
      ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
    }
    data$x <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y <- ranges$x[1] * data$slope + data$intercept
    data$yend <- ranges$x[2] * data$slope + data$intercept
    GeomScribblesegment$draw_panel(unique0(data), panel_params, coord,
                                    lineend = lineend, res = res)
  }
)
