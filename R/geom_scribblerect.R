
#' Create a ggplot layer containing scribble-filled rectangles
#'
#' @param mapping Set of aesthetic mappings created by `aes()`. If specified and
#'   `inherit.aes = TRUE` (the default), it is combined with the default mapping
#'   at the top level of the plot. You must supply `mapping` if there is no plot
#'   mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to `ggplot()`.
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    `fortify()` for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data. A `function` can be created
#'    from a `formula` (e.g. `~ head(.x, 10)`).
#' @param stat The statistical transformation to use on the data for this layer,
#'   either as a `ggproto` `Geom` subclass or as a string naming the stat
#'   stripped of the `stat_` prefix (e.g. "count" rather than `"stat_count"`)
#' @param position Position adjustment, either as a string naming the adjustment
#'   (e.g. "jitter" to use position_jitter), or the result of a call to a
#'   position adjustment function. Use the latter if you need to change the
#'   settings of the adjustment.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display. To include legend keys for all levels, even
#'   when no data exists, use `TRUE`.  If `NA`, all levels are shown in legend,
#'   but unobserved levels are omitted.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. `borders()`.
#' @param na.rm If FALSE, the default, missing values are removed with a
#'   warning. If TRUE, missing values are silently removed.
#' @param ... Other arguments passed to `layer()`
#'
#' @return NULL
#' @export
#'
#' @examples
#' ggplot2::ggplot(data = data.frame(xmin = c(5, 10), xmax = c(15, 20),
#'                                   ymin = c(5, 10), ymax = c(10, 20),
#'                                   g = 1:2),
#'                 mapping = ggplot2::aes(xmin = xmin, xmax = xmax,
#'                                        ymin = ymin, ymax = ymax,
#'                                        group = g)) +
#'  geom_scribblerect()
#'
geom_scribblerect <- function (mapping = NULL, data = NULL, stat = "identity",
          position = "identity", ..., linejoin = "mitre",
          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribbleRect, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(linejoin = linejoin, na.rm = na.rm, ...))
}

#' The ggproto object that powers scribble-filled rectangles.
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribbleRect <- ggplot2::ggproto("GeomScribbleRect", ggplot2::GeomRect,
      default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 1,
                      linetype = 1, alpha = NA, subgroup = NULL,
                      scribble_col = "black", scribble_lwd = 1,
                      wonkiness = 1, wibbliness = 1, randomness = 1,
                      neatness = 1, density = 200, angle = 46),
  draw_panel = function (self, data, panel_params, coord, lineend = "butt",
                         linejoin = "mitre") {

    if (is.null(data$linewidth) && !is.null(data$size)) {
      data$linewidth <- data$size
    }
    aesthetics <- setdiff(names(data), c("x", "y", "xmin",
        "xmax", "ymin", "ymax"))
    index <- rep(seq_len(nrow(data)), each = 4)
    new <- data[index, aesthetics, drop = FALSE]
    new$x <- vctrs::vec_interleave(data$xmin, data$xmax, data$xmax,
        data$xmin)
    new$y <- vctrs::vec_interleave(data$ymax, data$ymax, data$ymin,
        data$ymin)
    new$group <- index
    grob <- GeomScribblePolygon$draw_panel(new, panel_params,
        coord, lineend = lineend, linejoin = linejoin)
    grob$name <- grid::grobName(grob, "geom_scribblerect")
    grob
})
