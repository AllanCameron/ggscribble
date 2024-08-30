
#' Create a ggplot layer containing scribble-filled polygons
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
#' ggplot2::ggplot(data = data.frame(x = c(5, 10, 7.5, 10, 15, 12.5),
#'                                   y = c(5, 5, 10, 5, 5, 10),
#'                                   g = rep(1:2, each = 3)),
#'                 mapping = ggplot2::aes(x, y, group = g)) +
#'  geom_scribblepolygon()
#'
geom_scribblepolygon <- function (mapping = NULL, data = NULL,
                                  stat = "identity", position = "identity", ...,
                                  linejoin = "mitre", na.rm = FALSE,
                                  show.legend = NA, inherit.aes = TRUE) {

  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomScribblePolygon,
                 position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = rlang::list2(linejoin = linejoin, na.rm = na.rm, ...))
}



#' The ggproto object that powers scribble-filled shapes.
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export

GeomScribblePolygon <- ggplot2::ggproto("GeomScribblePolygon",
    ggplot2::GeomPolygon,

    default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 1,
                      linetype = 1, alpha = NA, subgroup = NULL,
                      scribblecolour = "black", scribblewidth = 1,
                      wonkiness = 1, wibbliness = 1, randomness = 1,
                      neatness = 1, density = 200, angle = 45),

    draw_panel = function (self, data, panel_params, coord,
                           lineend = "butt", linejoin = "round",
                           linemitre = 10) {

      if (is.null(data$linewidth) && !is.null(data$size)) {
        data$linewidth <- data$size
      }
      n <- nrow(data)
      if (n == 1) return(ggplot2::zeroGrob())

      munched <- ggplot2::coord_munch(coord, data, panel_params,
                                      is_closed = TRUE)

      if (!is.null(munched$subgroup)) {
        cli::cli_abort("Scribble grob does not support polygons with holes")
      }

      munched    <- munched[order(munched$group), ]
      first_idx  <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]

      scribbleGrob(x = munched$x, y = munched$y,
          default.units = "npc", id = munched$group,
          scribblecolour = first_rows$scribblecolour,
          scribblewidth = first_rows$scribblewidth,
          density = first_rows$density,
          wonkiness = first_rows$wonkiness,
          wibbliness = first_rows$wibbliness,
          neatness = first_rows$neatness,
          angle = first_rows$angle,
          gp = grid::gpar(col = first_rows$colour,
              fill = ggplot2::fill_alpha(first_rows$fill, first_rows$alpha),
              lwd = first_rows$linewidth * ggplot2::.pt,
              lty = first_rows$linetype,
              lineend = lineend, linejoin = linejoin, linemitre = linemitre))
  }
)
