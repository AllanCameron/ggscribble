#' Create a ggplot layer containing scribble-filled bars
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
#' @param orientation The orientation of the layer. The default (`NA`)
#'   automatically determines the orientation from the aesthetic mapping. In the
#'   rare event that this fails it can be given explicitly by setting
#'   orientation to either "x" or "y".
#' @param just Adjustment for column placement. Set to 0.5 by default, meaning
#'   that columns will be centered about axis breaks. Set to 0 or 1 to place
#'   columns to the left/right of axis breaks. Note that this argument may have
#'   unintended behaviour when used with alternative positions,
#'   e.g. `position_dodge()`.
#' @param width Bar width. By default, set to 90% of the `resolution()` of
#'   the data.
#'
#' @return NULL
#' @export
#'
#' @examples
#' ggplot(iris[c(1, 51, 101), ],
#'       aes(Species, Sepal.Length, scribblecolour = Species)) +
#'   geom_scribblecol()
#'
geom_scribblebar <- function (mapping = NULL, data = NULL, stat = "count",
                              position = "stack", ..., just = 0.5, width = NULL,
                              na.rm = FALSE, orientation = NA, show.legend = NA,
                              inherit.aes = TRUE) {

  layer(data = data, mapping = mapping, stat = stat, geom = GeomScribbleBar,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(just = just, width = width, na.rm = na.rm,
                              orientation = orientation, ...))
}

#' @rdname geom_scribblebar
#' @export
geom_scribblecol <- function (mapping = NULL, data = NULL, stat = "identity",
                              position = "stack", ..., just = 0.5, width = NULL,
                              na.rm = FALSE, orientation = NA, show.legend = NA,
                              inherit.aes = TRUE) {

  layer(data = data, mapping = mapping, stat = stat, geom = GeomScribbleCol,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(just = just, width = width, na.rm = na.rm,
                              orientation = orientation, ...))
}



#' The ggproto object that powers scribble-filled bar plots.
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribbleBar <- ggplot2::ggproto("GeomScribbleBar", ggplot2::GeomBar,

  default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 1,
                linetype = 1, alpha = NA, subgroup = NULL,
                scribblecolour = "black", scribble_lwd = 1,
                wonkiness = 1, wibbliness = 1, randomness = 1,
                neatness = 1, density = 200, angle = 45),

  draw_panel = function (self, data, panel_params, coord, lineend = "butt",
                         linejoin = "mitre", width = NULL,
                         flipped_aes = FALSE) {

  ggplot2::ggproto_parent(GeomScribbleRect, self)$draw_panel(data, panel_params,
      coord, lineend = lineend, linejoin = linejoin)

  }
)

#' The ggproto object that powers scribble-filled bar plots.
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribbleCol <- ggplot2::ggproto("GeomScribbleCol",
                                    GeomScribbleBar,

  default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 1,
                linetype = 1, alpha = NA, subgroup = NULL,
                scribblecolour = "black", scribble_lwd = 1,
                wonkiness = 1, wibbliness = 1, randomness = 1,
                neatness = 1, density = 200, angle = 45),

  draw_panel = function (self, data, panel_params, coord, lineend = "butt",
                         linejoin = "mitre", width = NULL,
                         flipped_aes = FALSE) {

    ggplot2::ggproto_parent(GeomScribbleBar, self)$draw_panel(data,
                    panel_params, coord, lineend = lineend, linejoin = linejoin)

  }
)
