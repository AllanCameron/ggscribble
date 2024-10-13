#' Create a ggplot layer containing scribbled paths
#'
#' @inheritParams ggplot2::geom_line
#' @eval rd_aesthetics("geom", "scribblepath")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' t <- seq(5, -1, length.out = 1000) * pi
#'
#' spiral <- data.frame(x = sin(t) * 1:1000, y = cos(t) * 1:1000)
#'
#' ggplot(spiral, aes(x, y)) +
#'   geom_scribblepath(wonkiness = 1.5, wibbliness = 0.5) +
#'   ggplot2::coord_equal(xlim = c(-1500, 1500), ylim = c(-1500, 1500))

geom_scribblepath <- function (mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", ..., lineend = "butt",
                               linejoin = "round", linemitre = 10, arrow = NULL,
                               na.rm = FALSE, show.legend = NA, res = 200,
                               inherit.aes = TRUE) {

  layer(data = data, mapping = mapping, stat = stat,
        geom = GeomScribblepath, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list2(lineend = lineend, linejoin = linejoin,
                       linemitre = linemitre, arrow = arrow,
                       na.rm = na.rm, res = res, ...))
}


#' @rdname ggscribble-ggproto
#' @usage NULL
#' @format NULL
#' @export

GeomScribblepath <- ggproto("GeomScribblepath", ggplot2::GeomPath,

  default_aes = aes(colour = "black", linewidth = 1,
                             linetype = 1, alpha = NA,
                             wonkiness = 0, wibbliness = 1),

  draw_panel = function (self, data, panel_params, coord, arrow = NULL,
                         lineend = "butt", linejoin = "round", linemitre = 10,
                         res = 200, na.rm = FALSE) {

      if(!is.null(arrow)) {
        arrow <- NULL
        cli::cli_warn("Arrows are not implemented in scribbled paths")
      }

      if (is.null(data$linewidth) && !is.null(data$size)) {
        data$linewidth <- data$size
      }

      if (!anyDuplicated(data$group)) {
          cli::cli_inform(paste0(c("{.fn {snake_class(self)}}: ",
              "Each group consists of only one observation."),
              i = "Do you need to adjust the {.field group} aesthetic?"))
      }
      data <- data[order(data$group), , drop = FALSE]
      munched <- coord_munch(coord, data, panel_params)
      rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
      munched <- munched[rows >= 2, ]
      if (nrow(munched) < 2) return(zeroGrob())

      attr <- dapply(munched, "group", function(df) {
          linetype <- unique0(df$linetype)
          data_frame0(solid = identical(linetype, 1) || identical(linetype,
              "solid"), constant = nrow(unique0(df[, names(df) %in%
              c("alpha", "colour", "linewidth", "linetype")])) ==
              1, .size = 1)
      })
      solid_lines <- all(attr$solid)
      constant <- all(attr$constant)
      if (!solid_lines && !constant) {
          cli::cli_abort(paste0("{.fn {snake_class(self)}} ",
            "can't have varying {.field colour}, {.field linewidth}, ",
            "and/or {.field alpha} along the line when {.field linetype} ",
            "isn't solid."))
      }
      n <- nrow(munched)
      group_diff <- munched$group[-1] != munched$group[-n]
      start <- c(TRUE, group_diff)
      end <- c(group_diff, TRUE)
      if (!constant) {
          grobs <- grid::segmentsGrob(munched$x[!end], munched$y[!end],
                             munched$x[!start], munched$y[!start],
                             default.units = "native", arrow = arrow,
              gp = gpar(
                col = alpha(munched$colour, munched$alpha)[!end],
                fill = alpha(munched$colour, munched$alpha)[!end],
                lwd = munched$linewidth[!end] * .pt,
                lty = munched$linetype[!end],
                lineend = lineend, linejoin = linejoin,
                linemitre = linemitre))
          grobs <- wonkify(grobs, munched$wonkiness[!end])
          wibblify(grobs, munched$wibbliness[!end], res)
      }
      else {
          id <- match(munched$group, unique0(munched$group))
          grobs <- grid::polylineGrob(munched$x, munched$y, id = id,
                             default.units = "native",
              arrow = arrow, gp = gpar(
                col = alpha(munched$colour, munched$alpha)[start],
                fill = alpha(munched$colour, munched$alpha)[start],
                lwd = munched$linewidth[start] * .pt,
                lty = munched$linetype[start], lineend = lineend,
                linejoin = linejoin, linemitre = linemitre))
            grobs <- wonkify(grobs, munched$wonkiness[start])
            wibblify(grobs, munched$wibbliness[start], res)
      }
  })
