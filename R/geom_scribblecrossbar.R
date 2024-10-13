#' Create a ggplot layer containing scribble-filled crossbars
#'
#' @inheritParams ggplot2::geom_crossbar
#' @eval rd_aesthetics("geom", "scribblecrossbar")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#'
#' @examples
#' df <- data.frame(trt = factor(c(1, 1, 2, 2)),
#'                  resp = c(1, 5, 3, 4),
#'                  group = factor(c(1, 2, 1, 2)),
#'                  upper = c(1.1, 5.3, 3.3, 4.2),
#'                  lower = c(0.8, 4.6, 2.4, 3.6))
#'
#' ggplot(df, aes(trt, resp, colour = group)) +
#'   geom_scribblecrossbar(aes(ymin = lower, ymax = upper),
#'                         width = 0.2) +
#'   theme_classic(16)
#'

geom_scribblecrossbar <- function (mapping = NULL, data = NULL,
                                   stat = "identity", position = "identity",
                                   ..., fatten = 2.5, na.rm = FALSE,
                                   orientation = NA, show.legend = NA,
                                   inherit.aes = TRUE, res = 200) {

  layer(data = data, mapping = mapping, stat = stat,
        geom = GeomScribblecrossbar, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list2(fatten = fatten, na.rm = na.rm,
                       orientation = orientation, res = res, ...))
}


#' @rdname ggscribble-ggproto
#' @usage NULL
#' @format NULL
#' @export

GeomScribblecrossbar <- ggproto("GeomScribblecrossbar",

  ggplot2::GeomCrossbar,

  default_aes = aes(colour = "black", fill = NA, linewidth = 0.5,
                             alpha = NA,
                             scribblecolour = "black", scribblewidth = 1,
                             wonkiness = 0, wibbliness = 1, randomness = 1,
                             sloppiness = 1, scribbledensity = 200, angle = 45),

  draw_panel = function (self, data, panel_params, coord, lineend = "butt",
                         linejoin = "mitre", fatten = 2.5, width = NULL,
                         flipped_aes = FALSE, res = 200) {

    if (is.null(data$linewidth) && !is.null(data$size)) {
      data$linewidth <- data$size
    }
    data <- ggplot2::flip_data(data, flipped_aes)
    middle <- transform(data, x = xmin, xend = xmax, yend = y,
        linewidth = linewidth * fatten, alpha = NA)
    has_notch <- !is.null(data$ynotchlower) && !is.null(data$ynotchupper) &&
        !is.na(data$ynotchlower) && !is.na(data$ynotchupper)
    if (has_notch) {
        if (data$ynotchlower < data$ymin || data$ynotchupper >
            data$ymax)
            cli::cli_inform(c("Notch went outside hinges",
                              i = "Do you want {.code notch = FALSE}?"))

        notchindent <- (1 - data$notchwidth) * (data$xmax - data$xmin)/2
        middle$x <- middle$x + notchindent
        middle$xend <- middle$xend - notchindent
        box <- data_frame0(x = c(data$xmin, data$xmin, data$xmin + notchindent,
                                 data$xmin, data$xmin, data$xmax, data$xmax,
                                 data$xmax - notchindent, data$xmax, data$xmax,
                                 data$xmin),
                           y = c(data$ymax, data$ynotchupper, data$y,
                                 data$ynotchlower, data$ymin, data$ymin,
                                 data$ynotchlower, data$y, data$ynotchupper,
                                 data$ymax, data$ymax),
                           alpha = rep(data$alpha, 11),
                           colour = rep(data$colour, 11),
                           linewidth = rep(data$linewidth, 11),
                           linetype = rep(data$linetype, 11),
                           fill = rep(data$fill, 11),
                           scribblecolour = rep(data$scribblecolour, 11),
                           scribblewidth = rep(data$scribblewidth, 11),
                           wonkiness = rep(data$wonkiness, 11),
                           wibbliness = rep(data$wibbliness, 11),
                           randomness = rep(data$randomness, 11),
                           sloppiness = rep(data$sloppiness, 11),
                           randomness = rep(data$randomness, 11),
                           scribbledensity = rep(data$scribbledensity, 11),
                           angle = rep(data$angle, 11),
                           group = rep(seq_len(nrow(data)), 11))
    }
    else {
        box <- data_frame0(x = c(data$xmin, data$xmin, data$xmax,
                                 data$xmax, data$xmin),
                           y = c(data$ymax, data$ymin, data$ymin,
                                 data$ymax, data$ymax),
                           alpha = rep(data$alpha, 5),
                           colour = rep(data$colour, 5),
                           linewidth = rep(data$linewidth, 5),
                           linetype = rep(data$linetype, 5),
                           fill = rep(data$fill, 5),
                           scribblecolour = rep(data$scribblecolour, 5),
                           scribblewidth = rep(data$scribblewidth, 5),
                           wonkiness = rep(data$wonkiness, 5),
                           wibbliness = rep(data$wibbliness, 5),
                           randomness = rep(data$randomness, 5),
                           sloppiness = rep(data$sloppiness, 5),
                           randomness = rep(data$randomness, 5),
                           scribbledensity = rep(data$scribbledensity, 5),
                           angle = rep(data$angle, 5),
                           group = rep(seq_len(nrow(data)), 5))
    }

    box <- ggplot2::flip_data(box, flipped_aes)
    middle <- ggplot2::flip_data(middle, flipped_aes)
    ggname("geom_scribblecrossbar",
           grid::gTree(children = grid::gList(GeomScribblepolygon$draw_panel(box,
              panel_params, coord, lineend = lineend, linejoin = linejoin,
              res = res),
        GeomScribblesegment$draw_panel(middle, panel_params, coord,
                                       lineend = lineend, linejoin = linejoin,
                                       res = res))))
})
