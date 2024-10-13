#' @rdname Scribbled-reference-lines
#' @export

geom_scribblehline <- function (mapping = NULL, data = NULL, ...,
                                yintercept, na.rm = FALSE, res = 100,
                                show.legend = NA)
{
  if (!missing(yintercept)) {
    if (!is.null(mapping)) {
      cli::cli_warn(paste0("{.fn geom_hline}: Ignoring {.arg mapping} ",
                           "because {.arg yintercept} was provided."))
    }
    if (!is.null(data)) {
      cli::cli_warn(paste0("{.fn geom_hline}: Ignoring {.arg data} ",
                           "because {.arg yintercept} was provided."))
    }
    data <- data_frame0(yintercept = yintercept)
    mapping <- aes(yintercept = yintercept)
    show.legend <- FALSE
  }
  layer(data = data, mapping = mapping, stat = ggplot2::StatIdentity,
        geom = GeomScribblehline, position = ggplot2::PositionIdentity,
        show.legend = show.legend, inherit.aes = FALSE,
        params = list2(na.rm = na.rm, res = res, ...))
}


#' @rdname ggscribble-ggproto
#' @usage NULL
#' @format NULL
#' @export

GeomScribblehline <- ggproto("GeomScribblehline", ggplot2::GeomHline,

   default_aes = aes(colour = "black", linewidth = 1,
                         linetype = 1, alpha = NA,
                         wonkiness = 0, wibbliness = 1),

   draw_panel = function (data, panel_params, coord,
                          lineend = "butt", res = 100) {

     ranges <- coord$backtransform_range(panel_params)
     data$x <- ranges$x[1]
     data$xend <- ranges$x[2]
     data$y <- data$yintercept
     data$yend <- data$yintercept
     GeomScribblesegment$draw_panel(unique0(data), panel_params, coord,
                                    lineend = lineend, res = res)
   }
)
