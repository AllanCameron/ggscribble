#' @rdname Scribbled-reference-lines
#' @export

geom_scribblevline <- function (mapping = NULL, data = NULL, ...,
                                xintercept, na.rm = FALSE,
                                show.legend = NA, res = 100) {
  if (!missing(xintercept)) {
    if (!is.null(mapping)) {
      cli::cli_warn(paste0("{.fn geom_vline}: Ignoring {.arg mapping} ",
                           "because {.arg xintercept} was provided."))
    }
    if (!is.null(data)) {
      cli::cli_warn(paste0("{.fn geom_vline}: Ignoring {.arg data} ",
                           "because {.arg xintercept} was provided."))
    }
    data <- data_frame0(xintercept = xintercept)
    mapping <- ggplot2::aes(xintercept = xintercept)
    show.legend <- FALSE
  }
  ggplot2::layer(data = data, mapping = mapping, stat = StatIdentity,
                 geom = GeomScribblevline, position = PositionIdentity,
                 show.legend = show.legend, inherit.aes = FALSE,
                 params = rlang::list2(na.rm = na.rm, res = res, ...))
}


#' The ggproto object that powers scribbled vertical reference lines
#'
#' See \link[ggplot2]{ggplot2-ggproto}
#'
#' @format NULL
#' @usage NULL
#' @export
GeomScribblevline <- ggplot2::ggproto("GeomScribblevline", ggplot2::GeomVline,

   default_aes = ggplot2::aes(colour = "black", linewidth = 1,
                         linetype = 1, alpha = NA,
                         wonkiness = 0, wibbliness = 1),

   draw_panel = function (data, panel_params, coord,
                          lineend = "butt", res = 100) {

     ranges <- coord$backtransform_range(panel_params)
     data$x <- data$xintercept
     data$xend <- data$xintercept
     data$y <- ranges$y[1]
     data$yend <- ranges$y[2]
     GeomScribblesegment$draw_panel(unique0(data), panel_params, coord,
                            lineend = lineend, res = res)
   }
)
