#' Scribbled axis scales
#'
#' The wobbly axis lines in ggscribble are actually achieved via ggplot's guide
#' system rather than via its scales, but since specifying guides requires
#' more coding and may not be familiar to less advanced users, there are
#' scribble versions of all the axis scales included as convenience functions.
#' These essentially just use the `wibbliness` and `res` parameters to generate
#' the correct guide, which is then passed on to the equivalent ggplot scale.
#'
#' @param ... Arguments passed to ggplot's `scale_x_` or `scale_y_` functions
#' @param wibbliness Controls how much the axis line deviates from straight.
#'  A value of 0 is perfectly straight, and the default of 1 is set to look
#'  carefully hand-drawn.
#' @param res This controls the resolution of the noise used to wibble the axis
#' line. Essentially it controls the frequency of the wibble. The default of
#' 200 emulates a fairly steady hand. Higher values will look shakier and lower
#' values give smoother deviations.
#'
#' @return A ggplot scale object which can be added to a plot
#' @export
#'
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Width)) +
#'   geom_scribbleboxplot(ggplot2::aes(scribblecolour = Species),
#'                        staplewidth = 0.4) +
#'   ggplot2::theme_classic(16) +
#'   scale_scribble_y_continuous() +
#'   scale_scribble_x_discrete()

scale_scribble_x_continuous <- function(..., wibbliness = 1, res = 200) {

  args <- rlang::list2(...)
  args <- check_scribbleguide(args, wibbliness, res)
  fn <- sub("scribble_", "", as.character(as.list(sys.call(0))[[1]]))
  do.call("do.call", list(what = str2lang(paste0("ggplot2::", fn)), args))
}

# The body of the above function is written in such a way that it will work
# generically for appropriately named scale functions. We can therefore copy
# it to correctly named functions for export.

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_x_binned <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_x_date <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_x_datetime <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_x_discrete <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_x_log10 <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_x_reverse <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_x_sqrt <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_x_time <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_y_binned <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_y_continuous <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_y_date <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_y_datetime <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_y_discrete <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_y_log10 <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_y_reverse <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_y_sqrt <- scale_scribble_x_continuous

#' @rdname scale_scribble_x_continuous
#' @export
scale_scribble_y_time <- scale_scribble_x_continuous

#-------------------------------------------------------------------------------
# Utility function to ensure appropriate warnings are given in the event of
# users trying to specify a non-scribbled guide in the scale calls, and
# inform of clashes between guide and scale parameters

check_scribbleguide <- function(args, wibbliness = 1, res = 200) {

  fn <- as.character(as.list(sys.call(1))[[1]])
  fn2 <- sub("scribble_", "", fn)
  if("guide" %in% names(args)) {
    if(!inherits(args$guide, "GuideScribbleaxis")) {
      cli::cli_warn(c(paste0(
        "Unscribbled \"guide\" argument ignored in ",
        "{.fn ", fn, "}"),
        i = paste0("Please use {.fn guide_scribbleaxis} to specify guide ",
                   "parameters, or use {.fn ", fn2, "} for a non-scribbled",
                   "axis instead")))
    } else {
      if(wibbliness != args$guide$params$wibbliness) {
        cli::cli_inform(paste0("{.var Wibbliness} set in guide; this will ",
                               "override {.var wibbliness} set in ",
                               "{.fn ", fn, "}"))
      }
      if(res != args$guide$params$res) {
        cli::cli_inform(paste0("{.var res} set in guide; this will ",
                               "override {.var res} set in ",
                               "{.fn ", fn, "}"))
      }
      return(args)
    }
  }
  args$guide <- guide_scribbleaxis(wibbliness = wibbliness, res = res)
  args
}
