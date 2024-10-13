#' @rdname ggscribble-ggproto
#' @usage NULL
#' @format NULL
#' @export

GuideScribbleaxis <- ggproto("GuideScribbleaxis", ggplot2::GuideAxis,

  params = list(
    title = waiver(),
              theme = NULL,
              name = "axis",
              hash = character(0),
              position = waiver(),
              direction = NULL,
              angle = NULL,
              n.dodge = 1,
              minor.ticks = FALSE,
              cap = "none",
              order = 0,
              check.overlap = FALSE,
              wibbliness = 1,
              res = 200),

  setup_params = function (params) {

    position <- rlang::arg_match0(params$position,
                                  c("top", "right", "bottom", "left"))

    direction <- if (position %in% c("left", "right")) {
        "vertical"
    } else {
        "horizontal"
    }

    new_params <- c("aes", "orth_aes", "para_sizes", "orth_size",
        "orth_sizes", "vertical", "measure_gtable", "measure_text")

    if (direction == "vertical") {
        params[new_params] <- list("y", "x", "heights", "width",
            "widths", TRUE, gtable::gtable_width, width_cm)
    } else {
        params[new_params] <- list("x", "y", "widths", "height",
            "heights", FALSE, gtable::gtable_height, height_cm)
    }
    new_params <- list(opposite = switch(position, top = "bottom",
                                         bottom = "top",
                                         left = "right",
                                         right = "left"),
        secondary = position %in% c("top", "right"), lab_first = position %in%
            c("top", "left"), orth_side = if (position %in% c("top",
            "right")) 0 else 1, direction = direction, position = position)

    c(params[setdiff(names(params), names(new_params))], new_params)
  },

  build_decor = function (decor, grobs, elements, params) {

    if (empty(decor)) {
      return(ggplot2::zeroGrob())
    }

    axis_line <- ggplot2::element_grob(elements$line,
                                       x = grid::unit(decor$x, "npc"),
                                       y = grid::unit(decor$y, "npc"))

    axis_line <- grid::segmentsGrob(x0 = axis_line$x[1],
                                    y0 = axis_line$y[1],
                                    x1 = axis_line$x[2],
                                    y1 = axis_line$y[2],
                                    gp = axis_line$gp)

    axis_line <- wibblify(axis_line, params$wibbliness,
                          default.units = "npc", res = params$res)

    return(axis_line)
  },

  assemble_drawing = function (grobs, layout, sizes, params, elements) {
    axis_line <- grobs$decor
    z <- if (params$position == "left") c(2, 1, 3) else 1:3
    z <- rep(z, c(1, length(grobs$labels), 1))
    grobs <- c(list(grobs$ticks), grobs$labels, list(grobs$title))
    gt <- rlang::exec(gtable::gtable,
                      `:=`(!!params$orth_sizes, sizes),
                      `:=`(!!params$para_sizes, grid::unit(1, "npc")),
                      name = "axis")
    gt <- gtable::gtable_add_grob(gt, grobs, t = layout$t, b = layout$b,
                                  l = layout$l, r = layout$r,
                                  clip = "off", z = z)
    vp <- rlang::exec(grid::viewport,
                      `:=`(!!params$orth_aes,
                           grid::unit(params$orth_side, "npc")),
                      `:=`(!!params$orth_size, params$measure_gtable(gt)),
                      just = params$opposite)

    ysize <- grid::convertY(vp$height, "cm", TRUE)
    xsize <- grid::convertX(vp$width, "cm", TRUE)
    ratio <- xsize / ysize
    xvals <- grid::convertX(axis_line$x, "npc", TRUE)

    is_ticks <- which(vapply(gt$grobs, function(x) inherits(x, "polyline"),
                             TRUE))
    if(length(is_ticks) > 0) {

      ticks <- gt$grobs[[is_ticks[1]]]
      ticks$vp <- vp

      npcs_x <- get_npc(ticks$x)[1]
      npcs_y <- get_npc(ticks$y)[1]

      ticks_y <- grid::convertY(ticks$y, "npc", TRUE)
      ticks_x <- grid::convertX(ticks$x, "npc", TRUE)
      start <- seq_along(ticks_y) %% 2 == 1
      end <- !start

      ticks <- grid::segmentsGrob(ticks_x[start], ticks_y[start],
                                  ticks_x[end], ticks_y[end], gp = ticks$gp,
                                  vp = vp)

      ticks <- wibblify(ticks, wibbliness = params$wibbliness,
                        res = params$res * 5, default.units = "npc")

      if(params$direction == "horizontal") {
        yvals <- grid::convertY(ticks$y - grid::unit(npcs_y, "npc"), "mm", TRUE)
        ticks$y <- grid::unit(npcs_y, "npc") + grid::unit(yvals, "mm")

      }
      if(params$direction == "vertical") {
        xvals <- grid::convertX(ticks$x - grid::unit(npcs_x, "npc"), "mm", TRUE)
        ticks$x <- grid::unit(npcs_x, "npc") + grid::unit(xvals, "mm")
      }
      ticks$vp <- NULL
      gt$grobs[[is_ticks[1]]] <- ticks
    }


    if(params$direction == "horizontal") {
      yvals <- grid::convertY(axis_line$y, "npc", TRUE)
      yvals <- ratio * (yvals - mean(yvals)) + mean(yvals)
      axis_line$y <- grid::unit(yvals, "npc")
    }

    if(params$direction == "vertical") {
      xvals <- grid::convertX(axis_line$x, "npc", TRUE)
      xvals <- (xvals - mean(xvals))/ratio + mean(xvals)
      axis_line$x <- grid::unit(xvals, "npc")
    }

    absoluteGrob(grid::gList(axis_line, gt),
                 width = gtable::gtable_width(gt),
                 height = gtable::gtable_height(gt),
                 vp = vp)
  }
)


#' Title
#'
#' @inheritParams ggplot2::guide_axis
#' @param wibbliness How wibbly the axis lines should be (0 = straight)
#' @param res A measure of the resolution of the line's wibbliness.
#'  Default is 200. Higher numbers give a higher frequency wibble
#'
#' @return A guide object that draws wibbly axis lines and ticks
#' @export
#'
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_scribblepoint() +
#'   ggplot2::guides(x = guide_scribbleaxis(wibbliness = 0.5),
#'                   y = guide_scribbleaxis(wibbliness = 0.5)) +
#'   theme_classic()

guide_scribbleaxis <- function (title = ggplot2::waiver(), theme = NULL,
                                check.overlap = FALSE,
                                angle = ggplot2::waiver(), n.dodge = 1,
                                minor.ticks = FALSE, cap = "none", order = 0,
                                position = ggplot2::waiver(),
                                wibbliness = 1, res = 200) {

  if(!is.logical(minor.ticks) || length(minor.ticks) > 1) {
    stop("In guide_scribbleaxis: \"minor.ticks\" must be `TRUE` or `FALSE`")
  }

  if (is.logical(cap)) {
    if(length(cap) > 1) {
      stop("In guide_scribbleaxis: \"cap\" must be length 1")
    }
    cap <- if (cap)
      "both"
    else "none"
  }
  cap <- rlang::arg_match0(cap, c("none", "both", "upper", "lower"))

  ggplot2::new_guide(title = title, theme = theme,
                     check.overlap = check.overlap,
                     angle = angle, n.dodge = n.dodge,
                     minor.ticks = minor.ticks,
                     cap = cap, wibbliness = wibbliness, res = res,
                     available_aes = c("x", "y", "r"),
                     order = order, position = position, name = "axis",
                     super = GuideScribbleaxis)
}



