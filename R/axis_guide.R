GuideScribbleaxis <- ggplot2::ggproto("GuideScribbleaxis", ggplot2::GuideAxis,

  assemble_drawing = function (grobs, layout, sizes, params, elements) {

    axis_line <- grobs$decor
    axis_line <- grid::segmentsGrob(x0 = axis_line$x[1],
                                    y0 = axis_line$y[1],
                                    x1 = axis_line$x[2],
                                    y1 = axis_line$y[2],
                                    gp = axis_line$gp)
    axis_line <- ggscribble:::wibblify(axis_line, 10 * params$wibbliness,
                                       default.units = "npc", res = params$res)

    z <- if (params$position == "left") c(2, 1, 3) else 1:3
    z <- rep(z, c(1, length(grobs$labels), 1))
    grobs <- c(list(grobs$ticks), grobs$labels, list(grobs$title))
    gt <- rlang::exec(gtable::gtable, `:=`(!!params$orth_sizes, sizes),
                      `:=`(!!params$para_sizes,  grid::unit(1, "npc")),
                      name = "axis")
    gt <- gtable::gtable_add_grob(gt, grobs, t = layout$t, b = layout$b,
                          l = layout$l, r = layout$r, clip = "off", z = z)
    vp <- rlang::exec(grid::viewport, `:=`(!!params$orth_aes,
                                           grid::unit(params$orth_side,
                 "npc")), `:=`(!!params$orth_size, params$measure_gtable(gt)),
               just = params$opposite)
    g <- ggplot2:::absoluteGrob(grid::gList(axis_line, gt),
                      width = gtable::gtable_width(gt),
                      height = gtable::gtable_height(gt), vp = vp)
    g
  },

  params = list(title = structure(list(), class = "waiver"),
                theme = NULL,
                name = "axis",
                hash = character(0),
                position = structure(list(), class = "waiver"),
                direction = NULL,
                angle = NULL,
                n.dodge = 1,
                minor.ticks = FALSE,
                cap = "none",
                order = 0,
                check.overlap = FALSE,
                wibbliness = 1,
                res = 200)
)

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



