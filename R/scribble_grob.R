#' Create imperfect polygons filled with scribbled lines
#'
#' @param x A single vector of x co-ordinates for all shapes, given as `grid`
#'   units. A numeric vector can be used instead; this will be converted into
#'   a units vector according to the `default.units` argument.
#' @param y The y co-ordinates for all shapes, following the same rules as `x`
#' @param id A vector matching the length of `x` and `y`. Different values
#'   within the `id` vector specify different shapes into which the `x` and `y`
#'   co-ordinates will be split.
#' @param pathId A numeric vector used to separate locations in x and y into
#'   distinct paths. All locations with the same pathId belong to the same path.
#' @param gp TA `grid::gpar` object containing the graphical parameters for the
#'   shape. Note that these will affect the outline and fill of the shape, but
#'   will _not_ affect the appearance of the scribbles, which are determined by
#'   the arguments specified below
#' @param angle A numeric vector with the same length as the number of shapes
#'   which specifies the angle of the scribbles drawn inside each shape. A
#'   single value can be given instead to apply to all shapes.
#' @param wonkiness A numeric vector with the same length as the number of
#'   shapes which controls the amount of random displacement of the vertices
#'   of each shape. A value of 0 means the shape is exactly as specified. The
#'   default value of 1 approximates a modestly careful hand drawing. A
#'   single value can be given instead to apply to all shapes.
#' @param wibbliness A numeric vector with the same length as the number of
#'   shapes which controls how imperfect the straight lines of the shapes are.
#'   A value of 0 is perfectly straight, and the default value of 1 aims to
#'   emulate a freehand line. A single value can be given instead to apply to
#'   all shapes.
#' @param sloppiness A numeric vector with the same length as the number of
#'   shapes which controls how closely the shape of the scribble fill
#'   approximates the outline it fills. A single value can be given instead to
#'   apply to all shapes.
#' @param scribbledensity A numeric vector with the same length as the number of
#'   shapes which controls how densely the scribbles are packed into the shape.
#'   The default value is 100. A single value can be given instead to apply to
#'   all shapes.
#' @param randomness A numeric vector with the same length as the number of
#'   shapes which controls how straight the scribbles are. This is the
#'   equivalent of "wibbliness" for the scribble fill lines. A
#'   single value can be given instead to apply to all shapes.
#' @param scribblewidth A numeric vector with the same length as the number of
#'   shapes which controls the line width of the scribble lines within each
#'   shape. A single value can be given instead to apply to all shapes.
#' @param scribblecolour A character vector with the same length as the number
#'   of shapes which controls the colors of the scribble lines inside each
#'   shape. A single value can be given instead to apply to all shapes.
#' @param default.units An atomic character string which determines the units
#'   into which the x and y co-ordinates will be converted if given as a
#'   numeric vector.
#' @param res The number of points into which the edges of the polygon will be
#'   split for "wibbling"
#' @return A `scribbles` object, which is a grob that can be drawn using the
#'   grid graphics system.
#' @export
#'
#' @examples
#' grid::grid.newpage()
#' sg <- scribbleGrob(x = c(0.4, 0.6, 0.6, 0.4, 0.8, 0.9, 0.85),
#'                    y = c(0.4, 0.4, 0.6, 0.6, 0.8, 0.8, 0.9),
#'                    id = c(1, 1, 1, 1, 2, 2, 2),
#'                    scribblecolour = c("red", "green"),
#'                    scribbledensity = 200)
#' grid::grid.draw(sg)

scribbleGrob <- function(x, y, id, gp = gpar(), angle = 45, wonkiness = 1,
                         pathId = NULL,
                         wibbliness = 1, sloppiness = 1,
                         scribbledensity = 100,
                         randomness = 1, scribblewidth = 1,
                         scribblecolour = "black", default.units = "npc",
                         res = 200) {

  if(missing(id)) id <- rep(1, length(x))
  if(is.null(pathId)) pathId <- id
  if(length(pathId) != length(x)) stop("mismatch between co-ordinates and id")
  n_groups <- length(unique(pathId))
  pars <- list(angle = angle,
               wonkiness = wonkiness,
               wibbliness = wibbliness,
               sloppiness = sloppiness,
               scribbledensity = scribbledensity,
               randomness = randomness,
               scribblewidth = scribblewidth,
               scribblecolour = scribblecolour)

  pars <- setNames(Map(function(x, nm) {
    if(length(x) == 1) x <- rep(x, n_groups)
    if(length(x) != n_groups) stop("mismatch between ", nm, " and groups")
    return(x)
  }, pars, names(pars)), names(pars))

  grobs <- Map(function(x, y, id, angle, density, wonk, wibble, neat, i,
                        col, lwd) {
    gs <- grid::pathGrob(x, y, id = id, gp = gp[i],
                   default.units = default.units)
    gs <- wonkify(gs, wonkiness = pars$wonkiness)
    gs <- wibblify(gs, wibbliness = pars$wibbliness, res)
    scribble_fill(gs, angle = angle, scribbledensity = density,
                  sloppiness = neat, randomness = randomness,
                  col = col, lwd = lwd)

  }, split(x, pathId), split(y, pathId), split(id, pathId), pars$angle,
  pars$scribbledensity,
  pars$wonkiness, pars$wibbliness, pars$sloppiness, seq_along(pars$angle),
  pars$scribblecolour, pars$scribblewidth)

  grid::setChildren(grid::gTree(cl = "scribbles"), do.call(grid::gList, grobs))
}
