

# Wonkify mechanism ------------------------------------------------------------

# Wonkiness is a measure of how inaccurate the vertices of a shape or line are.
# The details of applying this changes according to the different grob types and
# therefore it uses S3 method dispatch. It is unexported at present, but this
# could change if a need arose

wonkify <- function(x, ...) {
  UseMethod("wonkify")
}

#' @export
wonkify.polygon <- function(poly, wonkiness = 1) {
  wonkiness <- wonkiness[1]
  x <- grid::convertX(poly$x, "npc", TRUE)
  y <- grid::convertY(poly$y, "npc", TRUE)
  size <- max(diff(range(x)), diff(range(y)))
  x <- x + rnorm(length(x), 0, 0.01 * size * wonkiness)
  y <- y + rnorm(length(y), 0, 0.01 * size * wonkiness)
  if(is.null(poly$pathId)) {
    poly$pathId <- rep(1, length(x))
  }
  split(x, poly$pathId) <- lapply(split(x, poly$pathId),
                                  function(x) c(head(x, -1), x[1]))
  split(y, poly$pathId) <- lapply(split(y, poly$pathId),
                                  function(x) c(head(x, -1), x[1]))
  poly$x <- grid::unit(x, "npc")
  poly$y <- grid::unit(y, "npc")
  poly
}

#' @export
wonkify.pathgrob <- function(path, wonkiness = 1) {
  wonkify.polygon(path, wonkiness)
}

#' @export
wonkify.polyline <- function(line, wonkiness = 1) {

  x <- grid::convertX(line$x, "native", TRUE)
  y <- grid::convertY(line$y, "native", TRUE)

  if(is.null(line$id)) line$id <- rep(1, length(x))

  n_lines <- length(unique(line$id))

  if(length(wonkiness) == 1) wonkiness <- rep(wonkiness, n_lines)

  size <- max(diff(range(x)), diff(range(y)))

  x <- do.call("c", Map(function(x, w) {
    x + rnorm(length(x), 0, 0.01 * size * w)
  }, split(x,line$id), wonkiness))

  y <- do.call("c", Map(function(x, w) {
    x + rnorm(length(x), 0, 0.01 * size * w)
  }, split(y,line$id), wonkiness))

  line$x <- grid::unit(x, "native")
  line$y <- grid::unit(y, "native")
  line
}

#' @export
wonkify.segments <- function(line, wonkiness = 1) {

  x0 <- grid::convertX(line$x0, "native", TRUE)
  y0 <- grid::convertY(line$y0, "native", TRUE)
  x1 <- grid::convertX(line$x1, "native", TRUE)
  y1 <- grid::convertY(line$y1, "native", TRUE)

  if(length(wonkiness) == 1) wonkiness <- rep(wonkiness, length(x0))

  size <- sqrt((x1 - x0)^2 + (y1 - y0)^2)

  x0 <- x0 + rnorm(length(x0), 0, 0.01 * size * wonkiness)
  x1 <- x1 + rnorm(length(x0), 0, 0.01 * size * wonkiness)
  y0 <- y0 + rnorm(length(x0), 0, 0.01 * size * wonkiness)
  y1 <- y1 + rnorm(length(x0), 0, 0.01 * size * wonkiness)

  line$x0 <- grid::unit(x0, "native")
  line$y0 <- grid::unit(y0, "native")
  line$x1 <- grid::unit(x1, "native")
  line$y1 <- grid::unit(y1, "native")

  line
}

# Wibblify mechanism -----------------------------------------------------------

# Wibbliness attempts to emulate how shaky the hand drawing the line was. The
# details of applying this changes according to the different grob types and
# therefore it uses S3 method dispatch. It is unexported at present, but this
# could change if a need arose

wibblify <- function(shape, ...) {
  UseMethod("wibblify")
}

#' @export
wibblify.polygon <- function(poly, wibbliness = 1, res = 100) {
  x <- grid::convertX(poly$x, "npc", TRUE)
  y <- grid::convertY(poly$y, "npc", TRUE)

  if(is.null(poly$pathId)) {
    poly$pathId <- rep(1, length(x))
  }
  if(res < length(poly$x)) res <- 2 * length(poly$x)
  x <- do.call("c", lapply(split(x, poly$pathId), function(x) {
         approx(seq_along(x), x, xout = seq(1, length(x), len = res))$y
  }))
  y <- do.call("c", lapply(split(y, poly$pathId), function(x) {
    approx(seq_along(x), x, xout = seq(1, length(x), len = res))$y
  }))
  poly$pathId <- do.call("c", lapply(split(poly$pathId, poly$pathId),
                                     function(x) rep(x[1], res)))
  x <- x + rnorm(length(x), 0, 0.0005 * wibbliness)
  y <- y + rnorm(length(y), 0, 0.0005 * wibbliness)


  split(x, poly$pathId) <- lapply(split(x, poly$pathId),
                                  function(x) c(head(x, -1), x[1]))
  split(y, poly$pathId) <- lapply(split(y, poly$pathId),
                                  function(x) c(head(x, -1), x[1]))
  poly$x <- grid::unit(x, "npc")
  poly$y <- grid::unit(y, "npc")
  poly
}

#' @export
wibblify.pathgrob <- function(poly, wibbliness = 1, res = 100) {
  wibblify.polygon(poly, wibbliness, res)
}

#' @export
wibblify.segments <- function(line, wibbliness = 1, res = 100) {

  x0 <- grid::convertX(line$x0, "native", TRUE)
  y0 <- grid::convertY(line$y0, "native", TRUE)
  x1 <- grid::convertX(line$x1, "native", TRUE)
  y1 <- grid::convertY(line$y1, "native", TRUE)

  if(length(wibbliness) == 1) wibbliness <- rep(wibbliness, length(x0))

  xvals <- do.call("c", Map(function(x0, x1, w) {
    seq(x0, x1, length.out = res) + rnorm(res, 0, 0.0005 * w)
  }, x0, x1, wibbliness))

  yvals <- do.call("c", Map(function(y0, y1, w) {
    seq(y0, y1, length.out = res) + rnorm(res, 0, 0.0005 * w)
  }, y0, y1, wibbliness))

  ids <- rep(seq_along(x0), each = res)

  grid::polylineGrob(xvals, yvals, id = ids, gp = line$gp,
                     default.units = "native")
}

#' @export
wibblify.polyline <- function(line, wibbliness = 1, res = 100) {
  x <- grid::convertX(line$x, "native", TRUE)
  y <- grid::convertY(line$y, "native", TRUE)

  if(is.null(line$id)) line$id <- rep(1, length(x))
  n_lines <- length(unique(line$id))
  if(length(wibbliness) == 1) wibbliness <- rep(wibbliness, n_lines)

  if(res < length(line$x)) res <- 2 * length(line$x)

  x <- do.call("c", Map(function(x, w) {
    approx(seq_along(x), x, xout = seq(1, length(x), len = res))$y +
      rnorm(res, 0, 0.0005 * w)
  }, split(x, line$id), wibbliness))

  y <- do.call("c", Map(function(x, w) {
    approx(seq_along(x), x, xout = seq(1, length(x), len = res))$y +
      rnorm(res, 0, 0.0005 * w)
  }, split(y, line$id), wibbliness))

  line$id <- do.call("c", lapply(split(line$id, line$id),
                                 function(x) rep(x[1], res)))

  line$x <- grid::unit(x, "native")
  line$y <- grid::unit(y, "native")
  line
}


# Scribble fill mechanism ------------------------------------------------------

# This function creates a bunch of wibbly parallel lines of a given angle
# and density which will be masked and used for the scribble fill of solid
# shapes
make_scribbles <- function(angle = 45, density = 100, randomness = 1,
                           gp = grid::gpar(), vp = NULL) {

  density <- as.integer(density)
  x  <- seq(-1, 2, length = density * 2)
  y0 <- rep(-1, length = density * 2)
  y1 <- rep( 2, length = density * 2)
  angle <- pi * angle / (180)
  m <- Map(function(x0, x1, y0, y1, id) {
    xvals <- seq(x0, x1, length = 100)
    yvals <- seq(y0, y1, length = 100)
    x     <- xvals + (yvals - 0.5) * cos(angle)
    y     <- 0.5 + (yvals - 0.5) * sin(angle) - (xvals - 0.5) * cos(angle)
    x     <- x + rnorm(100, 0, 0.001 * randomness)
    y     <- y + rnorm(100, 0, 0.001*  randomness)
    data.frame(x = x, y = y, id = id)
  }, x0 = x, x1 = x, y0 = y0, y1 = y1, id = seq_along(x))
  m <- do.call("rbind", m)
  with(m, grid::polylineGrob(x = x, y = y, id = id, gp = gp, vp = vp))
}

# Creates a mask from the given polygon (after deforming it if desired),
# and generates parallel lines inside the mask. It bundles the result along
# with the original polygon into a single grob.
scribble_fill <- function(shape, angle = 45, density = 100, randomness = 1,
                          col = "black", lwd = 1, wonkiness = 1,
                          sloppiness = 1) {
  shape_mask <- wonkify(shape, sloppiness / 2)
  shape_mask$gp <- grid::gpar(fill = "black")
  scrib <- make_scribbles(angle, density, randomness,
                          gp = grid::gpar(lwd = lwd, col = col),
                          vp = grid::viewport(mask = shape_mask))
  grid::setChildren(grid::gTree(cl = "scribble"), grid::gList(shape, scrib))
}
