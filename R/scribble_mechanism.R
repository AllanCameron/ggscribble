

# Wonkify mechanism ------------------------------------------------------------

# Wonkiness is a measure of how inaccurate the vertices of a shape or line are.
# The details of applying this changes according to the different grob types and
# therefore it uses S3 method dispatch. It is unexported at present, but this
# could change if a need arose

wonkify <- function(x, ...) {
  UseMethod("wonkify")
}

#' @export
wonkify.polygon <- function(poly, wonkiness = 1, default.units = "npc") {
  wonkiness <- wonkiness[1]
  x <- grid::convertX(poly$x, default.units, TRUE)
  y <- grid::convertY(poly$y, default.units, TRUE)
  size <- max(diff(range(x)), diff(range(y)))
  x <- x + rnorm(length(x), 0, 0.01 * size * wonkiness)
  y <- y + rnorm(length(y), 0, 0.01 * size * wonkiness)
  if(is.null(poly$pathId)) {
    poly$pathId <- rep(1, length(x))
  }
  poly$x <- grid::unit(x, default.units)
  poly$y <- grid::unit(y, default.units)
  poly
}

#' @export
wonkify.pathgrob <- function(path, wonkiness = 1, default.units = "npc") {
  wonkify.polygon(path, wonkiness, default.units)
}

#' @export
wonkify.polyline <- function(line, wonkiness = 1, default.units = "native") {

  x <- grid::convertX(line$x, default.units, TRUE)
  y <- grid::convertY(line$y, default.units, TRUE)

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

  line$x <- grid::unit(x, default.units)
  line$y <- grid::unit(y, default.units)
  line
}

#' @export
wonkify.segments <- function(line, wonkiness = 1, default.units = "native") {

  x0 <- grid::convertX(line$x0, default.units, TRUE)
  y0 <- grid::convertY(line$y0, default.units, TRUE)
  x1 <- grid::convertX(line$x1, default.units, TRUE)
  y1 <- grid::convertY(line$y1, default.units, TRUE)

  if(length(wonkiness) == 1) wonkiness <- rep(wonkiness, length(x0))

  size <- sqrt((x1 - x0)^2 + (y1 - y0)^2)

  x0 <- x0 + rnorm(length(x0), 0, 0.01 * size * wonkiness)
  x1 <- x1 + rnorm(length(x0), 0, 0.01 * size * wonkiness)
  y0 <- y0 + rnorm(length(x0), 0, 0.01 * size * wonkiness)
  y1 <- y1 + rnorm(length(x0), 0, 0.01 * size * wonkiness)

  line$x0 <- grid::unit(x0, default.units)
  line$y0 <- grid::unit(y0, default.units)
  line$x1 <- grid::unit(x1, default.units)
  line$y1 <- grid::unit(y1, default.units)

  line
}

# Wibblify mechanism -----------------------------------------------------------

# Wibbliness attempts to emulate how shaky the hand drawing the line was. The
# details of applying this changes according to the different grob types and
# therefore it uses S3 method dispatch. It is unexported at present, but this
# could change if a need arose

do_wibble <- function(x0, y0, x1, y1, n, wibbliness) {

  n <- n - 1
  if(n < 2) n <- 2
  mult <- 0.0005 * wibbliness * dbeta(seq(0, 1, length = n + 1), 2, 2)
  bend <- mult * rnorm(1) * sin(seq(0, runif(1, pi/2, 2 * pi), length = n + 1))
  len <- sqrt((x1 - x0)^2 + (y1 - y0)^2)
  theta <- atan2(y1 - y0, x1 - x0)
  randomness <- rnorm(n + 1, 0, wibbliness * 0.001)
  new_y <- numeric(n + 1)

  for(i in head((seq(n)[-1]), -1)) {
    mult <- if(i/n > 0.75) 0.9 - (0.9 * (4 * (i/n - 0.75))) else 0.9
    new_y[i] <-  mult * new_y[i - 1] + randomness[i]
  }

  new_y <- new_y + bend
  new_x <- c(0, cumsum(rep(len/n, n)))
  list(x = cos(theta) * new_x + cos(theta - pi/2) * new_y + x0,
       y = sin(theta) * new_x + sin(theta - pi/2) * new_y + y0)
}

wibblify <- function(shape, ...) {
  UseMethod("wibblify")
}

#' @export
wibblify.zeroGrob <- function(x, ...) {
  return(ggplot2::zeroGrob())
}

#' @export
wibblify.polygon <- function(poly, wibbliness = 1, res = 100,
                             default.units = "npc") {
  x <- grid::convertX(poly$x, default.units, TRUE)
  y <- grid::convertY(poly$y, default.units, TRUE)

  if(is.null(poly$pathId)) {
    poly$pathId <- rep(1, length(x))
  }

  if(res < length(poly$x)) res <- 2 * length(poly$x)

  wibbliness <- rep(wibbliness[1], length(unique(poly$pathId)))

  li <- Map(function(x, y, w) {
    li <- Map(do_wibble, x0 = head(x, -1), x1 = tail(x, -1), y0 = head(y, -1),
              y1 = tail(y, -1), n = rep(res, length(x) - 1),
              wibbliness = rep(w, length(x) - 1))
    list(x = do.call("c", lapply(li, function(x) x$x)),
         y = do.call("c", lapply(li, function(x) x$y)))
  }, split(x, poly$pathId), split(y, poly$pathId), wibbliness)

  poly$x <- grid::unit(do.call("c", lapply(li, function(x) x$x)), default.units)
  poly$y <- grid::unit(do.call("c", lapply(li, function(x) x$y)), default.units)

  poly$pathId <- do.call("c", lapply(split(poly$pathId, poly$pathId),
                                     function(x) rep(x[1], res)))
  poly
}

#' @export
wibblify.pathgrob <- function(poly, wibbliness = 1, res = 100,
                              default.units = "npc") {
  wibblify.polygon(poly, wibbliness, res, default.units = default.units)
}

#' @export
wibblify.segments <- function(line, wibbliness = 1, res = 100,
                              default.units = "native") {

  x0 <- grid::convertX(line$x0, default.units, TRUE)
  y0 <- grid::convertY(line$y0, default.units, TRUE)
  x1 <- grid::convertX(line$x1, default.units, TRUE)
  y1 <- grid::convertY(line$y1, default.units, TRUE)

  if(length(wibbliness) == 1) wibbliness <- rep(wibbliness, length(x0))

  wibbled <- Map(do_wibble, x0 = x0, x1 = x1, y0 = y0, y1 = y1,
                 n = rep(res, length(x0)), wibbliness = wibbliness)

  xvals <- do.call("c", lapply(wibbled, function(x) x$x))
  yvals <- do.call("c", lapply(wibbled, function(x) x$y))

  ids <- rep(seq_along(x0), each = res)

  grid::polylineGrob(xvals, yvals, id = ids, gp = line$gp,
                     default.units = default.units)
}

#' @export
wibblify.polyline <- function(line, wibbliness = 1, res = 100,
                              default.units = "native") {

  x <- grid::convertX(line$x, default.units, TRUE)
  y <- grid::convertY(line$y, default.units, TRUE)

  if(is.null(line$id)) line$id <- rep(1, length(x))
  n_lines <- length(unique(line$id))
  if(length(wibbliness) == 1) wibbliness <- rep(wibbliness, n_lines)

  if(res < length(line$x)) res <- 2 * length(line$x)

  li <- Map(function(x, y, id, w) {
    li <- Map(do_wibble, x0 = head(x, -1), x1 = tail(x, -1), y0 = head(y, -1),
              y1 = tail(y, -1), n = rep(res, length(x) - 1),
              wibbliness = rep(w, length(x) - 1))
    li <- list(x = do.call("c", lapply(li, function(x) x$x)),
               y = do.call("c", lapply(li, function(x) x$y)))
    li$id <- rep(id[1], length(li$x))
    li
  }, split(x, line$id), split(y, line$id), split(line$id, line$id), wibbliness)

  x  <- do.call("c", lapply(li, function(x) x$x))
  y  <- do.call("c", lapply(li, function(x) x$y))
  id <- do.call("c", lapply(li, function(x) x$id))

  line$x <- grid::unit(x, default.units)
  line$y <- grid::unit(y, default.units)
  line$id <- id
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


