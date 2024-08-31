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

wonkify <- function(poly, wonkiness = 1) {
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

