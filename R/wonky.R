# Wonkify mechanism ------------------------------------------------------------

# Wonkiness is a measure of how inaccurate the vertices of a shape or line are.
# The details of applying this changes according to the different grob types and
# therefore it uses S3 method dispatch. It is unexported at present, but this
# could change if a need arose

wonkify <- function(obj, wonkiness, default.units, ...) {
  UseMethod("wonkify")
}

#' @export
wonkify.polygon <- function(obj, wonkiness = 1, default.units = "npc", ...) {

  if(all(wonkiness == 0)) return(obj)

  wonkiness <- wonkiness[1]

  x <- grid::convertX(obj$x, default.units, TRUE)
  y <- grid::convertY(obj$y, default.units, TRUE)

  closed <- abs(x[1] - x[length(x)]) < 1e-5 &
    abs(y[1] - y[length(y)]) < 1e-5
  size <- sqrt(max(diff(range(x, na.rm = TRUE)), diff(range(y, na.rm = TRUE))))
  x[!is.na(x)] <- x[!is.na(x)] + rnorm(sum(!is.na(x)),
                                       0, 0.005 * size * wonkiness)
  y[!is.na(y)] <- y[!is.na(y)] + rnorm(sum(!is.na(y)),
                                       0, 0.005 * size * wonkiness)

  if(is.null(obj$pathId)) {
    obj$pathId <- rep(1, length(x))
  }
  if(closed) {
    x[length(x)] <- x[1]
    y[length(y)] <- y[1]
  }
  obj$x <- unit(x, default.units)
  obj$y <- unit(y, default.units)
  obj
}

#' @export
wonkify.pathgrob <- function(obj, wonkiness = 1, default.units = "npc", ...) {
  wonkify.polygon(obj, wonkiness, default.units)
}

#' @export
wonkify.polyline <- function(obj, wonkiness = 1, default.units = "native", ...)
{

  x <- grid::convertX(obj$x, default.units, TRUE)
  y <- grid::convertY(obj$y, default.units, TRUE)

  if(is.null(obj$id)) {
    if(is.null(obj$id.lengths)) {
      obj$id <- rep(1, length(x))
    } else {
      obj$id <- rep(seq_along(obj$id.lengths), obj$id.lengths)
    }
  }

  n_lines <- length(unique(obj$id))

  if(length(wonkiness) == 1) wonkiness <- rep(wonkiness, n_lines)

  size <- max(diff(range(x, na.rm = TRUE)), diff(range(y, na.rm = TRUE)))

  x <- do.call("c", Map(function(x, w) {
    x[!is.na(x)] <- x[!is.na(x)] + rnorm(sum(!is.na(x)), 0, 0.01 * size * w)
    x
  }, split(x,obj$id), wonkiness))

  y <- do.call("c", Map(function(x, w) {
    x[!is.na(x)] <- x[!is.na(x)] + rnorm(sum(!is.na(x)), 0, 0.01 * size * w)
    x
  }, split(y, obj$id), wonkiness))

  obj$x <- unit(x, default.units)
  obj$y <- unit(y, default.units)
  obj
}

#' @export
wonkify.segments <- function(obj, wonkiness = 1, default.units = "native", ...)
{

  x0 <- grid::convertX(obj$x0, default.units, TRUE)
  y0 <- grid::convertY(obj$y0, default.units, TRUE)
  x1 <- grid::convertX(obj$x1, default.units, TRUE)
  y1 <- grid::convertY(obj$y1, default.units, TRUE)

  if(length(x0) == 1 && length(x1) == 1 &&
     length(y0) > 1 && length(y1) == length(y0)) {
    x0 <- rep(x0, length(y0))
    x1 <- rep(x1, length(y0))
  }

  if(length(y0) == 1 && length(y1) == 1 &&
     length(x0) > 1 && length(x1) == length(x0)) {
    y0 <- rep(y0, length(x0))
    y1 <- rep(y1, length(x0))
  }
  if(length(wonkiness) == 1) wonkiness <- rep(wonkiness, length(x0))

  size <- sqrt((x1 - x0)^2 + (y1 - y0)^2)

  x0 <- x0 + rnorm(length(x0), 0, 0.01 * size * wonkiness)
  x1 <- x1 + rnorm(length(x0), 0, 0.01 * size * wonkiness)
  y0 <- y0 + rnorm(length(x0), 0, 0.01 * size * wonkiness)
  y1 <- y1 + rnorm(length(x0), 0, 0.01 * size * wonkiness)

  obj$x0 <- unit(x0, default.units)
  obj$y0 <- unit(y0, default.units)
  obj$x1 <- unit(x1, default.units)
  obj$y1 <- unit(y1, default.units)

  obj
}
