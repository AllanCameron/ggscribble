# Wibblify mechanism -----------------------------------------------------------

# Wibbliness attempts to emulate how shaky the hand drawing the line was. The
# details of applying this changes according to the different grob types and
# therefore it uses S3 method dispatch. It is unexported at present, but this
# could change if a need arose

do_wibble <- function(x0, y0, x1, y1, n, wibbliness) {

  n <- n - 1
  if(n < 2) n <- 2
  mult <- 0.001 * wibbliness * dbeta(seq(0, 1, length = n + 1), 2, 2)
  bend <- mult * rnorm(1) * sin(seq(0, runif(1, pi/2, 2 * pi), length = n + 1))
  len <- sqrt((x1 - x0)^2 + (y1 - y0)^2)
  theta <- atan2(y1 - y0, x1 - x0)
  randomness <- rnorm(n + 1, 0, wibbliness * 0.00025)
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

wibble_segs <- function(x0, x1, y0, y1, wibbliness = 1, res = 200) {

  lx    <- length(x0)
  if(length(wibbliness) != lx) wibbliness <- rep(wibbliness[1], lx)
  len   <- sqrt((x1 - x0)^2 + (y1 - y0)^2)
  theta <- atan2(y1 - y0, x1 - x0)
  nvals <- ceiling(len * res)
  nvals <- pmax(2, nvals)
  id    <- rep(seq_along(nvals), nvals)
  x     <- len[id] * sequence(nvals, from = 0, by = 1) / (nvals[id] - 1)
  y     <- unlist(lapply(nvals, function(x) {
    noiz <- c(ambient::noise_perlin(c(x, 1), frequency = 0.02))
    noiz - tail(noiz, 1) * seq(0, 1, length.out = x)
  })) * wibbliness[id] * 0.02

  xvals <- cos(theta)[id] * x + cos(theta - pi/2)[id] * y + x0[id]
  yvals <- sin(theta)[id] * x + sin(theta - pi/2)[id] * y + y0[id]

  list(x = xvals, y = yvals, id = id)
}

wibble_lines <- function(x, y, id, wibbliness = 1, res = 100) {

  seg_len   <- numeric(length(x))
  seg_len[] <- NA_real_
  theta     <- seg_len
  is_seg    <- c(diff(id) == 0, FALSE)
  is_end    <- c(FALSE, diff(id) == 0)
  seg_len[is_seg] <- sqrt(diff(x)^2 + diff(y)^2)[is_seg]
  res_len   <- pmax(5, ceiling(res * seg_len))

  x <- do.call("c", Map(function(a, b, rl) {
    if(is.na(a) | is.na(b)) return(NA_real_)
    seq(a, b, length.out = rl)
    }, a = x[is_seg], b = x[is_end], rl = res_len[is_seg]))

  y <- do.call("c", Map(function(a, b, rl) {
    if(is.na(a) | is.na(b)) return(NA_real_)
    seq(a, b, length.out = rl)
    }, a = y[is_seg], b = y[is_end], rl = res_len[is_seg]))

  res_len[is.na(res_len) & is_seg] <- 1
  id <- rep(id[is_seg], res_len[!is.na(res_len)])
  xnoise <- do.call("c", Map(function(x, y, w) {
    ambient::gen_perlin(x + runif(1, 0, 1000),
                        y + runif(1, 0, 1000),
                        frequency = 10 * res/200) * 0.01 * w
  }, x = split(x, id), y = split(y, id), w = wibbliness))
  ynoise <- do.call("c", Map(function(x, y, w) {
    ambient::gen_perlin(x + runif(1, 0, 1000),
                        y + runif(1, 0, 1000),
                        frequency = 10 * res/200) * 0.01 * w
  }, x = split(x, id), y = split(y, id), w = wibbliness))

  list(x = unname(x + xnoise), y = unname(y + ynoise), id = id)
}

wibble_polys <- function(x, y, pathId, id, wibbliness = 1, res = 200) {

  seg_len   <- numeric(length(x))
  seg_len[] <- NA_real_
  theta     <- seg_len
  bits      <- paste(pathId, id, sep = ".")
  bit_id    <- match(bits, unique(bits))
  lens      <- rle(bit_id)$lengths
  is_seg    <- c(diff(bit_id) == 0, FALSE)
  is_end    <- c(FALSE, diff(bit_id) == 0)
  seg_len[is_seg] <- sqrt(diff(x)^2 + diff(y)^2)[is_seg]
  res_len   <- pmax(5, ceiling(res * seg_len))
  pathId <- rep(pathId[is_seg], res_len[!is.na(res_len)])
  id <- rep(id[is_seg], res_len[!is.na(res_len)])
  bit_id <- rep(bit_id[is_seg], res_len[!is.na(res_len)])

  x <- do.call("c", Map(function(a, b, rl) {
    seq(a, b, length.out = rl)
    }, a = x[is_seg], b = x[is_end], rl = res_len[is_seg]))
  y <- do.call("c", Map(function(a, b, rl) seq(a, b, length.out = rl),
                        a = y[is_seg], b = y[is_end], rl = res_len[is_seg]))

  xnoise <- do.call("c", Map(function(x, y, w, b) {
    ambient::gen_perlin(x + runif(1, 0, 1000),
                        y + runif(1, 0, 1000),
                        frequency = res/(b^0.5) * 0.15) * 0.005 * w
  }, x = split(x, bit_id), y = split(y, bit_id), w = wibbliness, b = lens))

  ynoise <- do.call("c", Map(function(x, y, w, b) {
    ambient::gen_perlin(x + runif(1, 0, 1000),
                        y + runif(1, 0, 1000),
                        frequency = res/(b^0.5) * 0.15) * 0.005 * w
  }, x = split(x, bit_id), y = split(y, bit_id), w = wibbliness, b = lens))

  list(x = x + xnoise, y = y + ynoise, id = id, pathId = pathId)
}


wibblify <- function(obj, wibbliness, res, default.units, ...) {
  UseMethod("wibblify")
}

#' @export
wibblify.zeroGrob <- function(obj, wibbliness, res, default.units, ...) {
  return(ggplot2::zeroGrob())
}

#' @export
wibblify.polygon <- function(obj, wibbliness = 1, res = 100,
                             default.units = "native", ...) {

  x <- grid::convertX(obj$x, default.units, TRUE)
  y <- grid::convertY(obj$y, default.units, TRUE)

  if(is.null(obj$pathId)) {
    obj$pathId <- rep(1, length(x))
  }

  if(res < length(obj$x)) res <- 5 * length(obj$x)

  wibbliness <- rep(wibbliness[1], length(unique(obj$pathId)))

  dat <- wibble_polys(x, y, obj$pathId, obj$id, wibbliness, res)

  obj$x <- unit(dat$x, default.units)
  obj$y <- unit(dat$y, default.units)
  obj$id <- dat$id
  obj$pathId <- dat$pathId
  obj
}

#' @export
wibblify.pathgrob <- function(obj, wibbliness = 1, res = 100,
                              default.units = "npc", ...) {
  wibblify.polygon(obj, wibbliness, res, default.units = default.units)
}

#' @export
wibblify.segments <- function(obj, wibbliness = 1, res = 100,
                              default.units = "native", ...) {

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

  if(length(wibbliness) == 1) wibbliness <- rep(wibbliness, length(x0))

  wibbled <- wibble_segs(x0 = x0, x1 = x1, y0 = y0, y1 = y1,
                         res = res, wibbliness = wibbliness)

  grid::polylineGrob(wibbled$x, wibbled$y, id = wibbled$id,
                     gp = obj$gp,
                     default.units = default.units)
}

#' @export
wibblify.polyline <- function(obj, wibbliness = 1, res = 100,
                              default.units = "native", ...) {

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
  if(length(wibbliness) == 1) wibbliness <- rep(wibbliness, n_lines)

  li <- wibble_lines(x, y, obj$id, wibbliness, res)

  obj$x <- unit(li$x, default.units)
  obj$y <- unit(li$y, default.units)
  obj$id <- li$id
  obj
}

