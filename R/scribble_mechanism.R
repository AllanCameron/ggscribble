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
  shape_mask <- shape
  shape_mask$gp <- grid::gpar(fill = "black", col = "white")
  line_mask <- wonkify(shape_mask, sloppiness / 2)
  scrib <- make_scribbles(angle, density, randomness,
                          gp = grid::gpar(lwd = lwd, col = col),
                          vp = grid::viewport(mask = line_mask))
  shape$vp <- grid::viewport(mask = shape_mask)
  grid::setChildren(grid::gTree(cl = "scribble"), grid::gList(shape, scrib))
}


