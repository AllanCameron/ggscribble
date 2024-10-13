draw_key_scribble <- function(data, params, size) {

    if (is.null(data$linewidth)) {
        data$linewidth <- 0.5
    }

    lwd <- min(data$linewidth, min(size)/4)

    r <- grid::pathGrob(x = unit(c(0.1, 0.9, 0.9, 0.1, 0.1), "npc"),
                        y = unit(c(0.1, 0.1, 0.9, 0.9, 0.1), "npc"),
                   gp = gpar(col = data$colour %||% NA,
                    fill = fill_alpha(data$fill %||% "grey20",
                                               data$alpha),
        lty = data$linetype %||% 1, lwd = lwd * .pt,
        linejoin = params$linejoin %||%
            "mitre", lineend = params$lineend %||% "butt"))

    r <- wonkify(r, wonkiness = data$wonkiness * 5)

    scribble_fill(r, angle = data$angle,
                  scribbledensity = data$scribbledensity / 10,
                  randomness = data$randomness,
                  col = data$scribblecolour, lwd = data$scribblewidth,
                  wonkiness = data$wonkiness,
                  sloppiness = data$sloppiness)
}
