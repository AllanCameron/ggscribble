
`%||%` <- function (x, y) {
  if (rlang::is_null(x))
    y
  else x
}

manual_scale <- function (aesthetic, values = NULL, breaks = ggplot2::waiver(),
                          name = waiver(), ..., limits = NULL,
                          call = rlang::caller_call()) {

  call <- call %||% current_call()
  if (rlang::is_missing(values)) {
    values <- NULL
  }
  else {
    force(values)
  }
  if (is.null(limits) && !is.null(names(values))) {
    force(aesthetic)
    limits <- function(x) {
      x <- intersect(x, c(names(values), NA)) %||% character()
      if (length(x) < 1) {
        cli::cli_warn(paste0(
          "No shared levels found between {.code names(values)} of the manual ",
          "scale and the data's {.field {aesthetic}} values."))
      }
      x
    }
  }
  if (is.vector(values) && is.null(names(values)) &&
      !inherits(breaks, "waiver") && !is.null(breaks) && !is.function(breaks)) {
    if (length(breaks) <= length(values)) {
      names(values) <- breaks
    }
    else {
      names(values) <- breaks[1:length(values)]
    }
  }
  pal <- function(n) {
    if (n > length(values)) {
      cli::cli_abort(paste0(
        "Insufficient values in manual scale. ",
        "{n} needed but only {length(values)} provided."))
    }
    values
  }
  ggplot2::discrete_scale(aesthetic, name = name, palette = pal,
                          breaks = breaks,
                          limits = limits, call = call, ...)
}
