

absoluteGrob <- function (grob, width = NULL, height = NULL,
                          xmin = NULL, ymin = NULL, vp = NULL) {

  grid::gTree(children = grob, width = width, height = height, xmin = xmin,
        ymin = ymin, vp = vp, cl = "absoluteGrob")
}

`%||%` <- function (x, y) {
  if (rlang::is_null(x))
    y
  else x
}

unique0 <- function (x, ...) if (is.null(x)) x else vctrs::vec_unique(x, ...)

empty <- function (df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || inherits(df, "waiver")
}

ggname <- function (prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

modify_list <- function (old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

data_frame0 <- function (...) vctrs::data_frame(..., .name_repair = "minimal")

len0_null <- function (x) if (length(x) == 0) NULL else x


# Not actually a borrowed function, surprised ggplot doesn't have this already
get_first_rows <- function(data) {

  if(is.null(data$group)) return(data)

  data <- data[order(data$group), ]
  first_idx <- !duplicated(data$group)
  return(data[first_idx, ])
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

# Simple lowercase converter

as_lower <- function(x) {

  chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)
}

as_upper <- function(x) {

  chartr("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", x)
}


camelize <- function(x, first = FALSE) {

  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) {
    x <- paste0(as_upper(substring(x, 1, 1)), substring(x, 2))
  }
  x
}

find_global <- function(name, env, mode = "any") {

  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }
  nsenv <- asNamespace("geomtextpath")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }
  NULL
}


rd_aesthetics_item <- function(x) {

  req          <- x$required_aes
  req          <- sub("|", "} \\emph{or} \\code{", req, fixed = TRUE)
  req_aes      <- unlist(strsplit(x$required_aes, "|", fixed = TRUE))
  optional_aes <- setdiff(x$aesthetics(), req_aes)
  all          <- union(req, sort(optional_aes))

  ifelse(test = all %in% req,
         yes  = paste0("\\strong{\\code{", all, "}}"),
         no   = paste0("\\code{", all, "}"))
}


check_subclass <- function(
    x,
    subclass,
    argname = as_lower(subclass),
    env     = parent.frame()
) {

  if (inherits(x, subclass)) {
    x
  }
  else if (is.character(x) && length(x) == 1) {
    name <- paste0(subclass, camelize(x, first = TRUE))
    obj  <- find_global(name, env = env)
    if (is.null(obj) || !inherits(obj, subclass)) {
      rlang::abort(paste0("Can't find `", argname, "` called '", x, "'"))
    } else {
      obj
    }
  }
  else {
    abort(paste0("`", argname, "` must be either a string or a ",
                 subclass, " object"))
  }
}


# Calculate aesthetics and produce documentation item for given function / topic

rd_aesthetics <- function(type, name, check_label_variant = TRUE) {

  obj <- switch(type,
                geom = check_subclass(name, "Geom", env = globalenv()),
                stat = check_subclass(name, "Stat", env = globalenv()))
  aes <- rd_aesthetics_item(obj)
  txt <- "@section Aesthetics:"
  txt <- c(txt,
           paste0("\\code{", type,
                  "_", name, "()} ",
                  "understands the following aesthetics ",
                  "(required aesthetics are in bold):"))
  txt <- c(txt, "\\itemize{", paste0("  \\item ", aes), "}")

  lab_aes <- NULL
  if (check_label_variant) {
    # Check if there is 'text' to be substituted by 'label'
    lab_name <- gsub("^text", "label", name)
    if (!(lab_name == name)) {
      # Check if label variant exists
      lab_obj <- tryCatch(
        {
          switch(type,
                 geom = check_subclass(lab_name, "Geom", env = globalenv()),
                 stat = check_subclass(lab_name, "Stat", env = globalenv()))
        },
        error   = function(cond) {return(NULL)}
      )
      if (!is.null(lab_obj)) {
        lab_aes <- rd_aesthetics_item(lab_obj)
        lab_aes <- setdiff(lab_aes, aes)
      }
      if (length(lab_aes)) {
        txt <- c(txt,
                 paste0("In addition to aforementioned aesthetics,",
                        " \\code{", type, "_", lab_name, "()} ",
                        "also understands:"))
        txt <- c(txt, "\\itemize{", paste0("  \\item ", lab_aes), "}")
      }
    }
  }

  if (any(grepl("spacing", aes))) {
    txt <- c(txt,
             "The \\code{spacing} aesthetic allows fine control of spacing",
             " of text, which is called 'tracking' in typography.",
             "The default is 0 and units are measured in 1/1000 em.",
             "Numbers greater than zero increase the spacing,",
             "whereas negative numbers decrease the spacing.")
  }

  txt <- c(txt,
           "\n\nLearn more about setting these aesthetics ",
           "in \\code{vignette(\"ggplot2-specs\")}.")
  txt
}

# Required for reference line geoms as parameters can overwrite aesthetics

warn_overwritten_args <- function (
    fun_name,
    overwritten_arg,
    provided_args,
    plural_join = " and/or "
) {

  overwritten_arg_text <- paste0("`", overwritten_arg, "`")
  n_provided_args      <- length(provided_args)
  if (n_provided_args == 1) {
    provided_arg_text <- paste0("`", provided_args,
                                "`")
    verb <- "was"
  }
  else if (n_provided_args == 2) {
    provided_arg_text <- paste0("`", provided_args,
                                "`", collapse = plural_join)
    verb <- "were"
  }
  else {
    provided_arg_text <- paste0(paste0("`", provided_args[-n_provided_args],
                                       "`", collapse = ", "), ",", plural_join,
                                "`", provided_args[n_provided_args], "`")
    verb <- "were"
  }
  warn(paste0(fun_name,
              ": Ignoring ", overwritten_arg_text, " because ",
              provided_arg_text, " ", verb, " provided."))
}

id_var <- function (x, drop = FALSE) {

  if (length(x) == 0) {
    id <- integer()
    n = 0L
  }
  else if (!is.null(attr(x, "n")) && !drop) {
    return(x)
  }
  else if (is.factor(x) && !drop) {
    x <- addNA(x, ifany = TRUE)
    id <- as.integer(x)
    n <- length(levels(x))
  }
  else {
    levels <- sort(unique0(x), na.last = TRUE)
    id <- match(x, levels)
    n <- max(id)
  }
  attr(id, "n") <- n
  id
}

id <- function (.variables, drop = FALSE) {

  nrows <- NULL
  if (is.data.frame(.variables)) {
    nrows <- nrow(.variables)
    .variables <- unclass(.variables)
  }
  lengths <- lengths(.variables)
  .variables <- .variables[lengths != 0]
  if (length(.variables) == 0) {
    n <- nrows %||% 0L
    id <- seq_len(n)
    attr(id, "n") <- n
    return(id)
  }
  if (length(.variables) == 1) {
    return(id_var(.variables[[1]], drop = drop))
  }
  ids <- rev(lapply(.variables, id_var, drop = drop))
  p <- length(ids)
  ndistinct <- vapply(ids, attr, "n", FUN.VALUE = numeric(1),
                      USE.NAMES = FALSE)
  n <- prod(ndistinct)
  if (n > 2^31) {
    char_id <- rlang::inject(paste(!!!ids, sep = "\r"))
    res <- match(char_id, unique0(char_id))
  }
  else {
    combs <- c(1, cumprod(ndistinct[-p]))
    mat <- rlsng::inject(cbind(!!!ids))
    res <- c((mat - 1L) %*% combs + 1L)
  }
  if (drop) {
    id_var(res, drop = TRUE)
  }
  else {
    res <- as.integer(res)
    attr(res, "n") <- n
    res
  }
}

split_with_index <- function (x, f, n = max(f)) {

  if (n == 1) return(list(x))
  f <- as.integer(f)
  attributes(f) <- list(levels = as.character(seq_len(n)), class = "factor")
  unname(split(x, f))
}

df_rows <- function (x, i) {

  cols <- lapply(x, `[`, i = i)
  data_frame0(!!!cols, .size = length(i))
}

dapply <- function (df, by, fun, ..., drop = TRUE) {

  grouping_cols <- .subset(df, by)
  fallback_order <- unique0(c(by, names(df)))
  apply_fun <- function(x) {
    res <- fun(x, ...)
    if (is.null(res)) return(res)
    if (length(res) == 0) return(data_frame0())
    vars <- lapply(setNames(by, by), function(col) .subset2(x, col)[1])
    if (is.matrix(res)) res <- split_matrix(res)
    if (is.null(names(res))) names(res) <- paste0("V", seq_along(res))
    if (all(by %in% names(res)))
      return(data_frame0(!!!unclass(res)))
    res <- modify_list(unclass(vars), unclass(res))
    res <- res[intersect(c(fallback_order, names(res)), names(res))]
    data_frame0(!!!res)
  }
  if (all(vapply(grouping_cols, single_value, logical(1)))) {
    return(apply_fun(df))
  }
  ids <- id(grouping_cols, drop = drop)
  group_rows <- split_with_index(seq_len(nrow(df)), ids)
  result <- lapply(seq_along(group_rows), function(i) {
    cur_data <- df_rows(df, group_rows[[i]])
    apply_fun(cur_data)
  })
  vctrs::vec_rbind(!!!result)
}

single_value <- function(x, ...) {
  if(is.factor(x)) {
    identical(levels(x), "1")
  } else {
    identical(attr(x, "n"), 1L)
  }
}

stairstep <- function (data, direction = "hv") {

  direction <- rlang::arg_match0(direction, c("hv", "vh", "mid"))
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)
  if (n <= 1) {
    return(data[0, , drop = FALSE])
  }
  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2 * n]
    ys <- c(1, rep(2:n, each = 2))
  }
  else if (direction == "hv") {
    ys <- rep(1:n, each = 2)[-2 * n]
    xs <- c(1, rep(2:n, each = 2))
  }
  else if (direction == "mid") {
    xs <- rep(1:(n - 1), each = 2)
    ys <- rep(1:n, each = 2)
  }
  if (direction == "mid") {
    gaps <- data$x[-1] - data$x[-n]
    mid_x <- data$x[-n] + gaps/2
    x <- c(data$x[1], mid_x[xs], data$x[n])
    y <- c(data$y[ys])
    data_attr <- data[c(1, xs, n), setdiff(names(data), c("x", "y"))]
  }
  else {
    x <- data$x[xs]
    y <- data$y[ys]
    data_attr <- data[xs, setdiff(names(data), c("x", "y"))]
  }
  data_frame0(x = x, y = y, data_attr)
}

ensure_nonempty_data <- function (data) {

  if (empty(data)) data_frame0(group = 1, .size = 1) else data
}
