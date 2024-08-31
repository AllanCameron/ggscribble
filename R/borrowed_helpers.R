
`%||%` <- function (x, y) {
  if (rlang::is_null(x))
    y
  else x
}

unique0 <- function (x, ...) if (is.null(x)) x else vctrs::vec_unique(x, ...)

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
