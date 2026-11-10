# include_graphics2 --------------------------------------------------------

#' Wrapper of \code{knitr::include_graphics} to Deal with URLs and Invalid File Types
#'
#' Deals with URL paths and invalid file types passed to \code{path}
#' of \code{\link[knitr]{include_graphics}}. When the output format
#' of the R Markdown is \code{PDF}, and an URL is passed to
#' \code{path}, the figure is automatically downloaded from the URL
#' and included using the local relative path.
#' If a figure has an invalid file extension for PDF output
#' (e.g. \code{.gif}, \code{.svg}), the function passed to
#' \code{handler} is used to override the default behavior:
#' inserting figures with \code{knitr::include_graphics}.
#'
#' @details Read more about using the function at
#'   \url{http://bit.ly/include_graphics2}.
#'
#' @param path String. Path to a figure to be included. Can be either
#'   an URL or a local path.
#' @param alt_path String. An alternative figure path for \code{path}
#'   with invalid extensions. In the case of PDF ("LaTeX") output,
#'   invalid extensions are \code{.gif}, \code{.svg}.
#' @param handler Function. A function with a single argument \code{path}.
#'   Used to insert alternative contents, such as a piece of text,
#'   when the figure cannot be inserted.
#' @param ... Other arguments to pass to
#'   \code{\link[knitr]{include_graphics}}.
#'
#' @examples
#' png_url <- "https://commonmark.org/images/markdown-mark.png"
#' gif_url <- "https://media.giphy.com/media/k3dcUPvxuNpK/giphy.gif"
#'
#' \dontrun{
#' include_graphics2(gif_url, alt_path = png_url)
#' }
#'
#' @export
include_graphics2 <- function(
  path,
  alt_path = NULL,
  handler = function(path)
    knitr::asis_output(paste("View", tools::file_ext(path), "at", path)),
  ...
) {
  if (knitr::is_latex_output()) {
    return(include_graphics_latex(path, alt_path, handler, ...))
  } else {
    return(knitr::include_graphics(path, ...))
  }
}

#' Wrapper of \code{knitr::include_graphics} for PDF Output
#'
#' Deals with URL and GIFs. If an url is passed to
#' \code{path} of \code{\link[knitr]{include_graphics}},
#' the figure is automatically downloaded and included
#' using local relative path. If a figure with \code{.gif}
#' extension is included, a piece of text, rather than the
#' figure, is inserted.
#'
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom utils download.file
#' @importFrom knitr current_input
#' @keywords internal
include_graphics_latex <- function(
  path,
  alt_path = NULL,
  handler = function(path)
    knitr::asis_output(paste("View", tools::file_ext(path), "at", path)),
  ...
) {
  # Estados posibles: "valida" | "alternativa" | "sin_alternativa".
  estado <- latex_img_estado(path, alt_path)

  if (estado == "sin_alternativa") {
    return(handler(path))
  }
  if (estado == "alternativa") {
    path <- alt_path
  }

  # URL: se descarga la figura y se usa la ruta local.
  if (grepl("^https?://", path)) {
    dir_path <- paste0(
      "downloadFigs4latex_",
      file_path_sans_ext(current_input())
    )
    if (!dir.exists(dir_path)) dir.create(dir_path)
    file_path <- paste0(
      dir_path,
      "/",
      knitr::opts_current$get()$label,
      ".",
      file_ext(path)
    )
    download.file(path, destfile = file_path)
    path <- file_path
  }

  # Insert Figure
  return(knitr::include_graphics(path, ...))
}

# Resuelve qué figura se puede incluir en salida LaTeX:
# - "valida": `path` tiene extensión soportada.
# - "alternativa": `path` no es soportado y se usa `alt_path`.
# - "sin_alternativa": `path` no es soportado y no hay `alt_path`; se usa `handler`.
latex_img_estado <- function(path, alt_path = NULL) {
  if (!inval_latex_img(path)) {
    return("valida")
  }
  if (is.null(alt_path)) {
    return("sin_alternativa")
  }
  stopifnot(!inval_latex_img(alt_path))
  "alternativa"
}

inval_latex_img <- function(path) {
  invalid_ext <- c("svg", "SVG", "GIF", "gif")
  return(tools::file_ext(path) %in% invalid_ext)
}
