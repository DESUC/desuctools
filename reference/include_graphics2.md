# Wrapper of `knitr::include_graphics` to Deal with URLs and Invalid File Types

Deals with URL paths and invalid file types passed to `path` of
[`include_graphics`](https://rdrr.io/pkg/knitr/man/include_graphics.html).
When the output format of the R Markdown is `PDF`, and an URL is passed
to `path`, the figure is automatically downloaded from the URL and
included using the local relative path. If a figure has an invalid file
extension for PDF output (e.g. `.gif`, `.svg`), the function passed to
`handler` is used to override the default behavior: inserting figures
with
[`knitr::include_graphics`](https://rdrr.io/pkg/knitr/man/include_graphics.html).

## Usage

``` r
include_graphics2(
  path,
  alt_path = NULL,
  handler = function(path) knitr::asis_output(paste("View", tools::file_ext(path), "at",
    path)),
  ...
)
```

## Arguments

- path:

  String. Path to a figure to be included. Can be either an URL or a
  local path.

- alt_path:

  String. An alternative figure path for `path` with invalid extensions.
  In the case of PDF ("LaTeX") output, invalid extensions are `.gif`,
  `.svg`.

- handler:

  Function. A function with a single argument `path`. Used to insert
  alternative contents, such as a piece of text, when the figure cannot
  be inserted.

- ...:

  Other arguments to pass to
  [`include_graphics`](https://rdrr.io/pkg/knitr/man/include_graphics.html).

## Details

Read more about using the function at <http://bit.ly/include_graphics2>.

## Examples

``` r
png_url <- "https://commonmark.org/images/markdown-mark.png"
gif_url <- "https://media.giphy.com/media/k3dcUPvxuNpK/giphy.gif"

if (FALSE) { # \dontrun{
include_graphics2(gif_url, alt_path = png_url)
} # }
```
