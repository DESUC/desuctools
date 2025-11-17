# Wrapper of `knitr::include_graphics` for PDF Output

Deals with URL and GIFs. If an url is passed to `path` of
[`include_graphics`](https://rdrr.io/pkg/knitr/man/include_graphics.html),
the figure is automatically downloaded and included using local relative
path. If a figure with `.gif` extension is included, a piece of text,
rather than the figure, is inserted.

## Usage

``` r
include_graphics_latex(
  path,
  alt_path = NULL,
  handler = function(path) knitr::asis_output(paste("View", tools::file_ext(path), "at",
    path)),
  ...
)
```
