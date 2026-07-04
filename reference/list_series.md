# List created series

List all series from the Atlas, regardless of whether they have data
available for download or not.

## Usage

``` r
list_series(theme_id = NULL, theme_title = NULL)
```

## Arguments

- theme_id, theme_title:

  Optional parameters that can be used individually or combined to
  filter the selected themes.

## Value

A data.frame

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
series <- list_series(theme_id = c(42, 50))
series$series_title
# \donttest{
# List all series and count the number of series available
all_series <- list_series()
length(unique(all_series$series_id))

# Count the number of series from Organizações do Estado theme
organizacoes <- list_series(theme_title = "Organizações do Estado")
nrow(organizacoes)
# }
}
```
