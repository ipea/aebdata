# Get series values

The function `get_series()` is the main function of this package. Its
goal is to facilitate direct access to the data published in the Atlas
do Estado Brasileiro so that the user can work with them as they wish.

## Usage

``` r
get_series(series_id = NULL, series_title = NULL)
```

## Arguments

- series_id, series_title:

  The series ids or titles to download

## Value

A data.frame or a list containing the data from the series

## Details

If the parameter used is for just one series, the result will be a
data.frame containing the requested information. Now, if the parameter
refers to more than one series, the return will be a list of
data.frames, with each data.frame corresponding to a series.

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
# Get the series 230 and print the head
serie_230 <- get_series(series_id = 230)
head(serie_230)

# Get the series from 230 to 232 and print the head of the 232
# \donttest{
series <- get_series(series_id = 230:232)
head(series[["232"]])
# }
}
```
