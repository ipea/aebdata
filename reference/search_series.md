# Search for specific words

The `search_series()` function provides a quick way to access a list of
series that may be of interest, along with their respective IDs. Each
series appears according to the number of themes it is present in.

If one result should appear before the other, the result can be sorted
by using the word multiple times in the search, thus giving it greater
importance.

## Usage

``` r
search_series(words, case_insensitive = TRUE, require_all = FALSE)
```

## Arguments

- words:

  A word or an array of words to check

- case_insensitive:

  Ignore the difference between uppercase and lowercase

- require_all:

  Require all words

## Value

A data.frame

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet() && requireNamespace("stringi", quietly = TRUE)
# \donttest{
search_result <- search_series("regime de contratação")
search_result$series_title

search_result <- search_series(c("remuneração", "raça"), require_all = TRUE)
search_result$series_title
# }
}
```
