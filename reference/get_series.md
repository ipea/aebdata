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
# Get the series 230 and print the head
serie_230 <- get_series(series_id = 230)
head(serie_230)
#>    ano nivel_federativo cor rem_media nome_cor nome_nivel_federativo
#> 1 2004                E   1  4519.025 Indígena              Estadual
#> 2 2004                E   2  4031.816   Branca              Estadual
#> 3 2004                E   4  3691.235    Preta              Estadual
#> 4 2004                E   6  4256.041  Amarela              Estadual
#> 5 2004                E   8  3642.660    Parda              Estadual
#> 6 2004                F   1  9856.214 Indígena               Federal

# Get the series from 230 to 232 and print the head of the 232
# \donttest{
series <- get_series(series_id = 230:232)
head(series[["232"]])
#>       poder     cor  ano  total
#> 1 Executivo Amarela 2021 102071
#> 2 Executivo Amarela 2020 102398
#> 3 Executivo Amarela 2019 106294
#> 4 Executivo Amarela 2018 109614
#> 5 Executivo Amarela 2017 113781
#> 6 Executivo Amarela 2016 112875
# }
```
