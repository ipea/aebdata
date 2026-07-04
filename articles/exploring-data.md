# Exploring The Data

In this tutorial, we will utilize just our package: aebdata. Let us
begin by loading the package:

``` r

library(aebdata)
```

Now, let us assume that you desire to explore some of the series that
are currently available. The primary function to be employed in this
scenario would be to compile a list of the themes that are accessible.

``` r

list_themes()
```

The two themes to be explored here will be “*Funcionalismo público por
cor/raça*” and “*Desigualdades no setor público*”. Let’s use their
respective ids in the series listing:

``` r

list_series(theme_id = c(41,49))
```

Looking at the series list, the two we wish to obtain data for at this
moment are “*Remuneração média mensal de vínculos públicos por sexo e
cor (2004-2021)*” and “*Total de vínculos públicos por cor e sexo
(2004-2021)*”. To retrieve the data, we will use the
[`get_series()`](https://hhmacedo.github.io/aebdata/reference/get_series.md)
function with the desired ids.

``` r

downloaded_series <- get_series(series_id = c(240, 241))
```

And now the data frame is available:

``` r

head(downloaded_series$`240`)
```
