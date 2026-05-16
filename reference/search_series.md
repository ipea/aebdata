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
# \donttest{
search_result <- search_series("regime de contratação")
search_result$series_title
#> [1] "Total e proporção dos vínculos públicos e dos níveis federativos, por regime de contratação (1994-2023)"

search_result <- search_series(c("remuneração", "raça"), require_all = TRUE)
search_result$series_title
#>  [1] "Remuneração líquida média mensal no Executivo civil federal ativo, por raça e escolaridade (1999-2020)"           
#>  [2] "Remuneração líquida média mensal no Executivo civil federal ativo, por raça e escolaridade (1999-2020)"           
#>  [3] "Remuneração líquida média mensal no Executivo civil federal ativo, por raça e escolaridade (1999-2020)"           
#>  [4] "Remuneração líquida média mensal no Executivo civil federal ativo, por raça e escolaridade (1999-2020)"           
#>  [5] "Remuneração líquida média mensal no Executivo civil federal ativo, por sexo e raça (1999-2020)"                   
#>  [6] "Remuneração líquida média mensal no Executivo civil federal ativo, por sexo e raça (1999-2020)"                   
#>  [7] "Remuneração líquida média mensal no Executivo civil federal ativo, por sexo e raça (1999-2020)"                   
#>  [8] "Remuneração líquida média mensal no Executivo civil federal ativo, por sexo e raça (1999-2020)"                   
#>  [9] "Distribuição da remuneração média e dos vínculos de trabalho por raça, sexo, Poder e nível federativo (2004-2022)"
#> [10] "Distribuição da remuneração média e dos vínculos de trabalho por raça, sexo, Poder e nível federativo (2004-2022)"
#> [11] "Distribuição da remuneração média e dos vínculos de trabalho por raça, sexo, Poder e nível federativo (2004-2022)"
# }
```
