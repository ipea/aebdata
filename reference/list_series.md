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
series <- list_series(theme_id = c(42, 50))
series$series_title
#>  [1] "A genealogia e o perfil dos partidos brasileiros (1945-2024)"                                                               
#>  [2] "Partidos do gabinete, por ministério e ao longo do tempo (Brasil, 1985-2024)"                                               
#>  [3] "Presidentes, partidos e coalizões: 2. Proporcionalidade entre ministérios e cadeiras legislativas, por gabinete (1985-2024)"
#>  [4] "Trajetória da posição ideológica dos partidos no eixo esquerda-direita (1990-2021)"                                         
#>  [5] "CAP 16 Trabalhadores dos Cras e Creas, por área de formação - Brasil (2011-2017)"                                           
#>  [6] "CAP 16 Trabalhadores dos Cras e Creas, por vínculos de trabalho - Brasil (2011-2017)"                                       
#>  [7] "CAP 16 Trabalhadores nos equipamentos do Suas: rede governamental e não governamental - Brasil (2011-2017)"                 
#>  [8] "CAP 17 Estabelecimentos de saúde, segundo a natureza jurídica (2012-2021)"                                                  
#>  [9] "CAP 17 Habitantes por estabelecimentos de saúde, por região (2006-2021)"                                                    
#> [10] "CAP 17 Número de estabelecimentos que compõem o SUS, por grupo de natureza jurídica (2012-2021)"                            
#> [11] "CAP 17 Número de habitantes por hospital, por região (2006-2021)"                                                           
#> [12] "CAP 17 Total anual dos quatro tipos mais numerosos de estabelecimentos de saúde (2006-2021)"                                
#> [13] "CAP 17 Total de estabelecimentos de saúde comparados, por região (2006-2021)"                                               
#> [14] "CAP 17 Trajetória do volume das demais estabelecimentos do setor de saúde (2006-2021)"                                      
# \donttest{
# List all series and count the number of series available
all_series <- list_series()
length(unique(all_series$series_id))
#> [1] 169

# Count the number of series from Organizações do Estado theme
organizacoes <- list_series(theme_title = "Organizações do Estado")
nrow(organizacoes)
#> [1] 10
# }
```
