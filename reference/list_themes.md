# List available themes

`list_themes()` returns a data.frame containing all available themes and
their corresponding ids.

## Usage

``` r
list_themes()
```

## Value

A data.frame

## Examples

``` r
# Get the data frame and show the theme titles
themes <- list_themes()
themes$theme_title
#>  [1] "Totais de vínculos de trabalho no funcionalismo público"
#>  [2] "Vínculos públicos por Poderes e níveis federativos"     
#>  [3] "Remunerações no setor público"                          
#>  [4] "Vínculos e remunerações no setor público, por sexo"     
#>  [5] "Ministérios e áreas de políticas no Brasil e no mundo"  
#>  [6] "Densidade dos vínculos públicos na população"           
#>  [7] "Escolaridade no setor público"                          
#>  [8] "Carreiras e ocupações"                                  
#>  [9] "Cargos de confiança no funcionalismo público"           
#> [10] "Despesas com vínculos ativos no funcionalismo público"  
#> [11] "Funcionalismo público por cor/raça"                     
#> [12] "Presidentes, partidos e coalizões"                      
#> [13] "Mapa da Defensoria Pública Estadual"                    
#> [14] "Militares na burocracia"                                
#> [15] "Desigualdades no setor público"                         
#> [16] "Organizações do Estado"                                 
```
