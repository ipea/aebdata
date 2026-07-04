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

| theme_title                                             | theme_id |
|:--------------------------------------------------------|---------:|
| Totais de vínculos de trabalho no funcionalismo público |       21 |
| Vínculos públicos por Poderes e níveis federativos      |       25 |
| Remunerações no setor público                           |       26 |
| Vínculos e remunerações no setor público, por sexo      |       28 |
| Ministérios e áreas de políticas no Brasil e no mundo   |       33 |
| Densidade dos vínculos públicos na população            |       34 |
| Escolaridade no setor público                           |       35 |
| Carreiras e ocupações                                   |       36 |
| Cargos de confiança no funcionalismo público            |       37 |
| Despesas com vínculos ativos no funcionalismo público   |       38 |
| Funcionalismo público por cor/raça                      |       41 |
| Presidentes, partidos e coalizões                       |       42 |
| Mapa da Defensoria Pública Estadual                     |       43 |
| Militares na burocracia                                 |       44 |
| Desigualdades no setor público                          |       49 |
| Organizações do Estado                                  |       50 |

The two themes to be explored here will be “*Funcionalismo público por
cor/raça*” and “*Desigualdades no setor público*”. Let’s use their
respective ids in the series listing:

``` r

list_series(theme_id = c(41,49))
```

| theme_title | theme_id | series_title | series_id |
|:---|---:|:---|---:|
| Funcionalismo público por cor/raça | 41 | DAS 5 e 6 do Executivo federal, por cor ou raça, sexo e órgão superior (2000, 2005, 2015, 2020) | 159 |
| Funcionalismo público por cor/raça | 41 | Mapa de ações afirmativas e leis de cotas raciais no serviço público | 162 |
| Funcionalismo público por cor/raça | 41 | Nota Técnica - Cor ou raça dos servidores civis ativos do Executivo federal (1999-2020) | 164 |
| Funcionalismo público por cor/raça | 41 | Percentual de vínculos em funções de Direção e Assessoramento Superior (DAS), por sexo e cor ou raça (1999-2020) | 160 |
| Funcionalismo público por cor/raça | 41 | Remuneração líquida média mensal no Executivo civil federal ativo, por raça e escolaridade (1999-2020) | 144 |
| Funcionalismo público por cor/raça | 41 | Remuneração líquida média mensal no Executivo civil federal ativo, por sexo e raça (1999-2020) | 145 |
| Funcionalismo público por cor/raça | 41 | Remuneração média mensal de vínculos públicos nos níveis federativos, por cor (2004-2021) | 230 |
| Funcionalismo público por cor/raça | 41 | Remuneração média mensal de vínculos públicos nos Poderes, por cor (2004-2021) | 229 |
| Funcionalismo público por cor/raça | 41 | Remuneração média mensal de vínculos públicos por cor (2004 - 2021) | 227 |
| Funcionalismo público por cor/raça | 41 | Remuneração média mensal de vínculos públicos por sexo e cor (2004-2021) | 240 |
| Funcionalismo público por cor/raça | 41 | Total de vínculos civis ativos no Executivo federal, por sexo e cor ou raça (1999-2024) | 152 |
| Funcionalismo público por cor/raça | 41 | Total de vínculos em funções de Direção e Assessoramento Superior (DAS), por raça (1999-2024) | 153 |
| Funcionalismo público por cor/raça | 41 | Total de vínculos por cor e nível federativo (2004-2021) | 231 |
| Funcionalismo público por cor/raça | 41 | Total de vínculos por cor e poder (2004-2021) | 232 |
| Funcionalismo público por cor/raça | 41 | Total de vínculos públicos por cor e sexo (2004-2021) | 241 |
| Funcionalismo público por cor/raça | 41 | Total e proporção de vínculos de trabalho no setor público por cor, nas regiões e nos estados (2004 - 2021) | 199 |
| Funcionalismo público por cor/raça | 41 | Vínculos civis ativos do Executivo federal em 2024, por cor ou raça, sexo e ano de ingresso no serviço público | 147 |
| Funcionalismo público por cor/raça | 41 | Vínculos civis ativos do Executivo federal, por cor ou raça e natureza jurídica do órgão (1999-2024) | 150 |
| Funcionalismo público por cor/raça | 41 | Vínculos civis ativos do Executivo federal, por cor ou raça, sexo e carreiras selecionadas (2024) | 148 |
| Funcionalismo público por cor/raça | 41 | Vínculos civis ativos do Executivo federal, por cor ou raça, sexo e escolaridade do do cargo (2000, 2010, 2020 e 2024) | 149 |
| Funcionalismo público por cor/raça | 41 | Vínculos civis ativos do Executivo federal, por cor ou raça, sexo e órgão superior (2024) | 151 |
| Desigualdades no setor público | 49 | CAP 02 Brasil: Participação relativa do emprego público municipal do SUS por grupo de municípios, segundo o tamanho da população, em anos escolhidos | 210 |
| Desigualdades no setor público | 49 | CAP 10 Índice de Gini | 275 |
| Desigualdades no setor público | 49 | CAP 10 Proporção da massa salarial apropriada pelos 10% de trabalhadores com maiores e os 10% com menores remunerações | 276 |
| Desigualdades no setor público | 49 | CAP 10 Proporção de trabalhadoras do setor público entre as 10% maiores e 10% menores remunerações | 277 |
| Desigualdades no setor público | 49 | CAP 10 Proporção de trabalhadoras e trabalhadores negros entre as 10% maiores e 10% menores remunerações | 278 |
| Desigualdades no setor público | 49 | CAP 10 Proporção de trabalhadores por poder entre os 10% com maiores e os 10% com menores remunerações | 279 |
| Desigualdades no setor público | 49 | CAP 10 Razão das remunerações médias | 274 |
| Desigualdades no setor público | 49 | DAS 5 e 6 do Executivo federal, por cor ou raça, sexo e órgão superior (2000, 2005, 2015, 2020) | 159 |
| Desigualdades no setor público | 49 | Decis, quartis e mediana das remunerações públicas, por nível federativo e poder (1985 - 2022) | 175 |
| Desigualdades no setor público | 49 | Desigualdade de Remunerações no Setor Público Brasileiro (1985-2021) | 258 |
| Desigualdades no setor público | 49 | Proporção de homens e mulheres em cargos DAS 5 e 6, por Ministérios | 132 |
| Desigualdades no setor público | 49 | Remuneração líquida média mensal no Executivo civil federal ativo, por raça e escolaridade (1999-2020) | 144 |
| Desigualdades no setor público | 49 | Remuneração líquida média mensal no Executivo civil federal ativo, por sexo e raça (1999-2020) | 145 |
| Desigualdades no setor público | 49 | Remuneração média mensal no setor público, nos Poderes e nos níveis federativos, por sexo (1985-2021) | 94 |
| Desigualdades no setor público | 49 | Total de vínculos em funções de Direção e Assessoramento Superior (DAS), por raça (1999-2024) | 153 |

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

|  ano | nome_sexo | nome_cor | remuneracao_media |
|-----:|:----------|:---------|:------------------|
| 2004 | Homem     | Preta    | 33.914.253        |
| 2004 | Mulher    | Indígena | 31.982.719        |
| 2004 | Homem     | Indígena | 4.138.483         |
| 2004 | Mulher    | Amarela  | 3.607.482         |
| 2004 | Homem     | Parda    | 36.340.293        |
| 2004 | Mulher    | Branca   | 32.386.196        |
