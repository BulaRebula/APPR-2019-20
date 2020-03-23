# Analiza podatkov s programom R, 2019/20

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2019/20

* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/BulaRebula/APPR-2019-20/master?urlpath=rstudio) RStudio

## Povezava med razvitostjo evropskih držav in zadovoljstvom njihovih državljanov

Analiziral bom povezavo med razvitostjo države ter zadovoljstvom oz. srečo njenih prebivalcev. Posebej bom obdelal ekonomske spremenljivke v državi in jih potem združil s tabelami, kjer so prebivalci držav ocenili svojo srečo in zadovoljstvo z življenjem. Analiziral bom evropske države in primerjal tudi s povprečjem Evropske unije. Vključil bom tudi primerjavo s stopnjo brezposelnosti v državi za cel svet.

Večino podatkov bom pridobil na strani Eurostat (https://ec.europa.eu/eurostat), kjer so podatki dosegljivi v formatih XLS, CSV in HTML.
Imena tabel, ki jih bom uporabil:
* Average rating of satisfaction by domain, sex, age and educational attainment level. 
(Ocena zadovoljstva z življenjem 1-10, različne starostne skupine in razdeljeno po spolu ter izobrazbi, letno (2013, 2018)
* Real GDP per capita. 
(realni BDP z osnovo 2010, realna sprememba BDP glede na prejšnje leto, letno (2013-2018)
* Total unemployment rate. 
(Brezposelnost v procentih glede na vso prebivalstvo in aktivno prebivalstvo, za vako državo najnovejši podatki)
(https://en.wikipedia.org/wiki/List_of_countries_by_unemployment_rate)
* Tabela s podatki o državah sveta(lokacija, populacija)
(Library v Rstudio)

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

*`knitr`

*`rvest`

*`gsubfn`

*`tidyr`

*`shiny`

*`readr`

*`dplyr`

*`tmap`

*`ggplot2`

*`grid`

*`rworldmap`

*`cowplot`

*`googleway`

*`ggrepel`

*`ggspatial`

*`rnaturalearth`

*`rnaturalearthdata`

*`RColorBrewer`

*`mgcv`

*`tidyverse`

*`shiny`

*`leaflet`

***