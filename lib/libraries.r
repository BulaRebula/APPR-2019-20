library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(shiny)
library(readr)
library(tidyr)
library(dplyr)
library(tmap)
library(ggplot2)
library(grid)
library(rworldmap)



#install.packages('varhandle')
options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")


