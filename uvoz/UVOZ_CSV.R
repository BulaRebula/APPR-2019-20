library(readr)
library(tidyr)
library(dplyr)

# UVOZ TABELE Z ZADOVOLJSTVOM PREBIVALCEV ZA LETO 2013 IN 2018. PROBLEM JE KER ZA 2 DRZAVI NI PPODATKOV ZA 2018 IN JU BOM MORAL IZBRISATI VEN 
uvozi.rating <- function(rating) {
  data1 <- read_csv2("Average_rating_1-10.csv", col_names=c("Drzava","Ocena"), skip_empty_rows=TRUE, skip=103,n_max=34, na=":",
                    locale=locale(encoding="CP1250"))
  data2 <- read_csv2("Average_rating_1-10.csv", col_names=c("Drzava","Ocena"), skip_empty_rows=TRUE, skip=148,n_max=34, na=":",
                     locale=locale(encoding="CP1250"))
  data1$leto <- factor(2013, levels=c(2013, 2018))
  data2$leto <- factor(2018, levels=c(2013, 2018))
  data1 <- data1[,c(1,3,2)]
  data2 <- data2[,c(1,3,2)]
  data <- rbind(data1,data2)
  ali <- data %>% filter(is.na("Ocena"))
  
  return(data)
}
rating <- uvozi.rating()

