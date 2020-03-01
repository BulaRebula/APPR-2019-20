# UVOZ TABELE Z ZADOVOLJSTVOM PREBIVALCEV ZA LETO 2013 IN 2018. PROBLEM JE KER ZA 2 DRZAVI NI PPODATKOV ZA 2018 IN JU BOM MORAL IZBRISATI VEN 
uvozi.rating <- function(rating) {
  stari_podatki <- read_csv2("podatki/Average_rating_1-10.csv", col_names=c("Drzava","Ocena"), skip_empty_rows=TRUE, skip=104,n_max=33, na=":",
                    locale=locale(encoding="CP1250"))
  novi_podatki <- read_csv2("podatki/Average_rating_1-10.csv", col_names=c("Drzava","Ocena"), skip_empty_rows=TRUE, skip=149,n_max=35, na=":",
                     locale=locale(encoding="CP1250")) %>% drop_na()
  stari_podatki$leto <- 2013
  novi_podatki$leto <- 2018
  data1 <- stari_podatki
  data2 <- novi_podatki
  data1 <- data1[,c(1,3,2)]
  data2 <- data2[,c(1,3,2)]
  #data1 <- gather(stari_podatki, -Drzava, key=leto, value=Ocena, na.rm = TRUE)
  #data2 <- gather(novi_podatki, -Drzava, key = leto, value = Ocena)
  #data <- inner_join(data1,data2, by = c("Drzava"))
  #data <- data$Drzava[data$Drzava == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'
  #data <- gather(data, -Drzava, key = "leto", value = "Ocena")
  data <- rbind(stari_podatki, novi_podatki) %>% filter(! Drzava %in% c("Iceland", "Turkey"))
  data$Drzava[data$Drzava == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'
  #spisek_drzav_1 <- data %>% select(Drzava)
  #spisek_drzav_1 <- spisek_drzav_1[1:32,]
  #vse_drzave <- strsplit(c(novi_podatki %>% select(Drzava))," ")
  #odpad <- as.list(as.data.frame(t(odpadle_drzave)))
  #data3 <- data1[! data1$Drzava %in% "Turkey",]
  #data3 <- data3[! data1$Drzava %in% "Iceland",]
  #data4 <- data2[! data2$Drzava %in% "Turkey",]
  #data4 <- data4[! data2$Drzava %in% "Iceland",]
  #data4 <- data2[!seznam %in% data2["Drzave"],]
  
  #data <- rbind(data3,data4)
  #data <- data %>% filter(is.na(Ocena) == FALSE)
  return(data)
  }

#UVOZ TABELE Z BDP
uvozi.BDP <- function() {
  BDP_na_prebivalca <- read_csv("podatki/Main_GDP_aggregates_per_capita.csv", col_names=c("Drzava",2013:2018), skip=17,n_max=37, na=":",locale=locale(encoding="CP1250")) %>% drop_na()
  #spisek_drzav_2 <- BDP_na_prebivalca %>% select(Drzava)
  #spisek_drzav <- inner_join(spisek_drzav_1,spisek_drzav_2, by = c("Drzava"))
  #BDP <- BDP_na_prebivalca %>% filter(Drzava %in% c("spisek_drzav"))
  BDP <- gather(BDP_na_prebivalca, -Drzava, key = "leto", value = "BDP per capita")
  BDP$Drzava[BDP$Drzava == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'
  return(BDP)
}


uvozi.zaposlenost <- function(){
  link <- "https://en.wikipedia.org/wiki/List_of_countries_by_unemployment_rate"
  stran <- html_session(link) %>% read_html()
  stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[1]] %>% html_table(dec=",") %>%
    transmute(name=`Name of Countries`, Brezposelnost=parse_number(`Unemployment rate (%)`))
}

