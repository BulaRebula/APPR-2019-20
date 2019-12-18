# UVOZ TABELE Z ZADOVOLJSTVOM PREBIVALCEV ZA LETO 2013 IN 2018. PROBLEM JE KER ZA 2 DRZAVI NI PPODATKOV ZA 2018 IN JU BOM MORAL IZBRISATI VEN 
uvozi.rating <- function(rating) {
  stari_podatki <- read_csv2("podatki/Average_rating_1-10.csv", col_names=c("Drzava","Ocena"), skip_empty_rows=TRUE, skip=103,n_max=34, na=":",
                    locale=locale(encoding="CP1250"))
  novi_podatki <- read_csv2("podatki/Average_rating_1-10.csv", col_names=c("Drzava","Ocena"), skip_empty_rows=TRUE, skip=148,n_max=34, na=":",
                     locale=locale(encoding="CP1250")) %>% drop_na()
  stari_podatki$leto <- factor(2013, levels=c(2013, 2018))
  novi_podatki$leto <- factor(2018, levels=c(2013, 2018))
  novi_podatki$leto <- unfactor(novi_podatki$leto)
  stari_podatki$leto <- unfactor(stari_podatki$leto)
  
  #data1 <- data1[,c(1,3,2)]
  #data2 <- data2[,c(1,3,2)]
  data1 <- gather(stari_podatki, -Drzava, key=leto, value=Ocena, na.rm = TRUE)
  data2 <- gather(novi_podatki, -Drzava, key = leto, value = Ocena)
  data <- inner_join(data1,data2, by = c("Drzava"))
  data <- gather(data, -Drzava, key = "leto", value = "Ocena")
  
  
  #drzave_kjer_NA_2013 <- data1 %>% filter(is.na(Ocena) == TRUE) %>% select(Drzava)
  #drzave_kjer_NA_2018 <- data2 %>% filter(is.na(Ocena) == TRUE) %>% select(Drzava)
  #odpadle_drzave <- rbind(drzave_kjer_NA_2013, drzave_kjer_NA_2018)
  #drzave_kjer_NA_2018 <- data2 %>% filter(is.na(Ocena) == TRUE)
  #odpadle <- split(odpadle_drzave, odpadle_drzave$Drzava)
  #seznam <- as.list(odpadle_drzave)
  #seznam <- unname(seznam, force=FALSE)
  #vse_drzave <- c(data1 %>% select(Drzava))
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
rating <- uvozi.rating()
