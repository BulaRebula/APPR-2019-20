# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
#zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                             pot.zemljevida="OB", encoding="Windows-1250")
#levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
#  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
#zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
#zemljevid <- fortify(zemljevid)

# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
#  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))


# zemljevid sveta z brezposelnostjo
brezposelnost.svet <- function(){
  podatki <- World
  podatki2 <- inner_join(podatki,zaposlenost, by = c("name"))
  svet <- tm_shape(podatki2) + tm_polygons("Brezposelnost")
  svet <- svet + tm_text("iso_a3", size="AREA")
  return(svet)
}
zemljevid_brezposelnost <- brezposelnost.svet()

#Histogram zadovoljstva prebivalcev za leto 2013 in 2018
zadovoljstvo_2013 <- function(){
  ocene <- uvozi.rating()
  stare_ocene <- ocene %>% filter(leto == 2013) %>% select(Drzava, Ocena)
  hist(stare_ocene$Ocena, main = 'Razporeditev ocen zadovoljstva prebivalcev evropskih držav v letu 2013', 
       xlab = 'Ocena zadovoljstva', ylab = 'Količina', xlim = c(4,9), ylim = c(0,10), col = 'blue')
  staro_povprecje <- mean(stare_ocene$Ocena)
  abline(v = staro_povprecje, col='red')
}

zadovoljstvo_2018 <- function(){
  ocene <- uvozi.rating()
  nove_ocene <- ocene %>% filter(leto == 2018) %>% select(Drzava, Ocena)
  hist(nove_ocene$Ocena, main = 'Razporeditev ocen zadovoljstva prebivalcev evropskih držav v letu 2018', 
       xlab = 'Ocena zadovoljstva', ylab = 'Količina', xlim = c(4,9), ylim = c(0,10), col = 'blue')
  novo_povprecje <- mean(nove_ocene$Ocena)
  abline(v = novo_povprecje, col='red')
}

## analiza razvitosti drzav glede na BDP
gibanje_BDP <- function(){
  BDP <- uvozi.BDP()
  bdp_po_letih <- aggregate(BDP$'BDP per capita', by=list(leto=BDP$leto), FUN=mean)
  h <- ggplot(bdp_po_letih, aes(x = leto, y = x, group = 1)) + geom_path()
  h <- h + xlab('Leto') + ylab('realni BDP na prebivalca') + ggtitle('Gibanje realnega BDP na prebivalca v Evropi')
  print(h)
}

## zemljevid evrope glede na BDP
zemljevid_evrope_BDP <- function(){
  evropa <- World %>% filter (continent == 'Europe')
  BDP <- uvozi.BDP()
  BDP <- BDP %>% filter (leto == 2018) %>% select('Drzava', 'BDP per capita')
  podatki <- merge(y = BDP,x = evropa, by.x='name', by.y = 'Drzava')
  evropa <- tm_shape(podatki) + tm_polygons('BDP per capita')
  evropa + tm_text("iso_a3", size="AREA")
  tmap_mode('view')
  return(evropa)
  
}
