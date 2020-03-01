# 3. faza: Vizualizacija podatkov

World <- ne_countries(scale = "medium", returnclass = "sf")
# zemljevid sveta z brezposelnostjo
brezposelnost.svet <- function(){
  podatki <- World
  zaposlenost <- uvozi.zaposlenost()
  podatki2 <- merge(podatki,zaposlenost, by = c("name"), all=TRUE)
  svet <- tm_shape(podatki2) + tm_polygons("Brezposelnost")
  svet <- svet + tm_text("iso_a3", size="AREA")
  svet
}
brezposelnost.svet()

#Histogram zadovoljstva prebivalcev za leto 2013 in 2018
zadovoljstvo_2013 <- function(){
  ocene <- uvozi.rating()
  stare_ocene <- ocene %>% filter(leto == 2013) %>% select(Drzava, Ocena)
  hist(stare_ocene$Ocena, main = 'Razporeditev ocen zadovoljstva prebivalcev evropskih držav v letu 2013', 
       xlab = 'Ocena zadovoljstva', ylab = 'Količina', xlim = c(4,9), ylim = c(0,10), col = 'blue')
  staro_povprecje <- mean(stare_ocene$Ocena)
  abline(v = staro_povprecje, col='red')
}
zadovoljstvo_2013()
zadovoljstvo_2018 <- function(){
  ocene <- uvozi.rating()
  nove_ocene <- ocene %>% filter(leto == 2018) %>% select(Drzava, Ocena)
  hist(nove_ocene$Ocena, main = 'Razporeditev ocen zadovoljstva prebivalcev evropskih držav v letu 2018', 
       xlab = 'Ocena zadovoljstva', ylab = 'Količina', xlim = c(4,9), ylim = c(0,10), col = 'blue')
  novo_povprecje <- mean(nove_ocene$Ocena)
  abline(v = novo_povprecje, col='red')
}
zadovoljstvo_2018()
## analiza razvitosti drzav glede na BDP
gibanje_BDP <- function(){
  BDP <- uvozi.BDP()
  bdp_po_letih <- aggregate(BDP$'BDP per capita', by=list(leto=BDP$leto), FUN=mean)
  h <- ggplot(bdp_po_letih, aes(x = leto, y = x, group = 1)) + geom_path()
  h <- h + xlab('Leto') + ylab('realni BDP na prebivalca') + ggtitle('Gibanje realnega BDP na prebivalca v Evropi')
  print(h)
}
gibanje_BDP()
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
