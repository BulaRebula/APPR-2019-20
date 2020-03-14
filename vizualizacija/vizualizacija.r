# 3. faza: Vizualizacija podatkov

# ZEMLJEVID SVETA S PODATKI O BREZPOSELNOSTI

brezposelnost.svet <- function(){
  podatki <- uvozi.svet()
  zaposlenost <- uvozi.zaposlenost()
  podatki2 <- merge(podatki,zaposlenost, by = c("name"), all=TRUE)
  svet <- tm_shape(podatki2) + tm_polygons("Brezposelnost")
  return(svet)
}


# HISTOGRAM ZADOVOLJSTVA PREBIVALCEV ZA LETO 2013 IN 2018

zadovoljstvo_2013 <- function(){
  ocene <- uvozi.rating()
  stare_ocene <- ocene %>% filter(leto == 2013) %>% select(Drzava, Ocena)
  histogram <- ggplot(stare_ocene, aes(x=Ocena)) +
                    geom_histogram(binwidth=1, fill="#c0392b", alpha=0.75) +
                    fte_theme() +
                    labs(title="Ocena zadovoljstva prebivalcev evropskih držav z življenjem leta 2013",
                         x="Ocena zadovoljstva", y="Število držav") +
                    scale_x_continuous(breaks = seq(0,10, by=0.5)) +
                    scale_y_continuous(breaks = seq(0,26, by=2)) + 
                    geom_hline(yintercept=0, size=0.4, color="black")
  return(histogram)
}


zadovoljstvo_2018 <- function(){
  ocene <- uvozi.rating()
  nove_ocene <- ocene %>% filter(leto == 2018) %>% select(Drzava, Ocena)
  histogram <- ggplot(nove_ocene, aes(x=Ocena)) +
    geom_histogram(binwidth=1, fill="#c0392b", alpha=0.75) +
    fte_theme() +
    labs(title="Ocena zadovoljstva prebivalcev evropskih držav z življenjem leta 2018",
         x="Ocena zadovoljstva", y="Število držav") +
    scale_x_continuous(breaks = seq(0,10, by=0.5)) +
    scale_y_continuous(breaks = seq(0,26, by=2)) + 
    geom_hline(yintercept=0, size=0.4, color="black")
  return(histogram)
}


# OSNOVNI GRAF, KI PRIKAZUJE GIBANJE POVPRECNEGA BDP V EVROPI

gibanje_BDP <- function(){
  BDP <- uvozi.BDP()
  bdp_po_letih <- aggregate(BDP$'BDP per capita', by=list(leto=BDP$leto), FUN=mean)
  bdp_po_letih$x <- as.numeric(bdp_po_letih$x)
  g <- ggplot(bdp_po_letih, aes(x=leto, y=x)) +
    geom_point(color="#c0392b") +
    ylim(20000, 30000)+
    fte_theme() +
    labs(x="Leto", y="Realni BDP v €", title="Realni povprečni BDP V € na prebivalca v Evropi")
  return(g)
}


# ZEMLJEVID EVROPE GLEDE NA BDP

zemljevid_evrope_BDP <- function(){
  evropa <- uvozi.svet() %>% filter (continent == 'Europe')
  BDP <- uvozi.BDP()
  BDP <- BDP %>% filter (leto == 2018) %>% select('Drzava', 'BDP per capita')
  podatki <- merge(y = BDP,x = evropa, by.x='name', by.y = 'Drzava')
  evropa <- tm_shape(podatki) + tm_polygons('BDP per capita')
  tmap_mode('view')
  return(evropa)
}
zemljevid_evrope_BDP()