# CLUSTER GLEDE NA ZADOVOLJSTVO PREBIVALCEV, ZAPOSLENOST, BDP TER STEVILO PREBIVALCEV

cluster_evropa <- function(){
  evropa1 <- uvozi.svet() %>% filter (continent == 'Europe')
  zadovoljstvo <- uvozi.rating()
  brezposelnost <- uvozi.zaposlenost()
  bdp <- uvozi.BDP() %>% filter(leto == 2018) %>% select('Drzava', 'BDP per capita')
  evropa2 <- inner_join(x = evropa1, y = zadovoljstvo, by = c('sovereignt'='Drzava'))
  evropa3 <- inner_join(x = evropa2, y = brezposelnost, by = c('sovereignt'='name'))
  evropa4 <- inner_join(x = evropa3, y = bdp, by = c('sovereignt'='Drzava'))
  podatki_cluster <- evropa4 %>% filter (leto == 2018) %>% select('sovereignt', 'pop_est','BDP per capita', 'Ocena','Brezposelnost')
  podatki_cluster2 <- podatki_cluster
  podatki_cluster$geometry = NULL
  cluster <- podatki_cluster %>% select(-sovereignt) %>% scale()
  rownames(cluster) <- podatki_cluster$sovereignt
  k <- kmeans(cluster, 5, nstart=1000)
  skupine <- data.frame(sovereignt=podatki_cluster2$sovereignt, skupina=factor(k$cluster))
  podatki_za_risat <- merge(podatki_cluster2, skupine, by ='sovereignt')
  zemljevid <- tm_shape(podatki_za_risat) + tm_polygons('skupina')
  zemljevid
  tmap_mode('view')
  return(zemljevid)
}


# GRAF KI PRIKAZUJE GIBANJE REALNEGA BDP SLOVENIJE TER EVROPE NA PREBIVALCA IN NAPOVEDUJE PRIHODNOST

napovedovanje_BDP <- function(){
  BDP <- uvozi.BDP()
  bdp_po_letih_eu <- aggregate(BDP$'BDP per capita', by=list(leto=BDP$leto), FUN=mean)
  bdp_po_letih_eu$x <- as.numeric(bdp_po_letih_eu$x)
  bdp_po_letih_eu$leto <- as.numeric(bdp_po_letih_eu$leto)
  bdp_po_letih_eu$z <- 'evropa'
  
  bdp_po_letih_slo <- BDP %>% filter(Drzava == 'Slovenia') %>% select('leto', 'BDP per capita')
  bdp_po_letih_slo <- bdp_po_letih_slo %>% rename(x = 'BDP per capita')
  bdp_po_letih_slo$x <- as.numeric(bdp_po_letih_slo$x)
  bdp_po_letih_slo$leto <- as.numeric(bdp_po_letih_slo$leto)
  bdp_po_letih_slo$z <- 'slovenia'
  
  bdp_po_letih <- rbind(bdp_po_letih_eu, bdp_po_letih_slo)
  
  h <- ggplot(bdp_po_letih, aes(leto, x, shape=z, colour=z, fill=z)) +
    geom_smooth(method="lm") +
    geom_point(size=3) +
    fte_theme() +
    xlab("Leto") +
    ylab("BDP na prebivalca v €") +
    ggtitle("Gibanje realnega BDP na prebivalca") + 
    scale_x_continuous(breaks = 2013:2020)
  return(h)
}