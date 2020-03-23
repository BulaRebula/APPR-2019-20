# CLUSTER GLEDE NA ZADOVOLJSTVO PREBIVALCEV, ZAPOSLENOST, BDP TER STEVILO PREBIVALCEV

cluster_evropa <- function(){
  evropa1 <- uvozi.svet() %>% filter (continent == 'Europe')
  zadovoljstvo <- uvozi.rating() %>% filter(leto == 2018)
  brezposelnost_cluster <- uvozi.zaposlenost()
  bdp <- uvozi.BDP() %>% filter(leto == 2018) %>% select('Drzava', 'BDP per capita')
  evropa2 <- merge(x = evropa1, y = zadovoljstvo, by.y = 'Drzava', by.x = 'name')
  evropa3 <- merge(x = evropa2, y = brezposelnost_cluster, by.y = 'name', by.x = 'name')
  evropa4 <- merge(x = evropa3, y = bdp, by.y = 'Drzava', by.x = 'name')
  podatki_cluster <- evropa4 %>% filter (leto == 2018) %>% select('name', 'pop_est','BDP per capita', 'Ocena','Brezposelnost')
  podatki_cluster2 <- podatki_cluster
  podatki_cluster$geometry = NULL
  cluster <- podatki_cluster %>% select(-name) %>% scale()
  rownames(cluster) <- podatki_cluster$name
  k <- kmeans(cluster, 5, nstart=1000)
  skupine <- data.frame(name=podatki_cluster2$name, skupina=factor(k$cluster))
  podatki_za_risat <- merge(podatki_cluster2, skupine, by ='name')
  zemljevid <- tm_shape(podatki_za_risat) + tm_polygons('skupina')
  zemljevid
  tmap_mode('view')
  return(zemljevid)
}
cluster <- cluster_evropa()


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
  bdp_po_letih_slo$z <- 'slovenija'
  
  bdp_po_letih <- rbind(bdp_po_letih_eu, bdp_po_letih_slo)
  colnames(bdp_po_letih)[3] <- "Legenda"
  
  h <- ggplot(bdp_po_letih, aes(leto, x, shape=Legenda, colour=Legenda, fill=Legenda)) +
        geom_smooth(method="lm", fullrange=TRUE) +
        geom_point(size=3) +
        fte_theme() +
        xlab("Leto") +
        ylab("€") +
        ggtitle("Gibanje realnega BDP v € na prebivalca") +
        scale_x_continuous(breaks = 2013:2022) +
        expand_limits(x = 2020) +
        geom_hline(yintercept=0, size=0.4, color="black") +
        theme(legend.position="right")
  return(h)
}
napovedovanje <- napovedovanje_BDP()
