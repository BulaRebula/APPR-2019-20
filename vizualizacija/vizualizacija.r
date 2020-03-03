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
zadovoljstvo_2013()

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
zadovoljstvo_2018()
## analiza razvitosti drzav glede na BDP
gibanje_BDP <- function(){
  BDP <- uvozi.BDP()
  bdp_po_letih <- aggregate(BDP$'BDP per capita', by=list(leto=BDP$leto), FUN=mean)
  bdp_po_letih$x <- as.numeric(bdp_po_letih$x)
  #h <- ggplot(bdp_po_letih, aes(x = leto, y = x, group = 1)) + geom_path()
  #h <- h + xlab('Leto') + ylab('realni BDP na prebivalca') + ggtitle('Gibanje realnega BDP na prebivalca v Evropi')
  #print(h)
  ggplot(bdp_po_letih, aes(x=leto, y=x)) +
    geom_point(color="#c0392b") +
    #scale_x_continuous(limits=c(2010, 2020))+
    #scale_y_continuous(limits=c(20000, 30000))+
    #scale_x_date(labels = as.Date(c("2010-01-01","2020-01-01")))
    ylim(20000, 30000)+
    fte_theme() +
    labs(x="Leto", y="Realni BDP v €", title="Realni povprečni BDP na prebivalca v Evropi")
}
gibanje_BDP()
## zemljevid evrope glede na BDP
zemljevid_evrope_BDP <- function(){
  evropa <- World %>% filter (continent == 'Europe')
  BDP <- uvozi.BDP()
  BDP <- BDP %>% filter (leto == 2018) %>% select('Drzava', 'BDP per capita')
  podatki <- merge(y = BDP,x = evropa, by.x='name', by.y = 'Drzava')
  evropa <- tm_shape(podatki) + tm_polygons('BDP per capita')
  evropa
  tmap_mode('view')
  return(evropa)
}
zemljevid_evrope_BDP()

## Cluster za evropske drzave glede na zadovoljstvo, zaposlenost in BDP
cluster_evropa <- function(){
  evropa1 <- World %>% filter (continent == 'Europe')
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

## funkckcija za temo pri histogramu
fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
