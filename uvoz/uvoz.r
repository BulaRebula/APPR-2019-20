# UVOZ TABELE Z ZADOVOLJSTVOM PREBIVALCEV ZA LETO 2013 IN 2018. PROBLEM JE KER ZA 2 DRZAVI NI PPODATKOV ZA 2018 IN JU BOM MORAL IZBRISATI VEN

uvozi.rating <- function() {
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
  data <- rbind(data1, data2) %>% filter(! Drzava %in% c("Iceland", "Turkey"))
  data$Drzava[data$Drzava == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'
  return(data)
}


# UVOZ TABELE Z BDP

uvozi.BDP <- function() {
  BDP_na_prebivalca <- read_csv("podatki/Main_GDP_aggregates_per_capita.csv", col_names=c("Drzava",2013:2018), skip=17,n_max=37, na=":",locale=locale(encoding="CP1250")) %>% drop_na()
  BDP <- gather(BDP_na_prebivalca, -Drzava, key = "leto", value = "BDP per capita")
  BDP$Drzava[BDP$Drzava == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'
  return(BDP)
}


# UVOZ TABELE S PODATKI O BREZPOSELNOSTI ZA CEL SVET

uvozi.zaposlenost <- function(){
  link <- "https://en.wikipedia.org/wiki/List_of_countries_by_unemployment_rate"
  stran <- html_session(link) %>% read_html()
  stran <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% .[[1]] %>% html_table(dec=",") %>%
    transmute(name=`Name of Countries`, Brezposelnost=parse_number(`Unemployment rate (%)`))
  return(stran)
}

# UVOZ PODATKOV O SVETU (uporabljeno za zemljevid)

uvozi.svet <- function(){
  World <- ne_countries(scale = "medium", returnclass = "sf")
  return(World)
}


# FUNCKIJA ZA TEMO PRI VIZUALIZACIJI

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