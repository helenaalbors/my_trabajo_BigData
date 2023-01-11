library(tidyverse)
library(wbstats)
library(ggplot2)
library(gganimate)
library(rnaturalearth)
library(rnaturalearthdata)
library(wordcloud2)
library(gt)
library(gapminder)


#PROPORCIÓN DE PIB PER CÁPITA (2007)
gapminder <- gapminder::gapminder  
gdp <- gapminder %>% filter(year == 2007)

gdp1 <- gdp %>% 
  select(year, gdpPercap, country) %>% 
  mutate(year = as.numeric(year)) %>%  
  mutate(proporción = ntile 
         (gdpPercap, 4))

world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>% filter(subregion != "Antarctica") %>% filter(admin != "Greenland")
aa <- ggplot() + geom_sf(data = world) + theme_void()
world <- world %>% select(name, geometry)

gdp2 <- left_join(gdp1, world, by = c("country" = "name"))

gdp2gg <- gdp2 %>% 
  ggplot + geom_sf (data = gdp2, aes(geometry = geometry , fill = proporción)) + 
  scale_fill_viridis_c(option = "mako") + labs(title = "PIB PER CÁPITA POR PAÍSES", subtitle = "(más claro mayor proporción, oscuro menor)") 

gdp2gg


#otra forma de visualizar los datos (solo para el último año, 2007)
nombres <- gapminder %>% 
  select(year, gdpPercap, country)

nombres0 <- nombres %>% group_by(country) %>% ungroup() %>% filter(year == 2007) %>% arrange(desc(gdpPercap))

nombrespaises <- nombres0 %>% mutate(word = country, freq = gdpPercap)
nombrespaises0 <- nombrespaises %>% select(word, freq)


wordcloud2(data = nombrespaises0, size = 0.1)


#tabla
gdptabla <- gapminder %>% 
  select(year, gdpPercap, country)

gdptabla1 <- gdptabla %>%
  tibble::as_tibble() %>%
  DT::datatable(filter = 'top', options = list(pageLength = 7, autoWidth = TRUE))

gdptabla1







#CANTIDAD DE POBLACIÓN POR PAÍSES

#AGRUPANDO EN UNA TABLA PARA UNA MEJOR VISIÓN DE LOS DATOS
pop <- gapminder %>% 
  select(year, pop, country)

pop00 <- pop %>% filter(country == "Spain"|
                        country == "France"|
                        country == "United Kingdom"|
                        country == "Italy")

banderas <- c("https://www.comprarbanderas.es/images/banderas/400/60-espana-sin-escudo_400px.jpg", "https://upload.wikimedia.org/wikipedia/commons/c/c3/Flag_of_France.svg", "https://www.banderas-mundo.es/data/flags/w1600/gb.png", "https://media.istockphoto.com/id/1063640060/es/vector/vector-bandera-de-italia.jpg?s=612x612&w=0&k=20&c=gZUTN3jEmTjiqIJWbW6oOZjcH55MzsjsLMBs74-R1Lo=")

tablapob <- cbind(pop00, banderas)

tablapob <- tablapob %>% gt()

tablapob %>% gt::text_transform(locations = cells_body(columns = c("")), fn = function(x){gt::web_image(x, height = 25)})


#otra forma de visualizar los datos (solo para el último año, 2007)
paises <- pop %>% group_by(country) %>% ungroup() %>% filter(year == 2007) %>% arrange(desc(pop))

nombrespaises <- paises %>% mutate(word = country, freq = pop)
nombrespaises0 <- nombrespaises %>% select(word, freq)


wordcloud2(data = nombrespaises0, size = 0.5)



#COMO ESTÁ REPARTIDA LA POBLACIÓN POR EDADES
pob <- wbsearch(pattern = "total population", field = "indicator")
pob0 <- wb(country = c( "ESP", "FRA", "GBR", "ITA", "CHN"), indicator = c("SP.POP.0014.TO.ZS", "SP.POP.1564.TO.ZS", "SP.POP.65UP.TO.ZS"), startdate = 2000, enddate = 2022, POSIXct = TRUE)

ggpob <- ggplot(pob0, aes(x = date, y = value, color = indicatorID, group = indicatorID)) + geom_point() + geom_line() + 
  theme(panel.background = element_rect(fill = "lightyellow", colour = "white"), plot.background = element_rect(fill = "navajowhite", colour = "bisque")) + 
  labs(title = "% DE POBLACIÓN TOTAL", subtitle = "(según rango de edades)", x = "período", y = "% de población total", colour = "rango edades") +
  facet_wrap(~country) + 
  scale_color_discrete(labels = c("0-14 años", "15-64 años", ">= 65 años")) + 
  scale_x_discrete(breaks = c("2000", "2005", "2010", "2015", "2020"))
ggpob



#ESPERANZA DE VIDA PAÍSES EUROPEOS
lifeexp <- wbsearch(pattern = "Life expectancy", field = "indicator")
lifeexp0 <- wb(indicator = c("SP.DYN.LE00.IN"))
lifeexp1 <- lifeexp0 %>% filter(country == "Spain" |
                                country == "France" |
                                country == "Italy" |
                                country == "United Kingdom" |
                                country == "China") %>%
  filter(date >= 2000)

gglifeexp <- ggplot(data = lifeexp1, aes(x = date, y = value, colour = country)) + geom_point() + geom_line(aes(group = country)) + labs(title = "ESPERANZA DE VIDA", subtitle = "en países europeos", x = "períodos", y = "nº años", colour = "países") + scale_x_discrete(breaks = c("2000", "2005", "2010", "2015", "2020")) + 
  theme(panel.background = element_rect(fill = "seashell", colour = "white"), plot.background = element_rect(fill = "mistyrose", colour = "bisque"))

gglifeexp


#SI DIVIDIMOS POR GÉNERO
lifeexp01 <- wb(country = c( "ESP", "FRA", "GBR", "ITA", "CHN"), 
                  indicator =c("SP.DYN.LE00.FE.IN", "SP.DYN.LE00.MA.IN"), startdate = 2000, enddate = 2022, POSIXct = TRUE)

por_géneros <- ggplot(lifeexp01, aes(x = date, y = value, color = indicatorID, group = indicatorID)) + geom_point() + geom_line() + 
  theme(panel.background = element_rect(fill = "seashell", colour = "white"), plot.background = element_rect(fill = "mistyrose", colour = "bisque")) + 
  labs(title = "ESPERANZA DE VIDA POR GÉNEROS", x = "período", y = "años", colour = "género") + 
  facet_wrap(~country) + 
  scale_color_discrete(labels = c("mujeres", "hombres")) + 
  scale_x_discrete(breaks = c("2000", "2005", "2010", "2015", "2020"))

por_géneros



#ESPERANZA DE VIDA ESCOLAR
school <- wb(indicator = c("SE.SCH.LIFE", "SE.SCH.LIFE.FE", "SE.SCH.LIFE.MA"))
school1 <- school %>% filter(country == "Spain" |
                                  country == "France" |
                                  country == "Italy" |
                                  country == "United Kingdom" |
                                  country == "China") %>%
  filter(date >= 2000)

ggschool <- 
  ggplot() +
  geom_col(data = school1, aes(x = indicatorID, y = value, fill = `country`), position = "dodge") +
  scale_fill_brewer(palette = "Blues") + theme(axis.text.x = element_text(colour = "black"),
panel.background = element_rect(fill = "lightyellow1"),
plot.background = element_rect(fill = "lightyellow2")) + labs(title = "ESPERANZA DE VIDA ESCOLAR", x = "indicador", y = "nº años", caption = "Fuente: Elaboración propia con datos del Banco de España")+ 
  scale_x_discrete(labels = c("ambos", "mujeres", "hombres")) + 
  theme(legend.position = "bottom", legend.direction = "horizontal")

ggschool



#DEUDA
debt0 <- wbsearch(pattern = "gdp", field = "indicator")

debt <- wb(indicator = c("GC.DOD.TOTL.GD.ZS"))

debt01 <- debt %>% filter(country == "Spain" |
                               country == "France" |
                               country == "Italy" |
                               country == "United Kingdom" |
                               country == "China") %>%
  filter(date >= 2000) %>%
  select(country, indicator, value, date)

ggdebt <- ggplot(debt01, aes(x = date, y = value, color = country)) + geom_point() + geom_line(aes(group = country)) + 
  theme(panel.background = element_rect(fill = "oldlace", colour = "white"), plot.background = element_rect(fill = "powderblue", colour = "bisque")) + 
  labs(title = "DEUDA TOTAL, % PIB", subtitle = "(por países)", x = "período", y = "deuda", colour = "países") + scale_x_discrete(breaks = c("2000", "2005", "2010", "2015", "2020")) 
  
ggdebt


#IDE
#inflows
otros <- wbsearch(pattern = "investment", field = "indicator")
IDE0 <- wb(indicator = c("BX.KLT.DINV.WD.GD.ZS"))
IDEinflows <- IDE0 %>% filter(country == "Spain" |
                            country == "France" |
                            country == "Italy" |
                            country == "United Kingdom" |
                            country == "China") %>%
  filter(date >= 2000) %>%
  select(country, indicator, value, date)

ggIDEin <- ggplot(IDEinflows, aes(x = date, y = value, color = country)) + geom_point() + geom_line(aes(group = country)) + 
  theme(panel.background = element_rect(fill = "oldlace", colour = "white"), plot.background = element_rect(fill = "powderblue", colour = "bisque")) + 
  labs(title = "IDE, entradas netas, % PIB", subtitle = "(por países)", x = "período", y = "Inv. Directa Extr.", colour = "países") + scale_x_discrete(breaks = c("2000", "2005", "2010", "2015", "2020")) 

ggIDEin

#outflows
otros <- wbsearch(pattern = "investment", field = "indicator")
IDE1 <- wb(indicator = c("BM.KLT.DINV.WD.GD.ZS"))
IDEoutflows <- IDE1 %>% filter(country == "Spain" |
                               country == "France" |
                               country == "Italy" |
                               country == "United Kingdom" |
                               country == "China") %>%
  filter(date >= 2000) %>%
  select(country, indicator, value, date)

ggIDEout <- ggplot(IDEoutflows, aes(x = date, y = value, color = country)) + geom_point() + geom_line(aes(group = country)) + 
  theme(panel.background = element_rect(fill = "oldlace", colour = "white"), plot.background = element_rect(fill = "powderblue", colour = "bisque")) + 
  labs(title = "IDE, salidas netas, % PIB", subtitle = "(por países)", x = "período", y = "Inv. Directa Extr.", colour = "países") + scale_x_discrete(breaks = c("2000", "2005", "2010", "2015", "2020")) 

ggIDEout



#paro
otros1 <- wbsearch(pattern = "unemployment", field = "indicator")
paro <- wb(indicator = c("SL.UEM.TOTL.NE.ZS"))
paro0 <- paro %>% filter(country == "Spain" |
                                 country == "France" |
                                 country == "Italy" |
                                 country == "United Kingdom" |
                                 country == "China") %>%
  filter(date >= 2000) %>%
  select(country, indicator, value, date)

ggparo <- ggplot(paro0, aes(x = date, y = value, color = country)) + geom_point() + geom_line(aes(group = country)) + 
  theme(panel.background = element_rect(fill = "ivory", colour = "white"), plot.background = element_rect(fill = "lightcyan1", colour = "bisque")) + 
  labs(title = "% DESEMPLEADOS", subtitle = "(del total de mano de obra)", x = "período", y = "% parados", colour = "países") + scale_x_discrete(breaks = c("2000", "2005", "2010", "2015", "2020")) 

ggparo


#ocupados
otros2 <- wbsearch(pattern = "employment", field = "indicator")
ocupados <- wb(indicator = c("SL.UEM.TOTL.NE.ZS"))
ocupados0 <- ocupados %>% filter(country == "Spain" |
                           country == "France" |
                           country == "Italy" |
                           country == "United Kingdom" |
                           country == "China") %>%
  filter(date >= 2000) %>%
  select(country, indicator, value, date) 
ocupados1 <- ocupados0 %>% mutate(ocup = 100 - value) %>% select(country, indicator, ocup, date)

ggocup <- ggplot(activos1, aes(x = date, y = ocup, color = country)) + geom_point() + geom_line(aes(group = country)) + 
  theme(panel.background = element_rect(fill = "ivory", colour = "white"), plot.background = element_rect(fill = "lightcyan1", colour = "bisque")) + 
  labs(title = "% OCUPADOS", subtitle = "(del total de mano de obra)", x = "período", y = "% ocupados", colour = "países") + scale_x_discrete(breaks = c("2000", "2005", "2010", "2015", "2020")) + ylim(ymin = 70, ymax = 100)

ggocup
