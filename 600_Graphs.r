# 01/04/25

if(!exists("meteo")) meteo <- readRDS("_data/meteo.rds")



# > Prepare data < --------------------------------------------------------

df.gra <- meteo %>% 
    filter(anno == 2019) %>% 
    group_by(data) %>% 
    summarise(t.min = min(t,  na.rm = T),
              t.med = mean(t, na.rm = T),
              t.max = max(t,  na.rm = T))



# > Grafico base < --------------------------------------------------------

df.gra %>% 
  ggplot(aes(x = data, y = t.med)) + geom_line(na.rm = T, color = "blue", alpha = .5, size = 1) +  # alpha è la trasparenza
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +                                      # date_breaks indica l'intervallo di data. %b indica l'abbreviazione del mese a 3 caratteri, se è %B mette il nome completo del mese
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +                                  # limits minimo 0 massimo 30; seq da 0 a 30 con passo 5
  theme_bw() +                                                                                     # bw è bianco e nero
  theme(plot.title    = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption  = element_text(hjust = 1),
        axis.text.x   = element_text(angle = 90, vjust = -1)) +                                    # vjust è verticale, hjust è orizzontale, serve per dare spazio tra etichette e asse
  labs(title    = "Temperatura media giornaliera",
       subtitle = "- Anno 2019 -",
       caption  = "Laboratorio di R 2025",
       x        = "Mese",
       y        = "°C")



# > Istogramma < ----------------------------------------------------------

# uso le funzioni che ho fatto in precedenza

fn.met.day(df.meteo = meteo) %>% 
  filter(anno == "2021" & id == "DSA001") %>%  # %>% distinct(anno, id)   # distinct serve per vedere se ho effettivamente l'anno che voglio
  ggplot(aes(x = t.med)) + geom_histogram(binwidth = 5, color = "black", fill = "blue", alpha = .25) +  # color è il colore del bordo (alpha si riferisce al fill)
  scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5)) +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, 10)) +
  theme_bw()



# > Geom_bar < ------------------------------------------------------------

fn.met.day(df.meteo = meteo, lbl.month = T) %>% 
  filter(anno == "2021" & id == "DSA001") %>% 
  select(-id, -anno) %>% 
  group_by(lbl) %>% summarise(t.med = mean(t.med)) %>% ungroup() %>% 
  mutate(lbl = factor(lbl, levels = month.abb)) %>%                                            # vogliamo ordinare i mesi non in ordine alfabetico
  # grafico base
  ggplot(aes(x = lbl, y = t.med)) + geom_bar(stat = "identity", fill = "grey", alpha = .8) +   # bisogna sempre mettere identity
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  # valori
  geom_text(aes(label = scales::number(t.med, accuracy = .2)),  # due decimali (per un decimale accuracy = 1L)
            position  = "stack",
            vjust     = +2.1,          # per mettere i numeri più in basso (+), per metterli più in alto (-)
            col       = "firebrick",
            size      = 3) +
  # tema
  theme_bw() + 
  labs(x = NULL, y = "°C")



# grafico rovesciato che mette a confronto tutte le stazioni a luglio

fn.met.day(df.meteo = meteo, lbl.month = T) %>% 
  filter(anno == "2021" & mese == "07") %>% 
  group_by(id) %>% summarise(r.mese = sum(r)) %>% 
  arrange(desc(r.mese)) %>% 
  ggplot(aes(x = r.mese, y = reorder(id, r.mese, decreasing = F))) + geom_bar(stat = "identity",
                                                                              fill = colorRampPalette(c("#000066", "#99ccff"))(18)) +
  scale_x_continuous(limits = c(0, 140), breaks = seq(0, 140, 20)) +
  #scale_y_discrete(limits = rev) +  # rev lo uso per cambiare l'ordine dei valori dell'asse y (in ordine crescente dall'alto)
  theme_bw() +
  labs(x = "mm di pioggia accumulati", y = NULL)
  



# . Sommatoria termica ----------------------------------------------------

# 03/04/25

df.gra <- fn.met.day(lbl.month = T, fun.pipe = T) %>%   # fun.pipe=T: i dati sono usati all'interno di una pipe
  filter(id == "DSA001" &
           between(x     = data,
                   left  = ymd("2020-10-15"),
                   right = ymd("2021-06-15"))) %>% 
  select(data, t.med) %>%  
  fn.add.sommter()                                      # anche se il df.meteo di questa funzione è NULL prende quello della funzione iniziale perché è dentro al pipe


range(df.gra$som.ter)  # per fare un grafico standard
y.min  <- 0
y.max  <- 2500
y.step <- 250


df.gra %>%                
  ggplot(aes(data, som.ter)) + geom_line(color = "blue", linewidth = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(y.min, y.max), breaks = seq(y.min, y.max, y.step), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  geom_vline(xintercept = ymd("2020-12-31"), linetype = "dotted", linewidth = 1, color = "red") +
  annotate(geom = "text", x = ymd("2020-12-15"), y = 100, label = "2020") +
  annotate(geom = "text", x = ymd("2021-01-15"), y = 100, label = "2021") +
  annotate(geom = "rect", xmin = ymd("2021-01-15"), xmax = ymd("2021-03-14"), ymin = -Inf, ymax = +Inf, fill = "forestgreen", alpha = .6) +
  annotate(geom = "rect", xmin = ymd("2021-03-15"), xmax = ymd("2021-04-14"), ymin = -Inf, ymax = +Inf, fill = "forestgreen", alpha = .4) +
  annotate(geom = "rect", xmin = ymd("2021-04-15"), xmax = ymd("2021-05-01"), ymin = -Inf, ymax = +Inf, fill = "forestgreen", alpha = .2) +
  annotate(geom = "rect", xmin = ymd("2021-05-02"), xmax = ymd("2021-05-15"), ymin = -Inf, ymax = +Inf, fill = "forestgreen", alpha = .1) +
  annotate(geom = "rect", xmin = ymd("2021-05-16"), xmax = ymd("2021-06-15"), ymin = -Inf, ymax = +Inf, fill = "yellow", alpha = .2) +
  annotate(geom = "text", x = ymd("2021-01-31"), y = 1000, label = "Accestimento") +
  annotate(geom = "text", x = ymd("2021-03-25"), y = 1500, label = "Levata") +
  annotate(geom = "text", x = ymd("2021-04-25"), y = 1800, label = "Spigatura") +
  annotate(geom = "text", x = ymd("2021-05-10"), y = 2100, label = "Fioritura") +
  annotate(geom = "text", x = ymd("2021-05-30"), y = 2450, label = "Maturazione") +
  theme_bw()



# . Pioggia cumulata ------------------------------------------------------

df.rain <- fn.met.day() %>% 
  filter(id == "DSA021" &
          between(data, left = dmy("01-11-2019"), right = dmy("31-10-2020"))) %>% 
  select(data, r) %>% 
  mutate(r.cum = cumsum(r),
         r.fra = r.cum / sum(r))


# per aggiungere soglie percentuali al grafico
which.min(abs(df.rain$r.fra - 0.10))  # qual è il valore assoluto più basso del nuovo vettore che ottengo andando a togliere il valore di soglia 0.10 ai valori osservati? Dà il numero della riga

df.rain[c(1, 3, 4, 18), ]             # dammi le righe selezionate e tutte le colonne

DescTools::Closest(x = df.rain$r.fra, a = .10)                          # questo restituisce il valore che si avvicina di più alle mie condizioni
which(df.rain$r.fra == DescTools::Closest(x = df.rain$r.fra, a = .10))  # questo restituisce il numero di riga


df.pct <- df.rain[c(which.min(abs(df.rain$r.fra - 0.10)),
                    which.min(abs(df.rain$r.fra - 0.25)),
                    which.min(abs(df.rain$r.fra - 0.50)),
                    which.min(abs(df.rain$r.fra - 0.75)),
                    which.min(abs(df.rain$r.fra - 0.90))), ] %>% 
  tibble() %>% 
  rename(x = data, y = r.fra) %>% 
  mutate(lbl = paste0(round(y * 100, 0), "%")) %>% 
  select(x, y, lbl)


df.rain %>% 
  ggplot(aes(data, r.fra)) + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
  geom_vline(xintercept = dmy("31-12-2019"), linetype = "dotted", linewidth = 1, color = "red") +
  annotate(geom = "rect", xmin = dmy("27-12-2019"), xmax = dmy("04-01-2020"), ymin = -Inf, ymax = +Inf, fill = "forestgreen", alpha = .2) +
  annotate(geom = "text", x = dmy("19-12-2019"), y = .1, label = "2019", color = "forestgreen") +
  annotate(geom = "text", x = dmy("13-01-2020"), y = .1, label = "2020", color = "forestgreen") +
  annotate(geom = "text", x = df.rain$data[1], y = df.pct$y, label = df.pct$lbl, color = "darkorange", size = 3.5) +
  annotate(geom = "segment", x = df.rain$data[7], xend = df.pct$x, y = df.pct$y, yend = df.pct$y, color = "darkorange", linetype = "dotted", size = .7) +
  annotate(geom = "segment", x = df.pct$x, xend = df.pct$x, y = 0, yend = df.pct$y, color = "darkorange", linetype = "dotted", size = .7) +
  theme_bw()



# . Mappe -----------------------------------------------------------------

df.punti <- read.table(file = "_data/meteo_anagrafe.csv",
                       sep  = ";",
                       dec  = ",") %>% 
  as_tibble()

# per rappresentare dati georeferenziati dobbiamo usare simple feature (package sf), in cui si può georeferenziare una linea, un poligono, etc.

library("sf")

sf.points <- st_as_sf(df.punti, 
                      coords = c("lon", "lat"),  # le coordinate sono numeriche
                      agr    = "constant",
                      crs    = 4326)             # crs sono le coordinate di riferimento (il numero corrisponde all'europa)

class(sf.points)


library("tmap")

tmap_mode(mode = "view")

tm_basemap(c("OpenStreetMap", "Esri.WorldGrayCanvas", "Esri.WorldTopoMap")) +  # bisogna dirgli quali mappe vogliamo in sottofondo
  tm_shape(sf.points, name = "Punti meteo fittizi") +
  tm_dots(col         = c("fornitore"),                                        # geometria a punti. colore diverso in base alla variabile fornitore
          popup.vars  = c("Fornitore:"  = "fornitore",
                          "Modello:"    = "modello",
                          "Altitudine:" = "altim"),
          size        = 1,
          palette     = c("red", "blue", "forestgreen"),
          legend.show = T,
          title       = "Punto meteo")   # la nuova versione ha dei nomi di etichetta diversi


library("giscoR")  

sf.poly.reg <- gisco_get_nuts(country = c("ITA"), spatialtype = "RG", nuts_level = 2)  # per i confini delle regioni

tmap_mode(mode = "plot")

tm_shape(shp = sf.poly.reg) + 
  tm_polygons(border.col = "blue", col = "blue", fill_alpha = .05) +
  tm_shape(shp = sf.points) + 
  tm_dots(col         = c("fornitore"), 
          palette     = c("red", "blue", "forestgreen"),
          legend.show = T,
          title       = "Fornitori",
          size = .1
          )



ggplot() + 
  # 1 - Layer base : Poligoni confini NUTS = 2
  geom_sf(data = sf.poly.reg, colour="blue", fill="blue", alpha = .05) +
  # 2 - Layer: Punti stazioni fittizie
  geom_sf(data = sf.points, aes(color = fornitore), show.legend = T) +
  scale_color_manual(values = c("yellow", "green", "orange"))
  # 3.1 - Theme base
  theme_bw() +
  # 3.2 - Theme, dettagli
  theme(axis.title.x  = element_blank(),
        axis.text.x   = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.title.y  = element_blank(),
        axis.text.y   = element_blank(),
        axis.ticks.y  = element_blank()) + 
  theme(plot.title    = element_text(hjust = 0), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption  = element_text(hjust = 1)) +
  # 4 - Etichette
  labs(title    = "Stazioni meteo fittizie",
       subtitle = NULL,
       caption  = "Laboratorio di R - 2025",
       color = "Fornitori",
       x = NULL,
       y = NULL)

  
  
# ritagliare la mappa (geojson.io permette di avere il boundary box che mi interessa)

library("geojsonsf")

sf.poly.ita.nord <- geojson_sf(geojson = "_data/bbox_ITA_Nord.geojson")  # è un poligono
bbox             <- st_bbox(sf.poly.ita.nord)

sf.poly.ita.nord <- st_crop(x = sf.poly.reg, y = bbox)  # ritaglia l'oggetto in modo che stia dentro al boundary box che ho scelto io


ggplot() + 
  geom_sf(data = sf.poly.ita.nord, colour="blue", fill="blue", alpha = .05) +
  geom_sf(data = sf.points, aes(color = fornitore), show.legend = T) +
  scale_color_manual(values = c("yellow", "green", "orange"))
theme_bw() +
  theme(axis.title.x  = element_blank(),
        axis.text.x   = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.title.y  = element_blank(),
        axis.text.y   = element_blank(),
        axis.ticks.y  = element_blank()) + 
  theme(plot.title    = element_text(hjust = 0), 
        plot.subtitle = element_text(hjust = 0),
        plot.caption  = element_text(hjust = 1)) +
  labs(title    = "Stazioni meteo fittizie",
       subtitle = NULL,
       caption  = "Laboratorio di R - 2025",
       color = "Fornitori",
       x = NULL,
       y = NULL)











