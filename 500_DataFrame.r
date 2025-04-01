
# DATA FRAME


if(!exists("meteo")) meteo <- readRDS("_data/meteo.rds")



# . Select ----------------------------------------------------------------

meteo %>% 
    select(1, (7:ncol(.)))



# . Rename ----------------------------------------------------------------

df.tmp <- meteo %>% 
  select(1, 7:ncol(.)) %>% 
  rename( temperatura          = t,
          pioggia              = r,
          `Umidità relativa`   = rh,
          `bagnatura fogliare` = lw) 
df.tmp %>% head(2)



# . Filter ----------------------------------------------------------------

meteo %>% 
  select(id, anno, t, r) %>% 
  filter(id == "DSA001" & anno == 2020) %>% 
  head(2)


meteo %>% 
  select(id, anno, t, r) %>% 
  filter(anno %in% c(2019, 2021) & 
           between(t, left = 5, right = 10)) %>%   # between è del pacchetto dplyr (seleziona gli anni 2019 e 2021 e i valori di t tra 5 e 10)
  sample_n(10)



# . Slice -----------------------------------------------------------------

# Seleziona delle righe

meteo %>% 
  select(id, anno, t, r) %>% 
  filter(anno %in% c(2019, 2021) & 
           between(t, left = 5, right = 10)) %>% sample_n(10) %>% 
  slice(3:5)



# . Missing data ----------------------------------------------------------

# Come esempio metto dei dati mancanti

meteo %>% 
  select(id, anno, t, r) %>% 
  mutate(t = if_else(between(t, 5, 5.5), NA, t)) %>%   # se la temperatura è tra 5 e 5.5 metti NA, altrimenti lascia t (if_else non consente di cambiare il tipo dei dati)
  filter(!complete.cases(.)) %>%                       # seleziona solo chi non è completo
  head(10)


# escludere i dati mancanti
meteo %>% 
  select(id, anno, t, r) %>% 
  mutate(t = if_else(between(t, 5, 5.5), NA, t)) %>% 
  drop_na()



# . Arrange ---------------------------------------------------------------

meteo %>% 
  filter(anno == 2021 & mese == 1) %>%   
  select(giorno, ora, t, rh) %>% 
  arrange(t, rh) %>%                     # ordina in modo crescente di default
  head()

meteo %>% 
  filter(anno == 2021 & mese == 1) %>% 
  select(giorno, ora, t, rh) %>% 
  arrange(desc(t), rh) %>%               # desc ordina in modo decrescente
  head()



# . Mutate ----------------------------------------------------------------

meteo %>% 
  select(3:4, 8:ncol(.)) %>%                                 # seleziono le colonne da 3 a 4 e da 8 in poi
  mutate(mese       = formatC(mese, width = 2, flag = "0"),  # voglio formattare il mese in modo che ci sia lo 0 davanti nei numeri singoli (formatC trasforma in carattere, trasforma in una stringa di larghezza 2 e se non è pieno aggiungi uno 0 davanti)
         anno.short = substr(anno, 3, 4),                    # anno. short è calcolato come una sottostringa di anno e prendi i valori che iniziano con il terzo posto e finiscono con il quarto posto (es. 19 invece di 2019)
         t.corretta = if_else(t <= 0, 0, t),                 # se la temperatura è minore di 0 scrivi 0
         r.classe   = if_else(r < 0, "asciutto", "pioggia"),
         dummy      = substr(r.classe, 2, 3)) %>%            # creo una colonna con una parte della stringa r.classe (in pioggia è la seconda e terza lettera)
  select(1, ncol(.), 3, 4, 5, (ncol(.)-1)) %>% 
  head(10)



# . Lag e Lead ------------------------------------------------------------

# Per confrontare i valori di colonne precedenti o successive

meteo %>% 
  filter(id == "DSA001" & 
           data == ymd("2021-07-27")) %>%       # filtro stazione 1 e la data indicata (ymd è del package lubridate)
  select(t, r) %>%                              # seleziono le colonne t e r
  mutate(t.is.changed   = (t != lag(t)),        # t corrente è diversa dalla t della riga precedente (lag)? Risponde con T o F
         t.dif          = (t - lag(t)),         # differenza tra la t corrente e quella precedente
         t.will.change  = (t != lead(t)),       # lead guarda nel futuro (è una cosa che NON dovremmo fare nel modello). t è diverso dal t che verrà dopo?
         r.dif          = (r - lag(r)),
         r.is.changed   = (r != lag(r)),
         r.will.change  = (r != lead(r))) %>% 
  select(1, 3:5, 2, 5:7) %>% 
  print(n = nrow(.))



# . Cumsum ----------------------------------------------------------------

# Somme cumulate

meteo %>% 
  filter(id == "DSA001" & 
           data == ymd("2021-07-27")) %>% 
  select(t, r) %>% 
  mutate(t.csum  = cumsum(t),          # temperatura cumulata (in questo caso della giornata che abbiamo filtrato)
         t.csm0  = t.csum - first(t),  # la somma cumulata corretta è uguale a alla sommatoria senza il primo valore del data frame
         r.csum  = cumsum(r),          # pioggia cumulata
         r.csm0  = r.csum - first(r),
  ) %>% 
  select(1, 3:4, 2, 5:6) %>% 
  print(n = nrow(.))



# . Riclassificazione -----------------------------------------------------

# se le classi sono poche si possono usare gli if, mentre dplyr ha introdotto case_when()


# verifica i valori estremi del vettore
range(meteo$t)  # minimo e massimo


set.seed(123)
meteo %>% 
  select(7:ncol(.)) %>% 
  mutate(tcl = case_when(t <= 0           ~ "gelo",        # la tilde si fa con alt + 126
                         t > 0  & t <= 10 ~ "freddo",
                         t > 10 & t <= 15 ~ "fresco",
                         t > 15 & t <= 25 ~ "normale",
                         t > 25 & t <= 30 ~ "caldo",
                         t > 30           ~ "caldissimo",
                         TRUE             ~ NA)) %>%       # i valori non compresi negli intervalli precedenti sono NA
  #filter(!complete.cases(.)) %>% 
  
  select(1:2, ncol(.), 3:ncol(.)-1) %>% 
  sample_n(10)



# . Group_by --------------------------------------------------------------

meteo %>% 
  filter(anno == 2021 & id == "DSA001") %>% 
  group_by(mese) %>%                               # raggruppa per mese (nell'anno 2021 della stazione 1)
  summarise(t.min = min(t,  na.rm = T),            # calcola statistiche (temperatura minima calcolata come il valore minimo di t, rimuovendo i valori mancanti se ci sono)
            t.med = round(mean(t, na.rm = T), 2),  # calcola la media arrotondata a 2 decimali
            r.max = max(t,  na.rm = T),            # posso aggiungere anche la mediana (median()) e la deviazione standard(sd())
            n     = n()) %>%                       # numero di casi per ogni gruppo (posso anche usare count())
  print(n = nrow(.))                               # questo serve se voglio far stampare tutte le righe del data frame



meteo %>%
  filter(id == "DSA001" & anno == 2021) %>%
  group_by(mese, giorno) %>%                       # raggruppa per mese e giorno
  summarise(t.med = mean(t, na.rm = T), 
            .groups = 'drop') #%>%                 # per togliere il messaggio di avvertimento (posso anche usare la funzione ungroup())
  #str()                                           # senza il drop nella struttura stampa anche il ricordo di come ha fatto a fare i gruppi (il data frame è pulito)


##

meteo %>% 
  filter(id == "DSA001" & anno == 2021) %>% 
  select(data, mese, giorno, ora, t, r) %>% 
  group_by(mese, giorno) %>% 
  mutate(t.cum  = cumsum(t),
         t.cum0 = t.cum - first(t),
         r.cum  = cumsum(r),
         r.cum0 = r.cum - first(r),
  ) %>% ungroup() %>%                         # ungroup serve per far ricominciare i calcoli della somma cumulata dal valore 0 dell'ora del giorno seguente (mantiene il data frame intatto e fa solo le statistiche per gruppo)
  filter(data %in% c(ymd("2021-07-26"),       # filtra tra queste due date
                     ymd("2021-07-27"))) %>% 
  print(n = nrow(.))



# . Conteggi e percentuali ------------------------------------------------

meteo %>% 
  filter(id == "DSA001" & anno == 2021) %>% 
  mutate(tcl = case_when(t < 0              ~ "Gelo",
                         between(t, 0, 15)  ~ "Fresco",
                         between(t, 15, 25) ~ "Normale",
                         t > 25             ~ "Caldo",
                         TRUE               ~ NA)) %>% 
  group_by(tcl) %>% count() %>% ungroup() %>%           # raggruppa per tcl (le classi che ho messo prima) e conta per ogni classe quanti casi ci sono 
  mutate(prop = n / sum(n),
         ptc  = round(prop * 100, 0))                   # prop calcola la proporzione dei casi di un gruppo sul totale (sum(n)) e ptc calcola la percentuale


##
meteo %>% 
  filter(id == "DSA001" & anno == 2021) %>% 
  group_by(mese, giorno) %>% 
  summarise(lwn  = sum(lw),
            lwcl = case_when(lwn <  9 ~ "Asciutto",
                             lwn >= 9 ~ "Umido",
                             TRUE     ~ NA)) %>% 
  ungroup() %>% 
  group_by(lwcl) %>% count() %>% ungroup() %>%     # n = 291 giorni asciutti e 74 bagnati
  mutate(pct = n / sum(n) * 100)                   # percentuale



# raggruppamento per settimana
meteo %>% 
  filter(anno == 2021 & id == "DSA001") %>% 
  select(data, t, r, rh, lw) %>% 
  group_by(week = lubridate::week(data)) %>%  # week è uguale al risultato di week del package lubridate (lubridate calcola la settimana a partire dal primo dell'anno, altri calendari calcolano in base al giorno, ad es se il primo dell'anno è mercoledì partono dall'anno prima)
  summarise(r.cum = sum(r))



# . Windowing -------------------------------------------------------------

# finestra mobile che si sposta in giù tenendo in considerazione quello che è successo nelle righe precedenti

# Parametri

if(!exists("meteo")) meteo <- readRDS("_data/meteo.rds")

met <- meteo %>% 
  filter(anno == 2021 & id == "DSA001") %>% 
  select(-id, - anno, - mese, - giorno, - ora, - data)   # tolgo tutte le colonne che non mi servono

win <- 8  # numero di ore della finestra

met %>% 
  mutate(n = seq(1, nrow(.), 1))

# selezione righe e colonne
met[11:18, c(2, 5)]
met[(18-7):18, c(2, 5)]  

i <- 18
met[(i-win+1):i, c("t")] %>% pull() %>% mean()  # estraggo la temperatura dalle righe da (i-win+1) a i %>% la trasformo in un vettore %>% sul vettore faccio la media

# unlist (fare quello che ho fatto prima ma senza il pipe)
mean(unlist(met[(i-win+1):i, c("t")], use.names = F))  # è diventato un vettore (coi nomi dei singoli valori di default)

sum(unlist(met[(i-win+1):i, c("lw")], use.names = F))  # somma di lw per 8 ore



## Script per il windowing ##

met$chk.tlw <- as.numeric(NA)  # sintassi base di R per aggiungere colonne a un data frame

n <- nrow(met)  # numero di righe totali del data frame met

for (i in win:n){   # win = 8 (l'abbiamo detto prima)
  # cat("\r", i, " di ", n)  # Contatore
  if (mean(unlist(met[(i-win+1):i, c("t")]), na.rm = T) >= 20 &    # se la media della temperatura >= 20 
      sum(unlist(met[(i-win+1):i, c("lw")]), na.rm = T) == win){   # se il numero di ore di bagnatura = 8
    
    met[i, "chk.tlw"] <- 1                                         # allora la iesima riga di met nella la colonna chk.tlw è uguale a 1
    
  } else {                                                         # altrimenti la iesima colonna di met nella la colonna chk.tlw è uguale a 0
    met[i, "chk.tlw"] <- 0
    
  } # END IF
} ### END FOR
rm(i)


# controlli
met <- met %>%   # aggiungere il numero di riga
  mutate(n = seq(1, nrow(.), 1)) %>% 
  select(7, 1:6)

met %>% filter(chk.tlw == 1)  # guarda quali giorni hanno la t maggiore di 20 gradi e lw = 8

# è utile per vedere da che ora si verifica un certo fenomeno
# in teoria non bisognerebbe fare il ciclo for


# puliamo il data frame e riportiamo il data frame alla sua struttura iniziale
rm(i, n, win)
met <- met %>% select(- chk.tlw)



# > Funzione < ------------------------------------------------------------

#### parametri
win         <- 8
# i         <- 4440
# df.win    <- met[(i-win+1):i, c("n", "t", "lw")]
# t.soglia  <- 20
# lw.soglia <- win


fn.chk <- function(df.win    = NULL,
                   t.soglia  = NULL,
                   lw.soglia = NULL) {
  #### calcoli
  t.med  <- mean(df.win$t, na.rm = T)
  lw.chk <- sum(df.win$lw, na.rm = T)
  
  #### risultato
  res    <- ifelse(t.med >= t.soglia & lw.chk == lw.soglia, 1, 0)
  
  ### return
  return(res)
  
} # END FUN


#### clear
# rm(i, df.win, t.soglia, lw.soglia, t.med, lw.chk, res)


#### usage
met$chk.tlw <- as.numeric(NA)
n           <- nrow(met)

for (i in win:n) {
  
  met[i, "chk.tlw"] <- fn.chk(df.win    = met[(i-win+1):i, c("t", "lw")],
                              t.soglia  = 20,
                              lw.soglia = win)
  
} # END FOR


met %>% filter(chk.tlw == 1)  # sono gli stessi 7 casi che abbiamo visto prima



## Per evitare il ciclo for (con rowwise)

met <- met %>% select(- n)

met <- met %>% 
  mutate(chk.tlw = as.numeric(NA)) %>% 
  rownames_to_column(var = "n") %>% 
  mutate(n = as.numeric(n)) %>% 
  rowwise() %>% 
  mutate(chk.tlw = ifelse(n >= win, fn.chk(df.win = met[(n-win+1):n, c("t", "lw")], t.soglia = 20, lw.soglia = win), NA)) %>% 
  ungroup() 



# . Join ------------------------------------------------------------------

# 27/03/25


## Dati esempio

x <- tibble(key   = c(1, 2, 3),
            val_x = c("x1", "x2", "x3"))

y <- tibble(key   = c(1, 2, 4),
            val_y = c("y1", "y2", "y4"))   # key è la colonna che hanno in comune


# inner join (solo le chiavi in comune)
inner_join(x, y, by = "key")  # il nome della colonna è sempre tra virgolette

# left join (tutti gli attributi di x)
left_join(x, y, by = "key")



# > Caso d'uso < ----------------------------------------------------------


library(readxl)

up  <- read_xlsx(path = "_data/trattamenti.xlsx", sheet = "up")            # unità produttive
tra <- read_xlsx(path = "_data/trattamenti.xlsx", sheet = "trattamenti")
pro <- read_xlsx(path = "_data/trattamenti.xlsx", sheet = "prodotti")
moa <- read_xlsx(path = "_data/trattamenti.xlsx", sheet = "moa")           # mode of action
pa  <- read_xlsx(path = "_data/trattamenti.xlsx", sheet = "pa")            # principio attivo


# Vogliamo fare una tabella con il nome del prodotto e quante volte è stato usato (per vedere il prodotto più usato)

tra %>% 
  select(prod_id) %>%                                                    # elenco di tutti i prodotti usati (anche ripetuti)
  left_join(y = pro %>% select(prod_id, prod_desc), by = "prod_id") %>%  # per ogni codice prod_id ho gli attributi di quel prodotto (ha tenuto tutte le colonne dei trattamenti e ha attaccato gli attributi della tabella del prodotto che mi interessano)
  group_by(prod_desc) %>% count() %>% ungroup() %>%                      # voglio contare quante volte sono stati usati i prodotti (usando i nomi comuni)
  arrange(desc(n)) %>%                                                   # tabella ordinata in ordine decrescente per utilizzo
  mutate(pct     = round(n / sum(n) * 100, 2),
         pct.cum = cumsum(pct))


# Tabella con i principi attivi più usati

tmp <- tra %>% 
  select(prod_id) %>% 
  group_by(prod_id) %>% count() %>% ungroup() %>%  # per ogni codice prodotto mi dice quante volte l'ho usato
  arrange(desc(n)) %>% 
  left_join(pro %>% select(prod_id, pa1, pa2, pa3), by = "prod_id")


# Smonto il data frame in 3 pezzi (i 3 principi attivi) uno sotto l'altro con lo stesso nome pa_id (chiave), quindi nella stessa colonna

tmp %>% select(prod_id, n, pa1) %>% rename(pa_id = pa1) %>%               # rinomino pa1 con lo stesso nome della chiave che userò per mettere in relazione le due tabelle
  bind_rows(tmp %>% select(prod_id, n, pa2) %>% rename(pa_id = pa2)) %>% 
  bind_rows(tmp %>% select(prod_id, n, pa3) %>% rename(pa_id = pa3)) %>%  # però non tutti i prodotti hanno tutti e 3 i principi attivi (mi aspetto dei NA)
  drop_na() %>%                                                           # tolgo i casi mancanti
  group_by(pa_id) %>% summarise(n.uso = sum(n)) %>%                       # per ogni id di principio attivo somma quante volte l'ho usato
  left_join(y = pa, by = "pa_id") %>%                                     # aggiungo il nome del principio attivo (la tabella pa ha la colonna pa_id)
  arrange(desc(n.uso)) %>% 
  select(pa_desc, n.uso) %>% 
  mutate(pct     = round(n.uso / sum(n.uso) * 100, 2),
         pct.cum = cumsum(pct)) %>% 
  rename(`Principio attivo` = pa_desc,                                    # uso `` perché ho degli spazi e dei caratteri speciali
         Applicazioni       = n.uso,
         `%`                = pct,
         `% cum`            = pct.cum) # %>% 
   
  # write.table("clipboard", sep = "\t", dec = ",", na = "", row.names = F, col.names = T)  # copio la tabella, posso andare su excel e fare incolla e ho la tabella (attenzione che non mette il nome a rowname, quindi i nomi delle colonne sono sfasati)


# Distinct()

tra %>% 
  left_join(y = pro, by = "prod_id") %>%
  distinct(prod_desc) %>%    # elenco dei prodotti unici (senza duplicati)
  arrange(prod_desc) %>% 
  pull()                     # pull trasforma il data frame in un vettore

# oppure
delme <- tra %>% 
  left_join(y = pro, by = "prod_id")

unique(delme$prod_desc)  # anche con unique si possono trovare i valori unici

rm(delme)


# Individuare i casi unici e il numero di osservazioni per ognuno

tmp <- tra %>% 
  left_join(y = pro %>% select(prod_id, prod_desc, pa1, pa2, pa3), by = "prod_id") %>% 
  select(up_id, day, prod_desc, pa1, pa2, pa3, target, ha, dose_ha, udm_ha)

tmp %>% distinct(prod_desc) %>% pull()

tmp %>% 
  select(prod_desc, pa1, pa2, pa3) %>% 
  distinct(prod_desc, .keep_all = T)    # di fianco al nome del prodotto unico ho i suoi attributi (keep_all, mantieni le colonne)

# distinct su colonne annidate (per avere elenchi unici gerarchici)
tmp %>% 
  distinct(up_id, prod_desc) # %>% view()  # per ogni id della up dimmi il prodotto unico



# . Dati mancanti ---------------------------------------------------------


# > Metodo A < ------------------------------------------------------------


## Serie discontinua

# voglio un data frame con il 5% di righe mancanti
if(!exists("meteo")) meteo <- readRDS("_data/meteo.rds")

set.seed(123)       
vc.missing <- meteo %>%                        # seleziono il 5% di date casuali (mi serve per avere il data frame con missing data)
  filter(anno == 2021 & id == "DSA001") %>% 
  select(day.time) %>% 
  sample_frac(size = .05, replace = F) %>%    
  pull()

# assegno valori mancanti al data frame
df.source <- meteo %>% 
  filter(anno == 2021 & id == "DSA001") %>%
  select(day.time, t, r, rh, lw) %>%
  mutate(day.time = ifelse(day.time %in% vc.missing, NA, day.time),
         day.time = lubridate::as_datetime(day.time))

# controlliamo se ci sono i dati mancanti
df.source %>% filter(!complete.cases(.)) %>% nrow()

# tolgo i casi mancanti dal data frame
df.source <- df.source %>% filter(complete.cases(.))


## Serie continua

# voglio sapere dove sono i dati mancanti
# genero un data frame con la sola colonna day.time
df.ctrl <- tibble(day.time = seq.POSIXt(from = ISOdate(year = 2021, month = 1,  day = 1,  hour = 0,  min = 0, sec = 0, tz = "UTC"),
                                        to   = ISOdate(year = 2021, month = 12, day = 31, hour = 23, min = 0, sec = 0, tz = "UTC"),
                                        by   = "hour"))  # sequenza con tutte le ore


# Abbiniamo le chiavi del dataframe completo con quello incompleto (ci aspettiamo dei NA)
df.wrk <- left_join(x = df.ctrl, y = df.source, by = "day.time")

df.wrk %>% filter(!complete.cases(.))  # estraggo le ore mancanti (ora conosco tutte le date e ore per cui non ho dati)

rm(df.ctrl, df.source, df.wrk, vc.missing)



# > Metodo B < ------------------------------------------------------------


# devo crearmi io una chiave 

# assegno un id
df.source <- meteo %>%
  filter(anno == 2021 & id == "DSA001") %>% 
  select(3:6, 8:ncol(.)) %>% 
  mutate(id = seq(1, nrow(.), 1)) %>% 
  select(ncol(.), 1:(ncol(.)-1))

# creo una serie discontinua
set.seed(123)
vc.missing <- sample_frac(df.source %>% select(id), size = .05, replace = F) %>% pull()   # campione frazionario (creo i dati mancanti)

df.source <- df.source %>% 
  mutate(id = ifelse(id %in% vc.missing, NA, id))   # metto degli NA dentro df.source

df.source %>% filter(!complete.cases(.)) %>% nrow(.)


# Creo una chiave univoca (key)
df.source <- df.source %>% filter(complete.cases(.)) %>% 
  select(-id) %>%                                                                # tolgo la colonna id
  unite(col = "key", c("anno", "mese", "giorno", "ora"), sep = ".", remove = T)  # unisce e toglie le colonne che non mi interessano (anno, mese, etc)


# Sequenza continua
df.ctrl <- tibble(day.time = seq.POSIXt(from = ISOdate(year = 2021, month =  1, day =  1, hour =  0, min = 0, sec = 0, tz = "UTC"),
                                        to   = ISOdate(year = 2021, month = 12, day = 31, hour = 23, min = 0, sec = 0, tz = "UTC"),
                                        by = "hour")) %>% 
  # costruiamo la chiave
  mutate(anno   = year(day.time), 
         mese   = month(day.time), 
         giorno = day(day.time), 
         ora    = hour(day.time)) %>% 
  unite(col = "key", c("anno", "mese", "giorno", "ora"), sep = ".", remove = T) %>% 
  select(-day.time)  # mi rimane solo la colonna key


# Uniamo i data frame
df.wrk <- df.ctrl %>% 
  left_join(y = df.source, by = "key") %>% 
  separate(col = key, into = c("anno", "mese", "giorno", "ora"), sep = "\\.", remove = T)  # ho rimosso la colonna key

# guardo i dati mancanti
df.wrk %>% filter(!complete.cases(.)) %>%  nrow(.) # %>% view() 



# . Pivoting --------------------------------------------------------------

# pivot wider

# temperatura media mensile per anno
meteo %>% 
  group_by(anno, mese) %>% summarise(t.med = round(mean(t), 2)) %>% ungroup() %>% 
  mutate(mese = paste0("m", formatC(mese, width = 2, flag = "0"))) %>% 
  pivot_wider(names_from = mese, names_sort = T, values_from = t.med, values_fill = 0)


# aggregare una tabella con pioggia totale per giorno (per i mesi del 2021)
meteo %>% 
  filter(anno == 2021 & id == "DSA001") %>% 
  group_by(mese, giorno) %>% summarise(mm = sum(r)) %>% ungroup() %>%   # mese, giorno perché sonp dati orari
  mutate(mese = paste0("m", formatC(mese, width = 2, flag = 0)),        # così mi esce m01, m02 etc (per il nome del mese)
         giorno = formatC(giorno, width = 2, flag = 0)) %>%             # anche qua voglio lo 0 davanti al numero
  pivot_wider(names_from = mese, names_sort = T, values_from = mm, values_fill = 0) %>% 
  rename(Giorno = giorno) %>% 
  print(n = nrow(.))



# 01/04/25

# pivot longer

tmp <- meteo %>% 
  filter(anno == 2021 & id == "DSA001") %>% 
  group_by(mese, giorno) %>% summarise(mm = sum(r)) %>% ungroup() %>%  
  mutate(mese = paste0("m", formatC(mese, width = 2, flag = 0)),   
         giorno = formatC(giorno, width = 2, flag = 0)) %>% 
  pivot_wider(names_from = mese, names_sort = T, values_from = mm, values_fill = 0) %>% 
  rename(Giorno = giorno)

tmp %>% 
  rename(giorno = Giorno) %>% 
  pivot_longer(cols      = !giorno,  # prendi le colonne che non sono giorno, mettile nella colonna mese e assegna a ciascuna riga il valore t
               names_to  = "mese", 
               values_to = "r") %>% 
  select(mese, giorno, r) %>% 
  arrange(mese, giorno)














