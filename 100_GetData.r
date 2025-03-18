
# [ GetData - Read weather files ] ----------------------------------------

if(exists("vc.fname")) rm(vc.fname)   # per rimuovere variabili "sporche"
if(exists("fname"))    rm(fname)


# >>> Fornitore A <<< -----------------------------------------------------


# [01] - Get vettore fname ------------------------------------------------

if (!exists("vc.fname")) vc.fname <- list.files(path       = "_data",                # se non esiste (l'ho pulito all'inizio), lo ricrei
                                                pattern    = glob2rx("DSA*.csv"),
                                                all.files  = F,
                                                full.names = T,
                                                recursive  = F)



# [02] - Legge un file di esempio -----------------------------------------

fname <- vc.fname[1]    # assegno a fname il primo elemento del vettore (cioè il primo file DSA001)

read.table(file      = fname,
           header    = T,
           sep       = ";",                                       # separatore del file csv
           dec       = ",",                                       # dec è come sono separati i decimali
           col.names = c("day.time", "t", "r", "rh", "lw")) %>% 
  as_tibble() %>%                                                 # questo con il pipe serve per visualizzare solo i primi 10 elementi e con anche il tipo
  mutate(day.time    = as.POSIXct(day.time, "%Y-%m-%d %H:%M:%S", tz = "GMT"),
         id          = substr(x = basename(fname), start = 1, stop = 6)) %>%  # è una sottostringa (in questo caso indica il file da cui vengono i dati). start e stop indicano quanti caratteri ci devono essere nella stringa
  select(6, 1:5)                                                              # dice che ci deve essere prima la sesta colonna e poi quelle dalla 1 alla 5
# select(ncol(.), 1:(ncol(.)-1))  # fa la stessa cosa, senza dover fare i calcoli
# %>% view()  # questo non è fondamentale (fa vedere la tabella)



# [03] - Stampa nomi dei file ---------------------------------------------

for (fname in vc.fname){                            # questo ciclo è per visualizzare l'indice dei file (il nome dei file)
  # fname <- vc.fname[1]                            # lo commento ma lo eseguo (così posso usare un caso di esempio)
  
  cat("\n. File:", substr(basename(fname), 4, 6))   # "\n" vuol dire vai a capo. Senza substr mette il nome completo del file
  
}



# [04] - Leggere i file e binding -----------------------------------------

if (exists("meteo.a")) rm(meteo.a)   # meteo.a è il nome del file finale che voglio ottenere con tutti i dati del fornitore A (il mio scopo è quello di attaccare insieme meteo.a, meteo.b e meteo.c)
for (fname in vc.fname){              
  
  cat("\n. File:", substr(basename(fname), 4, 6))
  
  tmp <- read.table(file      = fname,
                    header    = T,
                    sep       = ";",                                       
                    dec       = ",",                                       
                    col.names = c("day.time", "t", "r", "rh", "lw")) %>% 
    as_tibble() %>%                                                 
    mutate(day.time    = as.POSIXct(day.time, "%Y-%m-%d %H:%M:%S", tz = "GMT"),
           id          = substr(x = basename(fname), start = 1, stop = 6)) %>%  
    select(6, 1:5)
  
  if (exists("meteo.a")) meteo.a <- bind_rows(meteo.a, tmp) else meteo.a <- tmp   # la prima volta che si esegue il ciclo meteo.a non esiste, quindi lo creo rendendolo uguale a tmp, se invece già esiste (quando il loop prosegue) aggiungi righe (bind_rows)
  
}



# [05] - Controllare se ci sono dati mancanti -----------------------------

# 18/03/25

# per sapere quanti sono i dati che abbiamo per ogni id (per ogni stazione), cioè per ogni gruppo quanti casi ci sono
# da fare per controllare che non ci siano dati mancanti
meteo.a %>% 
  group_by(id) %>% count()


chk <- meteo.a %>% filter(!complete.cases(.))  # prendi le righe con casi non completi (in questo caso 0)


# per informare se ci sono dati mancanti
if(nrow(chk)==0){
  cat("\n-------------------------------")
  cat("\n . Non ci sono dati mancanti")
  cat("\n-------------------------------")
} else {
  cat("\n-------------------------------")
  cat("\n . Attenzione, ci sono dati mancanti")
  cat("\n-------------------------------")
}

rm(chk)



# [06] - Controllo casi per anno ------------------------------------------

# Creo un data frame temporaneo con due colonne in più (lo scopo è creare una tabella con elencati per ogni stazione gli anni e quanti casi ci sono in ogni anno)
# Di ogni riga voglio sapere l'anno e il mese
tmp <- meteo.a %>% 
         mutate(anno = year(day.time),
                mese = month(day.time))


# controllo casi per anno per ogni stazione (id). In ogni gruppo (anno e id) conta quanti casi ci sono
tmp %>% 
  group_by(anno, id) %>% count() %>%   # raggruppa per id e anno e poi conta i casi
  ungroup() %>%                        # ungroup per fargli dimenticare la regola che si è usata in precedenza
  pivot_wider(names_from  = anno,      # creo una tabella pivot (numero di casi per ogni combinazione stazione-anno)
              names_sort  = T,         # mette in ordine alfabetico
              values_from = n,         # n è il risultato del count() che ho fatto prima
              values_fill = 0)         # se ci sono dati mancanti metti 0 invece che NA

# in questo caso il 2022 ha meno valori rispetto agli altri anni
# voglio andare a vedere cosa succede
tmp %>% 
  filter(anno == 2022) %>% 
  group_by(id, mese) %>% count() %>% ungroup() %>%   # voglio vedere se ho tutti i 12 mesi
  pivot_wider(names_from  = mese,
              names_sort  = T,
              values_from = n,
              values_fill = 0)

# vedo che per il 2022 ho solo due mesi, gennaio e febbraio (il provider mi ha dato dati incompleti)


summary(tmp %>% select(t, r, rh, lw))  # statistica su variabili che mi interessano



# >>> Fornitore B <<< -----------------------------------------------------



# [01] -  Vettore dei nomi di file ----------------------------------------

vc.fname <- list.files(path       = "_data",
                       pattern    = glob2rx("20*.csv"),
                       all.files  = T,
                       full.names = T,
                       recursive  = F)

fname <- vc.fname[1]

# un altro modo per importare i file .csv
read_delim(file      = fname,
           col_names = T,
           delim     = ";",
           locale    = locale(decimal_mark = ","),  # qua gli diciamo che il separatore decimale è la virgola, ma si possono dire altre cose
           col_types = cols(unmidità  = col_integer(),
                            bagnatura = col_integer()),
           trim_ws   = T) %>% head(5)               # trim_ws toglie gli spazi bianchi



# [02] - Importa i file ---------------------------------------------------


if(exists("meteo.b")) rm(meteo.b)

for (fname in vc.fname) {
  cat("\n" , fname)
  tmp <- read_delim(file      = fname,
                    col_names = T,
                    delim     = ";",
                    locale    = locale(decimal_mark = ","), 
                    col_types = cols(unmidità  = col_integer(),
                                     bagnatura = col_integer()),
                    escape_double = F,
                    trim_ws   = T) %>% 
    rename(id       = stazione,
           day.time = ora,
           t        = temperatura,
           r        = pioggia,
           rh       = unmidità,
           lw       = bagnatura)
  
  if(exists("meteo.b")) meteo.b <- bind_rows(meteo.b, tmp) else meteo.b <- tmp
  rm(tmp)
  
}



# >>> Fornitore C <<< -----------------------------------------------------



# [01] - Lettura file Excel -----------------------------------------------


library("readxl")


fname  <- file.path("_data", "meteo.xlsx")  # c'è un solo nome di file (non un vettore)
vc.tab <- excel_sheets(fname)

tab.name <- vc.tab[1]

# Leggi il file
tmp <- read_xlsx(path = fname, sheet = tab.name) %>% 
         mutate(um_rel = as.integer(um_rel),
                bagn   = as.integer(bagn)) %>% 
        rename(id       = localita,
               day.time = gg.tt,
               t        = t2m,
               r        = mm,
               rh       = um_rel,
               lw       = bagn)


if(exists("meteo.c")) rm(meteo.c)

for (tab.name in vc.tab) {
  cat("\n" , tab.name)
  tmp <- read_xlsx(path = fname, sheet = tab.name) %>% 
    mutate(um_rel = as.integer(um_rel),
           bagn   = as.integer(bagn)) %>% 
    rename(id       = localita,
           day.time = gg.tt,
           t        = t2m,
           r        = mm,
           rh       = um_rel,
           lw       = bagn)
  
  if(exists("meteo.c")) meteo.c <- bind_rows(meteo.c, tmp) else meteo.c <- tmp
  rm(tmp)
  
}



# >>> Unire i 3 data frame <<< --------------------------------------------



# [01] - Unione -----------------------------------------------------------

meteo <- bind_rows(meteo.a, meteo.b) %>% 
  bind_rows(meteo.c)

# oppure
meteo <- meteo.a %>%
  bind_rows(meteo.b) %>%  
  bind_rows(meteo.c)


meteo %>% 
  group_by(id) %>% count()



# [02] - Nuove colonne e riordino -----------------------------------------

meteo <- meteo %>%                                                   # formato definitivo
  mutate(anno   = year(day.time),
         mese   = month(day.time),
         giorno = day(day.time),
         ora    = hour(day.time),
         data   = as.Date(day.time, "%Y-%m-%d", tz = "GMT")) %>% 
  select(id, day.time, anno, mese, giorno, ora, data, t, r, rh, lw)  # per decidere l'ordine delle colonne

head(meteo, n = 3)


# vincoli: bisogna che le serie storiche siano complete (anno 2022 incompleto)
meteo <- meteo %>% filter(anno != 2022)


# tabella pivot
meteo %>% 
  group_by(anno, mese) %>% count() %>% ungroup() %>%    # guardo per ogni mese quanti dati ho
  pivot_wider(names_from  = mese,
              names_sort  = T,
              values_from = n,
              values_fill = 0)



# [03] - Esporta il data frame su disco -----------------------------------

# Salvare un solo oggetto (in formato .rds, che è un file binario per cui c'è bisogno di un connettore per leggerlo)
saveRDS(object = meteo, file = "_data/meteo.rds")
file.exists("_data/meteo.rds")

# per leggere un formato .rds
meteo <- readRDS(file = "_data/meteo.rds")


# Salvare più di un oggetto (dentro un file di R con solo gli oggetti indicati)
save(meteo, meteo.a, meteo.b, meteo.c, file = "_data/meteo.RData")
file.exists("_data/meteo.RData")

# per leggere un formato .RData
load(file = "_data/meteo.RData")



# >>> Pulizia dell'ambiente <<< -------------------------------------------

rm(list = ls(pattern = glob2rx("vc.*")))
rm(list = ls(pattern = glob2rx("meteo.*")))
rm(chk, fname, tab.name, meteo)



# >>> Salva CSV <<< -------------------------------------------------------

if (!exists("meteo")) meteo <- readRDS("_data/meteo.rds")  # importa se l'ho cancellato dall'ambiente

write.table(x = meteo, file = "_data/meteo_export.csv", sep = ";", dec = ",", row.names = F)  # non voglio i nomi di riga



# >>> Salva XLSX <<< ------------------------------------------------------

library("writexl")

# voglio esportare solo 3 stazioni (creo una lista) (sono 3 data frame)
lst.out <- list("DAS001" = meteo %>% filter(anno == 2020 & mese == 1 & id == c("DSA001")),
                "DAS002" = meteo %>% filter(anno == 2020 & mese == 1 & id == c("DSA002")),
                "DAS003" = meteo %>% filter(anno == 2020 & mese == 1 & id == c("DSA003")))

# salva su disco
write_xlsx(x = lst.out, path = file.path("_data", "meteo_export.xlsx"))


# write_xlsx(x = meteo, path = file.path("_data", "meteo_export_all_years.xlsx"))


# Altra libreria per salvare i file .xlsx e fare operazioni più complesse
library("openxlsx")




















