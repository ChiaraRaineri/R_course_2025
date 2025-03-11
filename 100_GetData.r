
# [ GetData - Read weather files ] ----------------------------------------

if(exists("vc.fname")) rm(vc.fname)   # per rimuovere variabili "sporche"
if(exists("fname"))    rm(fname)



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









