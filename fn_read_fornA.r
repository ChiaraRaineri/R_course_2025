
# [ Lettura file CSV fornitore A ] ----------------------------------------


# Tutto ciò che non è la funzione è commentato (lo posso fare dopo che ho costruito la funzione completa)


# . Parametri -------------------------------------------------------------

# vcFname <- list.files(path       = "_data/",
#                       pattern    = glob2rx("DSA*.csv"),
#                       all.files  = F,
#                       full.names = T,
#                       recursive  = F)
# 
# fn.out <- "standard"  # c("standard", "esteso")  # output
# 
# #
# if (exists("df.res")) rm(df.res)



fn.read.fornA <- function(vcFname = NULL,
                          fn.out  = "standard",
                          fn.echo = F) {

  # . Controlli -------------------------------------------------------------
  if(is.null(vcFname)){
    cat("\nErrore: vcFname è NULL")
    cat("\n")
    return(NULL)
  }
  
  if (length(vcFname) == 0) {
    cat("\n-- Errore: nessun file in elenco")
    cat("\n")
    return(NULL)
  }
  
  
  # . Lettura CSV -----------------------------------------------------------
  
  for(fname in vcFname){
    # fname <- vcFname[1]  # questo serve per verificare che legga il file correttamente
    
    if(fn.echo) cat("\nLettura del file:", fname)
    
    tmp <- read.table(file = fname,
                      header = T,
                      sep = ";",
                      dec = ",",
                      col.names = c("day.time", "t", "r", "rh", "lw")) %>% 
      as_tibble() %>% 
      mutate(day.time = as.POSIXct(day.time, "%Y-%m-%d %H:%M:%S", tz = "GMT"),
             id       = substr(basename(fname), start = 1, stop = 6)) %>% 
      select(6, 1:5)
    
    if(exists("df.res")) df.res <- bind_rows(df.res, tmp) else df.res <- tmp
    rm(tmp)
    
  }  # END FOR
  
  
  
  # . Formato esteso --------------------------------------------------------
  
  fn.out <- tolower(fn.out)
  
  if (fn.out == "esteso"){
    df.res <- df.res %>% 
      mutate(anno   = year(day.time),
             mese   = month(day.time),
             giorno = day(day.time),
             ora    = hour(day.time),
             data   = as.Date(day.time, "%Y-%m-%d", tz="GMT")) %>% 
      select(id, day.time, anno, mese, giorno, ora, data, t, r, rh, lw)
  }

# . Return ----------------------------------------------------------------

  return(df.res)  
  
  
  

}  # END FUN



# . Clear memory ----------------------------------------------------------

# rm(vcFname, fn.out, fname, df.res)



# . Usage -----------------------------------------------------------------

# fn.read.fornA(vcFname = list.files(path = "_data/", pattern = glob2rx("DSA*.csv"), full.names = T, recursive = F), fn.out  = "standard")  # dà il formato standard
# fn.read.fornA(vcFname = list.files(path = "_data/", pattern = glob2rx("DSA*.csv"), full.names = T, recursive = F), fn.out  = "esteso")    # dà il formato esteso
# 
# 
# # controlli
# fn.read.fornA(vcFname = list.files(path = "_data/", pattern = glob2rx("XSA*.csv"), full.names = T, recursive = F),
#               fn.out  = "esteso")
# fn.read.fornA(vcFname = NULL, fn.out  = "esteso")





