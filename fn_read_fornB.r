
# [ Lettura file fornitore B ] --------------------------------------------



# . Parametri -------------------------------------------------------------

# vcFname <- list.files(path       = "_data",
#                       pattern    = glob2rx("20*.csv"),
#                       all.files  = F,
#                       full.names = T,
#                       recursive  = F)
# 
# fn.out <- "standard"  # c("standard", "esteso")  # output
# 
# #
# if (exists("df.res")) rm(df.res)


fn.read.fornB <- function(vcFname = NULL,
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

# fn.read.fornB(vcFname = list.files(path = "_data", pattern = glob2rx("20*.csv"), full.names = T, recursive = F), fn.out  = "standard")  # dà il formato standard
# fn.read.fornB(vcFname = list.files(path = "_data", pattern = glob2rx("20*.csv"), full.names = T, recursive = F), fn.out  = "esteso")    # dà il formato esteso
# 
# fn.read.fornB(vcFname = list.files(path = "_data", pattern = glob2rx("200*.csv"), full.names = T, recursive = F), fn.out  = "esteso")
# fn.read.fornB(vcFname = NULL, fn.out  = "esteso")


