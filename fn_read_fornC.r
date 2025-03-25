
# [ Lettura file fornitore C ] --------------------------------------------


# . Parametri -------------------------------------------------------------

# fname   <- file.path("_data","meteo.xlsx")
# fn.out  <- "standard"
# fn.echo <-  FALSE

# ###
# if (exists("df.res")) rm(df.res)

fn.read.fornC <- function(fname   = NULL, 
                          fn.out  = "standard", 
                          fn.echo = FALSE) {
  
  # . CHK - Parametri -------------------------------------------------------
  if (!file.exists(fname)) {
    cat("\n-- Errore: fname non esiste")
    cat("\n   Hai inserito:", fname, "\n")
    return(NULL)
  }
  
  vc.fnEcho <- c("standard", "esteso")
  if (!fn.out %in% vc.fnEcho){
    cat("\n-- Errore: fn.out")
    cat("\n   Hai inserito", fn.out)
    cat("\n")
    return(NULL)
  }
  rm(vc.fnEcho)
  
  # . Packages --------------------------------------------------------------
  if("readxl" %in% rownames(installed.packages()) == FALSE){
    cat("\n-- Errore: il package readxl non è installato\n")
    return(NULL)
  } else {
    library(readxl)
  }
  
  # . Data  preparation -----------------------------------------------------
  vc.tab <- excel_sheets(fname)
  if (length(vc.tab) == 0){
    cat("\n-- Errore: il file ", fname, "non contiene tabelle\n")
    return(NULL)
  }
  
  # . Lettura e binding -----------------------------------------------------
  
  for (tab.name in vc.tab) {
    
    if (fn.echo) cat("\n", tab.name)
    tmp.point <- readxl::read_xlsx(path = fname, sheet = tab.name) %>% 
      rename(id       = localita,
             day.time = gg.tt,
             t        = t2m,
             r        = mm,
             rh       = um_rel,
             lw       = bagn) %>% 
      mutate(rh       = as.integer(rh),
             lw       = as.integer(lw)) 
    
    if (exists("df.res")) df.res <- bind_rows(df.res, tmp.point) else df.res <- tmp.point
    rm(tmp.point)
  }
  rm(tab.name, vc.tab)
  
  # . Formato esteso --------------------------------------------------------
  #   Aggiunge le colonne al mese giorno ora e data al data frame standard
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
  
} #### END FUN



# . Clear -----------------------------------------------------------------
# rm(fname, fn.echo, fn.out)
# rm(df.res)



# . Usage -----------------------------------------------------------------
# fn.read.fornC(fname = "_data/meteo.xlsx")
# fn.read.fornC(fname = "_data/meteo.xlsx", fn.out = "esteso")
# fn.read.fornC(fname = "_data/meteo.xlsx", fn.out = "esteso", fn.echo = T)
# fn.read.fornC(fname = "_data/xmeteo.xlsx")
# fn.read.fornC(fname = "_data/meteo.xlsx", fn.out = "Xsteso")
