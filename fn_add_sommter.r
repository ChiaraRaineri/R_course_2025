
### Calcolo della somma termica ###


# . Parametri -------------------------------------------------------------

# df.meteo  <- fn.met.day(df.meteo = meteo) %>% filter(anno == "2019" & id == "DSA001")  # il data frame in ingresso è dello stesso tipo della funzione che abbiamo fatto prima (con la colonna t.med)
# t.base    <- 0
# clean.res <- T



# >> Funzione << ----------------------------------------------------------


fn.add.sommter <- function(df.meteo  = NULL, 
                           t.base    = 0, 
                           clean.res = T) {
  

  
  # . Check parametri -------------------------------------------------------

  if(is.null(df.meteo)){
    cat("\n --- ERRORE")
    cat("\n df.meteo è NULL")
    cat("\n")
    return(NULL)
  }
  
  
  
  # . Calcolo ---------------------------------------------------------------
  
  df.meteo <- df.meteo %>% 
    mutate(som.ter.day = if_else(t.med <= t.base, 0, t.med),
           som.ter     = cumsum(som.ter.day))
  
  
  
  # . Direttiva IF ----------------------------------------------------------
  
  if(clean.res) df.meteo <- df.meteo %>% select(-som.ter.day)
  
  
  
  # . Return ----------------------------------------------------------------
  
  return(df.meteo)

  
}  ### END FUN



# . Clear -----------------------------------------------------------------

# rm(df.meteo, t.base, clean.res)



# . Usage -----------------------------------------------------------------

# fn.add.sommter(df.meteo = fn.met.day(df.meteo = meteo) %>% filter(anno == "2020" & id == "DSA003"),
#                clean.res = F)





