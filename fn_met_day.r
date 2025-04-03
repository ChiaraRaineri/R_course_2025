
# 00 - Parametri ----------------------------------------------------------

# df.meteo  <- meteo %>% filter(id == "DSA001")
# lbl.month <- T
# fun.pipe  <- F



# 01 - Check parameters ---------------------------------------------------

if(is.null(df.meteo)){
  cat("\n -- ERRORE")
  cat("\n. df.meteo è NULL")
  cat("\n")
  return(NULL)
}



# 02 - Function -----------------------------------------------------------


fn.met.day <- function(df.meteo  = meteo,
                       lbl.month = F,
                       fun.pipe  = F) {
  # Default
  
  df.meteo <- df.meteo %>% 
    group_by(id, data) %>% summarise(t.min   = min(t,   na.rm = T),
                                     t.med   = mean(t,  na.rm = T),
                                     t.max   = max(t,   na.rm = T),
                                     r       = sum(r,   na.rm = T),
                                     rh.med  = mean(rh, na.rm = T),
                                     lw.sum  = sum(lw,  na.rm = T),
                                     .groups = "drop") %>% 
    mutate(anno = format(data, "%Y"),
           mese = format(data, "%m")) %>% 
    select(1:2, 9:10, 3:8)
  
  
  # Add data labels
  
  # month.abb   # nomi dei mesi abbreviati
  # month.name  # nomi dei mesi interi
  
  if(lbl.month){
  
  df.mesi <- tibble(mese = seq(1, 12, 1),
         lbl  = month.abb) %>% 
    mutate(mese = formatC(mese, width = 2, flag = 0))
  
  
  df.meteo <- df.meteo %>% 
    left_join(df.mesi, by = "mese") %>%     #%>% sample_n(10)
    select(1:4, 11, 5:10)
  
  }  ## END IF
  
  
  # Return
  
  if(fun.pipe) invisible(return(df.meteo)) else return(df.meteo)
  
}  ### END FUN



# 03 - Clear --------------------------------------------------------------

# rm(df.meteo, lbl.month, fun.pipe)



# 04 - Usage --------------------------------------------------------------

fn.met.day(df.meteo = meteo, lbl.month = T)




