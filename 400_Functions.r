
# [ FUNZIONI ] ------------------------------------------------------------



# >>> Funzione di esempio <<< ---------------------------------------------



# . Parametri -------------------------------------------------------------

# df.meteo <- meteo
# f.anno   <- 2021
# f.mese   <- 3
# f.frac   <- .2
# g.titolo <-  "titolo del grafico"
# f.out    <- "gra" # c("gra", "df")

res <- df.meteo %>% 
    filter(anno == f.anno & mese == f.mese) %>% 
    sample_frac(size = f.frac) %>% 
    ggplot(aes(x = rh, y = t)) + geom_point()



# . Debug -----------------------------------------------------------------

rm(df.meteo, f.anno, f.mese, f.frac)
rm(res)


# Per creare la funzione: seleziono il data frame, vado in alto su Code, Extract Function, dò il nome alla funzione

# dentro la funzione function() inserisco i miei parametri. Nell'output, se non li specifico, userà quelli di default (ovvero quelli in function).


fn.gra.rht <- function(df.meteo = meteo, 
                       f.anno   = NULL,    # NULL perché voglio inserire io i valori quando si arriva all'usage
                       f.mese   = NULL, 
                       f.frac   = .2) {
  # calcolo
  res <- df.meteo %>%                    
    filter(anno == f.anno & mese == f.mese) %>% 
    sample_frac(size = f.frac) %>% 
    ggplot(aes(x = rh, y = t)) + geom_point()
  # return
  return(res)
  
}  # END FUN



# . Usage -----------------------------------------------------------------

# qui è il momento in cui faccio l'output della funzione. Se non specifico il valore dei parametri mi resitutisce quelli di default che avevo messo in function()

fn.gra.rht(f.anno = 2021, f.mese = 3)
fn.gra.rht(f.anno = 2019, f.mese = 8)
fn.gra.rht(f.anno = 2019, f.mese = 8, f.frac = .8)



# . Controllo dei parametri -----------------------------------------------


# 20/03/25


fn.gra.rht <- function(df.meteo = meteo, 
                       f.anno   = NULL,
                       f.mese   = NULL, 
                       f.frac   = .2) {
  
  
  if(!f.mese %in% c(1:12)){                             # controllo sul mese
    cat("\n-- [Errore] -------------------------")
    cat("\n . Mese non compreso tra 1 e 12")
    cat("\n . Hai inserito", f.mese)
    cat("\n-------------------------------------")
  }  # END IF
  
  if(!f.anno %in% c(2019:2021)){                        # controllo sull'anno
    cat("\n-- [Errore] -------------------------")
    cat("\n . Anno non compreso tra 2019 e 2021")
    cat("\n . Hai inserito", f.anno)
    cat("\n-------------------------------------")
  }  # END IF


  res <- df.meteo %>%                    
    filter(anno == f.anno & mese == f.mese) %>% 
    sample_frac(size = f.frac) %>% 
    ggplot(aes(x = rh, y = t)) + geom_point()

  return(res)
  
}  # END FUN



fn.gra.rht(f.anno = 2021, f.mese = 14)
fn.gra.rht(f.anno = 2011, f.mese = 4)



# . Sdoppio la parte di calcolo -------------------------------------------

# il grafico nella funzione di prima lo faceva di default, adesso gli posso dire se darmi solo il data frame (df) o anche il grafico (gra)
# in più voglio decidere se far visualizzare il messaggio di errore o se mettere solo NULL


fn.gra.rht <- function(df.meteo = meteo, 
                       f.anno   = NULL,
                       f.mese   = NULL, 
                       f.frac   = .2,
                       g.titolo = "titolo del grafico",
                       f.out    = "gra",                # output (grafico o data frame)
                       f.echo   = T) {                  # voglio che venga scritto il messaggio di errore?
  
  
  if(!f.mese %in% c(1:12)){                             # controllo sul mese
    
    if(f.echo){                                         # se f.echo = T stampa il messaggio di errore, altrimenti NULL
    cat("\n-- [Errore] -------------------------")
    cat("\n . Mese non compreso tra 1 e 12")
    cat("\n . Hai inserito", f.mese)
    cat("\n-------------------------------------")
    cat("\n")
    }
    
    return(NULL)                                        # se faccio script più complessi è più comodo che mi ritorni anche un NULL
    
  }  # END IF
  
  if(!f.anno %in% c(2019:2021)){                        # controllo sull'anno
    
    if(f.echo){
    cat("\n-- [Errore] -------------------------")
    cat("\n . Anno non compreso tra 2019 e 2021")
    cat("\n . Hai inserito", f.anno)
    cat("\n-------------------------------------")
    cat("\n")
    }
    
    return(NULL)
    
  }  # END IF
  
  f.out <- tolower(f.out)                               # guida l'output della funzione (o il data frame o il grafico). tolower fa in modo che la stringa sia tutta minuscola
  if(!f.out %in% c("gra", "df")){                       # controllo sull'anno
    cat("\n-- [Errore] -------------------------")
    cat("\n . f.out non compreso in gra o df")
    cat("\n . Hai inserito", f.out)
    cat("\n-------------------------------------")
    cat("\n")
    
    return(NULL)
    
  }  # END IF
  
  
  res.df <- df.meteo %>%                              # restituisce il data frame
    filter(anno == f.anno & mese == f.mese) %>% 
    sample_frac(size = f.frac)
  
  if(f.out == "gra"){
    res.g <- res.df %>%                               # restituisce il grafico
      ggplot(aes(x = rh, y = t)) + geom_point() +
      labs(title = g.titolo)
    
  }  # END IF
  
  
  # return
  if(f.out == "df")  return(res.df)
  if(f.out == "gra") return(res.g)
  
}  # END FUN


# Output

fn.gra.rht(f.anno = 2019, f.mese = 4, f.out = "GRA")  # restituisce solo il grafico (anche se è maiuscolo va bene perché prima ho messo tolower)
fn.gra.rht(f.anno = 2019, f.mese = 4, f.out = "df")   # restituisce solo il data frame
fn.gra.rht(f.anno = 2019, f.mese = 4, f.out = "fg")

fn.gra.rht(f.anno = 2019, f.mese = 41, f.echo = F)    # se f.echo = F restituisce solo NULL senza il messaggio di errore

if(is.null(fn.gra.rht(f.anno = 2019, f.mese = 18, f.out = "df", f.echo = F))) stop("la funzione restituisce NULL")  # quando si usa stop si blocca lo script e restituisce la parola Errore: (in cui si può mettere un messaggio)


fn.gra.rht(f.anno = 2019, f.mese = 4, f.out = "gra")                          # così mette i valori di default ("titolo del grafico")
fn.gra.rht(f.anno = 2019, f.mese = 4, f.out = "gra", g.titolo = "Anno 2019")  # così lo posso personalizzare



# >>> Debug delle funzioni <<< --------------------------------------------


# funzione di esempio (guarda la documentazione)


# Parametri
# n <- 10 

fn.test <- function(n = 10) {
  # Setup
  set.seed(123)
  totale <- 0
  
  # Calcolo
  for(i in 1:n){
    numero <- runif(n = 1, min = 10, max = 100)
    totale <- totale + numero / 10
    #browser()   # browser fa vedere nell'ambiente il valore dei parametri, ci fa fare passo passo gli step della funzione e serve per il debug
  
  }  # END FOR
  
  # Return
  res <- totale + 1000
  #browser()             
  return(res)
  
}  # END FUN

# Debug
# rm(n, totale, i, res, numero)

# Usage
fn.test()
fn.test(n = 3)










