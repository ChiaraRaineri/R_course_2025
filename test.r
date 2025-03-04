# 04/03/25

# per fare quello sotto (le sezioni) ctrl + maiusc + r

# [ Prima prova ] ---------------------------------------------------------


# >>> Prima prova <<< -----------------------------------------------------

# non sono in ordine gerarchico (devi trovare il modo di distinguere le sezioni)
# Le sezioni le trovo in alto a destra di fianco a Source (show document outline) oppure in basso a sinistra


###
# ctrl + l per pulire la console
# ctrl + o per aprire la directory e aprire i file


5/3
4/7
5/0
print(5/3)
sqrt(25)



# [01] - Variables ---------------------------------------------------------

# Per fare la freccia alt + -

w <- 25            # Assegnazione 
s <- "Testo"

# per cancellare: rm(s) o anche rm(s, w)

ls()   # dà l'elenco delle variabili



# [02] - Use of variables -------------------------------------------------

lato.maggiore <- 7
lato.minore   <- 5
area          <- lato.maggiore * lato.minore

area
area * 2


iris
iris$Sepal.Length   # Il dollaro permette di selezionare una colonna
head(iris)          # Seleziona le prime 5 righe
head(iris, n = 10)  # Seleziona le prime 10 righe (posso anche non mettere n =)

# Dopo la virgola, ctrl + spazio fa vedere i suggerimenti sugli elementi da mettere


plot(iris$Sepal.Length)


# Informazioni sulle funzioni
?head()
help(head)



# [03] - Functions --------------------------------------------------------

# Parentesi graffe: shift + alt gr + []
# oppure alt + 1 2 3


# Area di un triangolo
area.triangolo <- function(base, altezza) {    # gli faccio imparare la funzione
  res <- base * altezza / 2   # calcoli
  return(res)                 # restituzione valori
}  # END FUNCTION             # lo metto soprattutto quando ho delle funzioni lunghe


area.triangolo(base = 5, altezza = 10)   # ora posso usare la funzione

a <- area.triangolo(6, 8)  # devono essere in sequenza come li ho dichiarati nella function



# Base e altezza sono variabili locali (non del global environment), meglio non usare quelle globali (perché magari non le trovi più)
# Le variabili globali si vedono in Environment, quelle locali no



# [04] - Packages ---------------------------------------------------------










