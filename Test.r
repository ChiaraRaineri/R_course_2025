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

DescTools::Desc(iris$Sepal.Length)   # Richiama specifiche funzioni all'interno del pacchetto (in questo caso Desc)

library("DescTools")
library("tidyverse")  # tidyverse è un insieme di packages (include ggplot2, dplyr e altri)

packageVersion("DescTools")          # Versione del package

"package:ggplo2" %in% search()       # Ci dice se l'ho già caricato nell'ambiente (in questo caso no = FALSE)
"package:DescTools" %in% search()


# Per fare uno script nuovo: ctrl +shift + n



# 06/03/25

# [05] - Logicals --------------------------------------------------------

estate <- FALSE
primavera <-TRUE

class(primavera)


if (estate) print("E' vero") else print("Non è vero")   # if(estate)=T è sottinteso
if (!estate) print("E' vero") else print("Non è vero")  # Con il ! davanti è la negazione if(NOT estate)=T

if (is.character("35")) ("ok") else ("no")


rm(estate, primavera)



# [06] - Numbers ----------------------------------------------------------

n <- 32
class(n)
typeof(n)

i <- 32L   # la L per farlo diventare un numero intero
typeof(i)

r <- 26.67
class(r)
typeof(r)

# arrotondamento
round(2.4636)     # arrotonda per difetto
round(2.4636, 2)  # si può decidere il numero di decimali
ceiling(2.4363)   # arrotonda per eccesso

# conversioni
as.character(2.555)

format(2.555, decimal.mark = ",")  # si può formattare la stringa

formatC(x = 123, digits = 0, width = 6, flag = "0", mode = "integer")  # deriva dal linguaggio C

formatC(1, width = 2, flag = "0") # lo 0 davanti serve per mettere ad esempio i mesi in ordine numerico 


d1 <- as.numeric("123.3")
class(d1)
d1 * 3



# [07] - Characters -------------------------------------------------------

coltura <- "mais"
class(coltura)
typeof(coltura)

n <- 3

stringa <- paste(formatC(n, width = 2, flag = "0"), coltura, sep = " - ")  # paste concatena più stringe (anche con un separatore)
# estrarre i caratteri
substr(stringa, start = 1, stop = 2)               # estraggo i valori dal primo carattere al secondo (compresi)
substr(stringa, start = 0, stop = 1)               # R inizia a contare da 1 (non come python che inizia da 0)
substr(stringa, start = 6, stop = nchar(stringa))  # nchar arriva fino alla fine della stringa

# concatenare stringhe
n    <- 3
anno <- 2025
mese <- "Marzo"

paste(formatC(n, width = 2, flag = "0"), mese, anno, sep = " - ")



# [08] - Dates ------------------------------------------------------------

# %Y maiuscolo è l'anno esteso, %y minuscolo sono le ultime 2 cifre
# formato ISO %Y-%m-%d

oggi <- Sys.Date()  # è la data di oggi in formato ISO
oggi
class(oggi)

ora <- Sys.time()   # data, ora, minuti, secondi e time zone (del sistema, Central Europe Time)
ora                 # Per scambiarsi dati è meglio usare UTC
class(ora)


# convertire stringhe in date
start.date <- "07-04-2020"
class(start.date)

start.date <- as.Date("07-04-2020", "%d-%m-%Y", tz = "CET")
class(start.date)

end.date <- as.Date("10-04-2020", "%d-%m-%Y", tz = "CET")
class(end.date)


# calcolare la differenza tra data di inizio e di fine
differenza <- end.date - start.date
differenza   # così risponde con una stringa (non utile per fare dei calcoli)
attributes(differenza)

as.integer(differenza)  # per avere come risultato un numero

end.date <- end.date + 3
end.date                       # di default dà il formato ISO
format(end.date, "%d-%m-%Y")

format(end.date, "%Y")
format(end.date, "%W")  # numero progressivo delle settimane


ISOdate(year = 2025, month = 3, day = 1, hour = 15, min = 32, sec = 20, tz = "CET")  # possiamo costruire noi la data


start.time <- Sys.time()
end.time   <- ISOdate(year = 2025, month = 1, day = 18, hour = 14, min = 20, sec = 45, tz = "CET")
past.time  <- as.POSIXct(x = "2025-01-07 09:13:27", "%Y-%m-%d %H:%M:%S", tz = "CET")
past.time

attributes(as.POSIXlt(x = "2025-01-07 09:13:27", "%Y-%m-%d %H:%M:%S", tz = "CET"))













