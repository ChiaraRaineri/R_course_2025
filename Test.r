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


past.time  <- as.POSIXct(x = "2025-01-07 09:13:27", "%Y-%m-%d %H:%M:%S", tz = "CET")
past.time
attributes(as.POSIXlt(x = "2025-01-07 09:13:27", "%Y-%m-%d %H:%M:%S", tz = "CET"))



# differenza tra date
start.time <- Sys.time()
end.time   <- ISOdate(year = 2025, month = 1, day = 18, hour = 14, min = 20, sec = 45, tz = "CET")

difftime(time1 = start.time, time2 = end.time, tz = "CET", units = "day")
difftime(time1 = start.time, time2 = end.time, tz = "CET", units = "hours")


# lubridate
"package:lubridate" %in% search()



# [09] - Vectors ----------------------------------------------------------

vc.num <- c(1, 2, 8, 5)  # c = concatena
vc.colture <- c("pomodoro", "patata", "lattuga", "carota", "cipolla")

c(3:5, 10:16)  # sequenze di numeri
c(1:10)        # primo e ultimo compresi


# I vettori hanno una dimensione
length(vc.num)
1:length(vc.num)

# La posizione di un elemento nel vettore è l'indice
vc.colture[3]
# oppure
i <- 3
vc.colture[i]

# Per cercare un valore
vc.colture[vc.colture == "patata"]
vc.colture[vc.colture == "peperone"]
which(vc.colture == "patata")  # trova la posizione



# cat() concatena testo e numeri
cat("\n", vc.num[3], "è minore di 5\n   Errore")



colt <- "patata"
if (colt %in% vc.colture) cat(colt, "è presente") else cat(colt, "non è presente")  # grazie a cat posso aggiungere alla stringa il valore della variabile


#
value <- 35
if(!is.character(value)) value <- as.character(value)
class(value)
#



# togliere tutte le variabili che iniziano con qualcosa (in questo caso con vc.)
rm(list = ls(pattern = glob2rx("vc.*")))



# Generare sequenze
seq(from = 0, to = 100, by = 10)

seq.Date(from = as.Date("2019-01-01, %Y-%m-%d"), 
         to =   as.Date("2019-05-31, %Y-%m-%d"), 
         by =   "15 day")

seq.POSIXt(from = ISOdate(2025, 1, 27, hour = 10, tz ="CET"),
           to   = ISOdate(2025, 1, 27, hour = 14, tz ="CET"),
           by   = "hour")


# Distribuzioni casuali
set.seed(123)                                 # non conta il numero che ci metto dentro (però per fare più operazioni con le stesse caratteristiche devo usare lo stesso)
runif(n =10, min = 10, max = 50)              # non sono ordinati

set.seed(123)
sample(x = 1:365, size = 10, replace = TRUE)  # replace=TRUE vuol dire che può prendere due volte lo stesso numero

set.seed(123)
vc.rnd <- runif(n = 100, min = 1, max = 400)
hist(vc.rnd, breaks = seq(0, 400, 20))        # cambia ogni volta perché sono numeri random; seq significa da 0 a 400 con un passo di 20

set.seed(123)
vc.norm <- rnorm(n= 100, mean = 0, sd = 1)    # distribuzione normale (randomica)
hist(vc.norm, breaks = seq(-4, +4, .1))


sample(LETTERS[1:3], replace = T, size = 100)



# [10] - Statistics -------------------------------------------------------

# Statistiche descrittive

res <- summary(rnorm(n= 100, mean = 10, sd = 2))
res
res[3]  # è il terzo indice (la mediana)

# res è un vettore con le colonne nominate
# per togliere il nome delle colonne (così posso usare direttamente i valori della statistica):
unname(res)


# per vedere i quantili
vc.norm
quantile(vc.norm, probs = c(.25, .5, .75), na.rm = T, names = T)  # na.rm toglie i NA, names dà il nome alle colonne


# library("DescTools")
Desc(vc.norm)           # dà le statistiche descrittive

# frequenza (e percentuale e frequenza cumulata e cumulata percentuale)
df <- Freq(vc.norm, breaks = seq(-3, +3, .5))   # è un data frame
df$freq



# [11] - Matrices ---------------------------------------------------------

# Sono tabelle con lo stesso tipo di dati

mx <- matrix(data = c(1:20), nrow = 5, ncol = 4)
dim(mx)   # numero di righe e numero di colonne
mx[2, 3]  # coordinate di riga e colonna
mx[, 2]   # vettore della colonna 2
mx[3, ]   # vettire della riga 3


# Combinare vettori
vc.dati.a <- c(1, 3, 4, 6)
vc.dati.b <- c(11, 33, 55, 66)
cbind(vc.dati.a, vc.dati.b)    # per colonna
rbind(vc.dati.a, vc.dati.b)    # per riga



# [12] - Data frames ------------------------------------------------------

set.seed(123)
df.test <- data.frame(id     = as.integer(seq(1, 5, 1)),
                      day    = seq.Date(from = as.Date("2025-02-14", "%Y-%m-%d"), length.out = 5, by = "2 week"),
                      classe = sample(LETTERS[1:3], size = 5, replace = T),
                      valido = sample(c(T, F), size = 5, replace = T),
                      peso   = rnorm(n = 5, mean = 10, sd = 2))



# 11/03/25

# Altro modo per scrivere il df di prima
set.seed(123)

id     <-  as.integer(seq(1, 5, 1))
day    <-  seq.Date(from = as.Date("2025-02-14", "%Y-%m-%d"), length.out = 5, by = "2 week")
classe <-  sample(LETTERS[1:3], size = 5, replace = T)
valido <-  sample(c(T, F), size = 5, replace = T)
peso   <-  rnorm(n = 5, mean = 10, sd = 2)

tmp    <- as.data.frame(cbind(id, day, classe, valido, peso))  # il problema è che restituisce i numeri come caratteri (bisogna specificare il tipo)
str(tmp)


# Richiamare valori
df.test$day 
df.test[, c(2)]     # tutte le righe di una colonna (è un vettore)
df.test[, c(2, 4)]  # quando sono più colonne è un data frame
df.test[2, ]        # seleziona la colonna

# usando dplyr
select(df.test, day)               # restituisce un data frame
pull(df.test, day)                 # restituisce un vettore (destruttura un oggetto complesso come un data frame)
slice(df.test, 2)                  # restituisce una colonna
df.test[df.test$classe == "C", ]   # restituisce un data frame con solo gli elementi con valore C nella colonna classe

subset(df.test, peso >= 9, c(2,5))  # condizione sulle righe per selezionare le colonne

# altro modo per scrivere subset usando il piping
df.test %>%                # questo simbolo (pipes) indica che l'elemento prima viene passato a quello dopo
  filter(peso >= 9) %>%    # questo risultato viene passato a quello dopo (sono azioni a cascata)
  select(2, 5)

# Per fare il simbolo del pipe: ctrl + shift + m
# Facendo il pipe non c'è bisogno delle variabili temporanee



str(iris)
DescTools::Desc(iris$Sepal.Length)
# filtrare per specie
iris %>% 
  filter(Species %in% c("setosa", "virginica")) %>%    # ho evitato di usare la sintassi and
  select(Petal.Length, Sepal.Length)                   # seleziono i valori delle colonne indicate che corrispondono alle specie che ho scelto prima

# dentro un pipe si può commentare una riga per provare più soluzioni


