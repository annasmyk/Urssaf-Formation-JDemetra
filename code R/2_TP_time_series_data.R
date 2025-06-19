#  "TP : Manipulation de séries temporelles avec R"

# Installation packages 

install.packages(c("zoo", "xts", "dplyr", "tsibble", "lubridate", "XLConnect", "tsbox", "imputeTS"))

# Loading packages 

library("dplyr")
library("zoo")
library("xts")
library("tsbox")
library("imputeTS")
library("lubridate")
library("rjd3toolkit")


### Objets TS direcement disponibles 

# exemple 1
ts<-AirPassengers
plot(ts)
# exemple 2
ts<-rjd3toolkit::ABS$X0.2.09.10.M
plot(ts)
rjd3toolkit::ABS$X0.2.09.10.M |> class()


### Import de data frame et creation d'objets TS

# importer ipi_nace4.csv du repertoire Data
ipi <- read.csv2("Data/IPI_nace4.csv")

# quels formatages sont necessaires ?
str(ipi)
ipi$DATE <- as.Date(ipi$DATE, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
str(ipi)

 # creer un objet TS avec la serie RF3030 (attention au start)
y_raw <- ts(ipi[, "RF3030"], start = c(1990, 1), frequency = 12)
#y_raw

### graphique simple 
plot.ts(y_raw)
# # ajout autre courbe
lines(1.5 * y_raw, col = "red")

# avec ts box
tsbox::ts_plot(y_raw, 1.5 * y_raw)


# afficher valeurs janvier 2000 et decembre 2019
y_j2000 <- window(y_raw, start = c(2000, 1), end = c(2000, 1))
y_j2000
y_d2019 <- window(y_raw, start = c(2019, 12), end = c(2019, 12))
y_d2019

# avec tsbox
library(tsbox) # voir aide fonction ts_span
j2000 <- ts_span(y_raw, start = "2000-01-01", end = "2000-01-01")
j2000

d2019 <- ts_span(y_raw, start = "2019-12", end = "2019-12")
d2019

# avec attribut time
time(y_raw)
y_raw[time(y_raw) == 2000.000]
y_raw[time(y_raw) == 2019.000 + 11 / 12]

# en transformant l'attribut time en date (zoo)
y_raw[as.Date(time(y_raw)) == "2000-01-01"]
y_raw[as.Date(time(y_raw)) == "2019-12-01"]


# creer la serie RF3030 entre janvier 2000 et decembre 2019
y_raw_s <- window(y_raw, start = c(2000, 1), end = c(2019, 12))
y_raw_s

y_raw_s2 <- ts_span(y_raw, start = "2000-1", end = "2019-12")
y_raw_s2


# afficher les 3 dernières années et les 4 derniers mois
y_3y <- ts_span(y_raw, "-3 years") #dynamique
y_3y
class(y_3y)

y_4m <- ts_span(y_raw, "-4 months") #dynamique
y_4m

#afficher date de debut, de fin et frequence des 2 dernieres séries créées
start(y_3y)
class(start(y_3y))
start(y_3y)[1]
end(y_3y)
frequency(y_3y)

ts_summary(y_3y)



### Séquences de dates et création data frames 

# creer une serie de dates mensuelles entre fevrier 2019 et novembre 2023

d <- seq(from = as.Date("2019-02-01"),
         to = as.Date("2023-11-01"),
         by = "month")
d

# extraire la date d'un TS
y_raw
time(y_raw)
colonne_date<-as.Date(time(y_raw)) 


# extraire les valeurs de l'année 2020 de la serie Ipi RF3030 (yraw)

y_2020  <- ts_span(y_raw, "2020") # attention faux, ici start = 2020
y_2020
y_2020  <- ts_span(y_raw, start = "2020-1", end = "2020-12")
y_2020



# creer un data frame serie Ipi RF3030 pour l'année 2020

df_2020 <- ts_df(y_2020) # on utilise la date de l'objet TS
View(df_2020)

# creer un data frame avec valeurs Ipi RF3030 de 2020 mais avec date correspondante en 2024

df_2024 <- data.frame(
  date = seq(from = as.Date("2024-01-01"),
             to = as.Date("2024-12-01"),
             by = "month"),
  ipi = y_2020
)
View(df_2024)


### Serie temporelle avec valeurs ad hoc 

## vecteur 
u <- c(121,132,114, 114,0)
u<- 100+ seq(0,2,0.1)
ts <-ts(u, start= c(2022,4), frequency=4)
ts

### Taux de variation 

# ecrire une fonction taux de variation par rapport à la période précedente
# mensuelle ou trimestrielle

### package base et stat: attention comportement different

### package dplyr : sur un vecteur ! 


tx_cr <- function(v) {
  w <- (v - lag(v)) / lag(v) * 100
  return(w)
}

tx_cr(ipi[, "RF3030"])

tx_12 <- function(v) {
  w <- (v - lag(v, 12)) / lag(v, 12) * 100
  return(w)
}

### package tsbox: fonctions directes

ts1  <- ts(1:12, start = c(2024, 1), frequency = 12)
ts_diff(ts1)
ts_pc(ts1)
ts_pcy(ts1)


### Valeurs manquantes 

serie_avec_NA <- ts(c(rep(NA, 12), rep(0, 24), rep(NA, 24),
                      rep(1, 24), rep(NA, 12)), start = 2000, frequency = 12)
serie_avec_NA



# Reperer les positions des valeurs manquantes

p_na <- which(is.na(serie_avec_NA))
p_na

# Enlever les valeurs manquantes au début de la série

etape_1 <- zoo::na.trim(serie_avec_NA, sides = "left")
etape_1

# Interpoler de manière linéaire les valeurs manquantes entre les 0 et les 1

etape_2 <- na.approx(etape_1, na.rm = FALSE) # na.rm = FALSE : on veut garder les NA de la fin
etape_2


# Remplacer les valeurs manquantes à la fin de la série par la dernière valeur observée
etape_3 <- na.locf(etape_2)
etape_3