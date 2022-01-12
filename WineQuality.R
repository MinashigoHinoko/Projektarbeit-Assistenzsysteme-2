setwd("C:/Users/User/Desktop/Uni/1.Vorlesungen/3.Semester_WS_2021-2022/02.Projekte/03.Assistenzsysteme/Projektarbeit_Wein_Qualität")

WeinR <- read.csv("winequality-red.csv", header=TRUE, sep=";",fill=TRUE)
WeinW <- read.csv("winequality-white.csv", header=TRUE, sep=";",fill=TRUE)

# Berechnung des neuronalen Netzes

  # Laden des R-Pakets
  library(ANN2)

  # Erstellen eines Datensatzes mit Dummy-Codierung der kategoriellen Variablen
  X <- model.matrix(alcohol ~ quality + density, WeinR)
  X <- X[,-1]   # entferne den Intercept

  y <- WeinR[,"alcohol"]
# Trainieren des neuronalen Netzes
# mit 2 Hidden Layer, wobei der 1. Hidden Layer 4 Hidden Units hat und
# der 2. Hidden Layer 3 Units hat

modelR <- neuralnetwork(X, y, hidden.layers=c(4,3), regression = TRUE,
                       loss.type = "absolute", learn.rates = 1e-04,n.epochs = 100,
                       verbose=FALSE)
# Erstellen eines Datensatzes mit Dummy-Codierung der kategoriellen Variablen
X <- model.matrix(alcohol ~ quality + density, WeinW)
X <- X[,-1]   # entferne den Intercept

y <- WeinW[,"alcohol"]
# Trainieren des neuronalen Netzes
# mit 2 Hidden Layer, wobei der 1. Hidden Layer 4 Hidden Units hat und
# der 2. Hidden Layer 3 Units hat

modelW <- neuralnetwork(X, y, hidden.layers=c(4,3), regression = TRUE,
                        loss.type = "absolute", learn.rates = 1e-04,n.epochs = 100,
                        verbose=FALSE)
# Starten der Shiny-App

library(shiny)
library(shinyWidgets)

runApp("App-WeinAlcoholgehalt")
