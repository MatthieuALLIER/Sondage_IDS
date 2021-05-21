# Importation des données
RawData <- read.csv("Data/Fifa_players.csv")
# Sélection des colonnes
data <- RawData[, c(22,28,66)]
#Mise en forme 
data$Weight <- as.integer(substr(data$Weight, 1,3))

