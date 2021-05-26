# Importation des données
RawData <- read.csv("Data/Fifa_players.csv")

# Sélection des colonnes
data <- RawData[, c(22,28,66)]
data <- data[complete.cases(data),]

#Mise en forme 
data$Weight <- as.integer(substr(data$Weight, 1,3))
data$Weight <- ifelse(data$Weight<170, "leger", "lourd")

#Variable de population :
#Nombre d'individus : 
N <- nrow(data) # =18207

#Taille des échantillons voulus :
round(0.05*N) # =910

#Variable d'interêt :
Y <- data$SprintSpeed
head(Y,100)
boxplot(Y)

#Paramêtre à retrouver :
teta <- sum(Y) # =1175377
mu <- round(teta/N,3) # =64.727
var_teta <- sum((Y-mu)^2) # =3897090
p <- round(nrow(data[data$Weight == "leger",])/N,3) # =0.587

#### Plan de sondage de Bernoulli ####
PI     <- 0.05                            # Proportion souhaitée dans l'échantillon de Bernoulli
xi     <- runif(N)                        # Loi Uniforme de taille N
BE.index <- ifelse(xi < PI, TRUE, FALSE)  # Sélection des individus pour lesquels xi < PI
BE.sample <- data[BE.index, ]             # Création de l'échantillon
BE.n <- nrow(BE.sample)                   # Taille aléatoire qui suit Bin(18207, 0.05)
head(BE.sample)                           # Haut de l'échantillon
boxplot(Y, BE.sample$SprintSpeed, names = c("Y", "Y hat"), col = c("lightblue","pink"))

#On cherche d'abord à estimer la moyenne de la vitesse :
#Estimation du paramêtre :
BE.teta_y_hat <- 1/PI*sum(BE.sample$SprintSpeed) #Estimateur du total, θ chapeau
BE.var_teta_hat <- ((1-PI)/PI)*sum(BE.sample$SprintSpeed^2) #Estimation de la variance de l'estimateur du total
BE.mu_y_hat <-  round((1/BE.n)*sum(BE.sample$SprintSpeed), 3) #Estimateur de la moyenne, μ chapeau
BE.sigma_carre <- (BE.n/(BE.n-1))*((1/BE.n)*sum(BE.sample$SprintSpeed^2)-((1/BE.n)*sum(BE.sample$SprintSpeed))^2) #Variance débiaisée
BE.var_mu_hat <- (1-BE.n/N)*(BE.sigma_carre/BE.n) #Variance de l'estimateur μ
paste0("On obtient donc pour μ chapeau la valeur ", BE.mu_y_hat)

#Intervalle de confiance de mu chapeau à 95% de confiance :
BE.I_inf <- BE.mu_y_hat-1.96*sqrt(BE.var_mu_hat)
BE.I_sup <- BE.mu_y_hat+1.96*sqrt(BE.var_mu_hat)

#On cherche ensuite a estimer la proportion de poids leger dans la population
BE.p_hat <- round(nrow(BE.sample[BE.sample$Weight == "leger",])/nrow(BE.sample),3)
