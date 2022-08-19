#### Musterlösung Parameterschätzung
#### Anne Voormann
#### Workshop kognitive Modellierung SoSe2022

setwd("toYourSouceFileLocation")

library(plyr)
###################################
#### Lösungen Aufgaben Sitzung ####
###################################

# Simulation von Regressionsdaten
set.seed(1234)
beta0 <- 3
beta1 <- 2
epsilon <- rnorm(n = 100, mean = 0, sd = 3)
x1 <- rnorm(n = 100, mean = 10, sd = 4)
y <- beta0 + beta1*x1 + epsilon
data <- cbind(x1, y)




# Funktion Regression mit einem Prädiktor
PredReg <- function(para, predictor){
  b0 <- para[1]
  b1 <- para[2]
  y <- b0 + b1*predictor
  return (y)
}


# Funktion Berechnung der quadrierten Abweichung
RMSE <- function(para, data){
  predictor <- data[,1]
  criterium <- data[,2]
  PredictedData <- PredReg(para, predictor)
  testvalue <- sqrt(sum((criterium - PredictedData)^2)/nrow(data))
  return(testvalue)
}


# Funktion Likelihood-Berechnung
Deviance <- function(para, data){
  predictor <- data[,1]
  criterium <- data[,2]
  PredictedData <- PredReg(para, predictor)
  sd <- sd(PredictedData-criterium)
  mean <- para[1] + para[2]*x
  likelihood <- dnorm(criterium, mean = mean, sd = sd)
  deviance <- -2*sum(log(likelihood))
  return(deviance)
}


# Optimierung der Funktion

minvalue <- 10^10


for(run in 1:50){
  startpar <- c(runif(1, -5, 5),runif(1, 0.5, 10))  
    fit <- optim(startpar, fn = Deviance, data = data)
    
    if(fit$value < minvalue){
      bestfitDeviance <- fit
      minvalue <- bestfitDeviance$value
    }
}


minvalue <- 10^10


for(run in 1:50){
  startpar <- c(runif(1, -5, 5),runif(1, .5, 10))  
  fit <- optim(startpar, fn = RMSE, data = data)
  
  if(fit$value < minvalue){
    bestfitRMSE <- fit
    minvalue <- bestfitRMSE$value
  }
}



#########################################
#### Lösungen Aufgabenblatt ####
########################################

# 1a: Binomial-Verteilung (oder Bernoulli, wenn man es für jeden Durchgang einzeln modelliert)
# 1b: Lognormal, Ex-Gauß, Wald, Weibul
# 1c: Poisson Verteilung



# 2a: Modell für Gedächtnistest
# sei d1 die Wahrscheinlichkeit ein altes Wort als alt zu detektieren
# sei r zufälliges raten 

RecogPred <- function(para, data){
  d1 <- para[1]
  r <- para[2]
  
  npred <- ddply(data, .(ID, wordtype, response), summarize, obs = length(response))
  n <- ddply(data, .(ID, wordtype), summarize, n = length(response))
  npred <- merge(npred, n, by= c("ID", "wordtype"))
  
  phit <- d1 + (1-d1)*r
  pcr <- (1-r)
  
  npred$pred <- ifelse(npred$wordtype == npred$response, 
                       ifelse(npred$wordtype == 0, pcr*npred$n,  phit*npred$n),
                       ifelse(npred$wordtype == 0, (1-pcr)*npred$n,  (1-phit)*npred$n))
  
  pred <- list(prob = c(pcr, phit), n = npred)
  return(pred)
  
}


# 2b: Abweichungsfunktion und Plausibilitätsfunktion
GSquare <- function(para, data){
  pred <- RecogPred(para, data)
  
  G2 <- 2*sum(pred$n$obs*(log(pred$n$obs)-log(pred$n$pred)))
  
  message(pred$prob)
  message(G2)
  message("")
  
  return(G2)
}



Deviance <- function(para, data){
  pred <- RecogPred(para, data)
  
  ncor <- subset(pred$n, wordtype == response)
  
  likelihood <- ifelse(ncor$wordtype == 0, dbinom(ncor$obs, ncor$n, pred$prob[1]), dbinom(ncor$obs, ncor$n, pred$prob[2]))
  deviance <- -2*sum(log(likelihood))
  
  message(pred$prob)
  message(deviance)
  message("")
  
  return(deviance)
}


# 2c: Optimierung der Funktion

load("./recogdata.RData")

minvalue <- 10^10


for(run in 1:10){
  startpar <- runif(2, 0, 1)
  fit <- optim(startpar, fn = Deviance, data = recogdata, method = "L-BFGS-B", lower = c(0.01,0.01), upper = c(0.99,0.99))
  
  if(fit$value < minvalue){
    bestfitDeviance <- fit
    minvalue <- bestfitDeviance$value
  }
}


minvalue <- 10^10


for(run in 1:10){
  startpar <- runif(2, 0, 1)
  fit <- optim(startpar, fn = GSquare, data = recogdata, method = "L-BFGS-B", lower = c(0.01,0.01), upper = c(0.99,0.99))
  
  if(fit$value < minvalue){
    bestfitGSquare <- fit
    minvalue <- bestfitGSquare$value
  }
}


