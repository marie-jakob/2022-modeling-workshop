
#--------------------------------
# chi-quadrat Anpassungstest
#---------------------------
x <- c(89, 37, 30, 28, 2)
N <- sum(x)
p <- c(.4, .2, .2, .15, .05)
## mit chisq.test()


## mit Formel
## Hinweis: berechnen Sie erst m



#--------------------------------
# G-test
#-------
## mit g.test() vom R-Paket AMR


## mit Formel



#--------------------------------
# Devianzmasse
#-------------
## Lineare Modelle mit lm()


## Modell Devianz, Devianz und relative Devianz


## Mit Annes Funktionen (etwas verkuerzt)
PredReg1 <- function(para, predictor){
  b0 <- para[1]
  b1 <- para[2]
  y <- b0 + b1*predictor
  return (y)
}
Deviance1 <- function(para, data){
  predictor <- data[,2]
  criterium <- data[,1]
  PredictedData <- PredReg1(para, predictor)
  likelihood <- dnorm(criterium, 
                      mean = PredictedData, 
                      sd = sd(criterium-PredictedData))
  deviance <- -2*sum(log(likelihood))
  return(deviance)
}

minvalue <- 10^10

for(run in 1:5){
  startpar <- c(runif(1, -5, 5),runif(1, 0.5, 10))  
  fit <- optim(par = startpar, fn = Deviance1, data = df)
  
  if(fit$value < minvalue){
    bestfitDeviance1 <- fit
    minvalue <- bestfitDeviance$value
  }
}
M_Devianz1 <- bestfitDeviance1$value



#--------------------------------
# Likelihood Ratio
#-----------------
## mit anova() FUnktion und den lm-Objekten von Oben
# install.packages("lmtest")
library(lmtest)


## mit Annes Funktionen
### Nullmodell
PredReg0 <- function(para, predictor){
  b0 <- para[1]
  y <- b0
  return (y)
}
Deviance0 <- function(para, data){
  criterium <- data[,1]
  PredictedData <- PredReg0(para, predictor)
  likelihood <- dnorm(criterium, 
                      mean = PredictedData, 
                      sd = sd(criterium-PredictedData))
  deviance <- -2*sum(log(likelihood))
  return(deviance)
}

minvalue <- 10^10

for(run in 1:5){
  startpar <- runif(1, -5, 5)  
  fit <- optim(par = startpar, fn = Deviance0, data = df)
  
  if(fit$value < minvalue){
    bestfitDeviance0 <- fit
    minvalue <- bestfitDeviance$value
  }
}
M_Devianz0 <- bestfitDeviance0$value
### hier kommt die Formel 



#--------------------------------
# AIC
#----
## mit AIC()


## mit Formel bzw. mit M_Devianz1 und der Formel



#--------------------------------
# BIC
#----
## mit BIC()


## mit Formel bzw. mit M_Devianz1 und der Formel




