#### Musterlösung Modellierung von Reaktionszeiten
#### Anne Voormann
#### Workshop kognitive Modellierung SoSe2022

setwd("C:/Users/Anne Arbeit/Nextcloud/Dokuments/ab_Lehre/ModellingWorkshop")
setwd("K:/Nextcloud/ab_Lehre/ModellingWorkshop")

library(plyr)

load("./RTData.RData")


#############################
#### Log-transformation ####
############################

# Beispiel logarithmus-Transformation

par(mfrow = c(1,2))
hist(RTData$rt, xlab = "RT", main = "", ylab = "", freq = F, breaks = 15)
hist(log(RTData$rt), xlab = "log RT", main = "", ylab = "", freq = F, breaks = 15)
par(mfrow = c(1,1))


png(filename = "./plotRTData.png",
    width = 9, height = 7, units = "cm", res = 500, pointsize = 8)
hist(RTData$rt, xlab = "RT", main = "", ylab = "",  freq = F, breaks = 15)
dev.off()

png(filename = "./plotlogRTData.png",
    width = 9, height = 7, units = "cm", res = 500, pointsize = 8)
hist(log(RTData$rt), xlab = "log RT", main = "", ylab = "", freq = F, breaks = 15)
dev.off()

par(mfrow = c(1,1))







####################
#### Log-normal ####
####################



DLogNormal <- function(para, x){
  mu <- para[1]
  sig <- para[2]
  part1 <- 1/(x*sig*sqrt(2*pi))
  part2 <- exp(-1*(log(x)-mu)^2/(2*sig^2))
  dens <- part1*part2
  return(dens)
}

para <- c(0,1)
x <- seq(0.01,4,0.01)

par(mfrow = c(1,2))
plot(x, DLogNormal(para, x), main = "Log-normal", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(log(x), DLogNormal(para, x), main = "Log-normal", type = "l", xlab = "log RT", ylab = "density", bty = "l")
par(mfrow = c(1,1))

# save plot
# png(filename = "./plotlogNormal.png",
#     width = 14, height = 7, units = "cm", res = 500, pointsize = 8)
# par(mfrow = c(1,2))
# plot(x, DLogNormal(para, x), main = "Log-normal", type = "l", xlab = "RT", ylab = "density", bty = "l")
# plot(log(x), DLogNormal(para, x), main = "Log-normal", type = "l", xlab = "log RT", ylab = "density", bty = "l")
# dev.off()



# Compute Deviance
DevLogNor <- function(para, data){
  likelihood <- DLogNormal(para, data)
  dev <- -2*sum(log(likelihood))
  return(dev)
}

# iterativ minimization
optim(para, fn = DevLogNor, data = RTData$rt, 
      method = "L-BFGS-B", 
      lower = c(-Inf, 0.1))

# algebraically minimization
data <- RTData$rt
mu <- log((mean(data))^2/(sqrt(var(data)+(mean(data))^2)))
sigma <- sqrt(log((var(data)/(mean(data))^2) + 1))









#################
#### Weibull ####
#################

lambda <-  1
k <-  1.5 
x <- seq(0.01,4,0.01)

# plot Weibull für verschiedene k
par(mfrow = c(2,2))
plot(x, dweibull(x, shape = 1, scale = lambda), main = "Weibull - k = 1", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(x, dweibull(x, shape = 1.5, scale = lambda), main = "Weibull - k = 1.5", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(x, dweibull(x, shape = 2, scale = lambda), main = "Weibull - k = 2", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(x, dweibull(x, shape = 2.5, scale = lambda), main = "Weibull - k = 2.5", type = "l", xlab = "RT", ylab = "density", bty = "l")


# plot Weibull für verschiedene lambda
plot(x, dweibull(x, shape = k, scale = 1), main = "Weibull - k = 1", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(x, dweibull(x, shape = k, scale = 1.5), main = "Weibull - k = 1.5", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(x, dweibull(x, shape = k, scale = 2), main = "Weibull - k = 2", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(x, dweibull(x, shape = k, scale = 2.5), main = "Weibull - k = 2.5", type = "l", xlab = "RT", ylab = "density", bty = "l")
par(mfrow = c(1,1))


# save plot
# png(filename = "./plotWeibull.png",
#     width = 7, height = 14, units = "cm", res = 500, pointsize = 8)
# par(mfrow = c(2,1))
# plot(x, dweibull(x, shape = k, scale = lambda), main = "Weibull - k = 1.5", type = "l", xlab = "RT", ylab = "density", bty = "l")
# plot(x, dweibull(x, shape = 1, scale = lambda), main = "Weibull - k = 1", type = "l", xlab = "RT", ylab = "density", bty = "l")
# dev.off()



# Density of shifted Weibul
DShifWeibul <- function(para, x){
  k <- para[1]
  lambda <- para[2]
  theta <- para[3]
  
  part1 <- lambda*k*(lambda*((x-theta)^(k-1)))
  part2 <- exp(-(lambda*(x-theta))^k)
  f <- part1*part2
  
  return(f)
}


theta <- 0.5
para <- c(k, lambda, theta)

# save plot
# png(filename = "./plotWeibullShifted.png",
#     width = 7, height = 14, units = "cm", res = 500, pointsize = 8)
# par(mfrow = c(2,1))
# plot(x, DShifWeibul(c(k, lambda, 0), x), main = "Weibull - theta = 0", type = "l", xlab = "RT", ylab = "density", bty = "l")
# plot(x, DShifWeibul(c(k, lambda, theta), x), main = "Weibull - theta = 0.5", type = "l", xlab = "RT", ylab = "density", bty = "l")
# dev.off()



# Compute Deviance
DevWeib <- function(para, data){
  likelihood <- dweibull(data, shape = para[1], scale = para[2])
  likelihood[likelihood == 0] <- 10^(-16)
  dev <- -2*sum(log(likelihood))
  return(dev)
}

DevWeibShifted <- function(para, data){
  likelihood <- DShifWeibul(para, data)
  likelihood[likelihood == 0] <- 10^(-16)
  dev <- -2*sum(log(likelihood))
  return(dev)
}

# iterative minimization
para <- c(1.5, 1)
optim(para, fn = DevWeib, data = (RTData$rt/1000), method = "L-BFGS-B", lower = c(0.0001, 0.0001))

parashift <- c(1.5, 1, 0)
optim(parashift, fn = DevWeibShifted, data = (RTData$rt/1000), method = "L-BFGS-B", lower = c(0.0001, 0.0001, 0), upper = c(Inf, Inf, (min(RTData$rt/1000)- 0.001)))




##################
#### Ex-Gauss ####
##################

erfc <- function(x) 2*pnorm(x * sqrt(2), lower = FALSE)

# tau = 1/lambda
DexGaussian <- function(para, t){
  mu <- para[1]
  sigma2 <- para[2]
  tau = para[3]
  x <- mu + sigma2/tau - t
  y <- sqrt(2*sigma2)
  denexGaussian <-  0.5/tau * exp((2*mu + sigma2/tau - 2*t) / (2*tau)) * erfc(x/y) 
  return(denexGaussian)
}


#install.packages("gamlss")
#library(gamlss)
#dexGAUS(1, mu = 5, sigma = 1, nu = 2, log = FALSE)

mu = 0.5
sigma = 0.1 
tau = 2
para = c(mu, sigma, tau)
x <- seq(0.01,4,0.01)

par(mfrow = c(2,2))
plot(x, DexGaussian(para, x), main = "ex-Gauss ", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(x, DexGaussian(c(1, 0.1, 2), x), main = "ex-Gauss - higher mu", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(x, DexGaussian(c(0.5, 0.05, 2), x), main = "ex-Gauss - lower sigma", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(x, DexGaussian(c(0.5, 0.1, 0.5), x), main = "ex-Gauss - higher lambda", type = "l", xlab = "RT", ylab = "density", bty = "l")
par(mfrow = c(1,1))

# save plot
# png(filename = "./plotExGauss.png",
#     width = 14, height = 10, units = "cm", res = 500, pointsize = 8)
# par(mfrow = c(2,2))
# plot(x, DexGaussian(para, x), main = "ex-Gauss ", type = "l", xlab = "RT", ylab = "density", bty = "l")
# plot(x, DexGaussian(c(1, 0.1, 2), x), main = "ex-Gauss - higher mu", type = "l", xlab = "RT", ylab = "density", bty = "l")
# plot(x, DexGaussian(c(0.5, 0.05, 2), x), main = "ex-Gauss - lower sigma", type = "l", xlab = "RT", ylab = "density", bty = "l")
# plot(x, DexGaussian(c(0.5, 0.1, 0.5), x), main = "ex-Gauss - higher lambda", type = "l", xlab = "RT", ylab = "density", bty = "l")
# par(mfrow = c(1,1))
# dev.off()



# Deviance ex-Gaussian
DevExGaussian <- function(para, data){
  likelihood <- DexGaussian(para, data)
  likelihood[likelihood <= 0] <- 10^(-16)
  dev <- -2*sum(log(likelihood))
  return(dev)
}

# iterative minimization
para <- c(mu, sigma, tau)
optim(para, fn = DevExGaussian, data = (RTData$rt/1000))







#######################
#### Wald Funktion ####
#######################

x <- seq(0.01,4,0.01)
alpha <- 2
gamma <- 1


DWald <- function(para, x){
  gamma <- para[1]
  alpha <- para[2]
  theta <- para[3]
  part1 <- alpha/(sqrt(2*pi*(x-theta)^3))
  part2 <- exp(-1*(alpha-gamma*(x-theta))^2/(2*(x-theta)))
  dens <- part1*part2
  return(dens)
}


# plot density
para <- c(gamma, alpha, 0)

png(filename = "./plotWald.png",
    width = 14, height = 10, units = "cm", res = 500, pointsize = 8)
par(mfrow = c(2,2))
plot(x, DWald(c(gamma, alpha, 0), x), main = "Wald ", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(x, DWald(c(gamma, 1, 0), x), main = "Wald - lower alpha", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(x, DWald(c(0.5, alpha, 0), x), main = "Wald - lower gamma", type = "l", xlab = "RT", ylab = "density", bty = "l")
plot(x, DWald(c(gamma, alpha, 0.5), x), main = "Wald - higher theta", type = "l", xlab = "RT", ylab = "density", bty = "l")
par(mfrow = c(1,1))
dev.off()


# plot acumulation process
set.seed(246)
samples <- rnorm(1, gamma/100, 1/10)
for(i in 2:length(x)){
  samples[i] <- samples[i-1] + rnorm(1, gamma/100, 1/10)
} 

xplot <- c(0,x[1:min(which(samples > 2))])
samplesplot <- c(0,samples[1:(min(which(samples > 2)))])


png(filename = "./plotWaldProcess.png",
    width = 14, height = 10, units = "cm", res = 500, pointsize = 8)
par(fig= c(0, 1, 0, 0.5), mai = c(1, 1, 0, 1))
plot(xplot, samplesplot, type = "l", xlab = "RT", ylab = "", xlim = c(0, 4), ylim = c(-.01, 2), bty = "l", xaxs = "i")
abline(h = 2)
par(fig= c(0, 1, 0.49, 1), mai = c(0, 1, 1, 1), new = TRUE)
plot(x, DWald(c(gamma, alpha, 0), x), main = "Wald distribution", type = "l", ylab = "", bty = "n", tck = 0, yaxt = "n", xaxt = "n", yaxs = "i", xaxs = "i")
dev.off()


par(mfrow = c(1,1))


# Deviance Wald-Funktion
DevWald <- function(para, data){
  likelihood <- DWald(para, data)
  likelihood[likelihood == 0] <- 10^(-16)
  dev <- -2*sum(log(likelihood))
  return(dev)
}

# iterative minimization
para <- c(gamma, alpha, 2)
optim(para, fn = DevWald, data = (RTData$rt/1000), 
      method = "L-BFGS-B", 
      lower = c(0.0001, 0.0001, 0.0001), 
      upper = c(Inf, Inf, (min(RTData$rt/1000)- 0.001)))




###############################
#### Lösungen Aufgabenblatt####
###############################

# Am Beispiel der Wald-Verteilung

results <- as.data.frame(matrix(data = NA, nrow = length(unique(RTData$vp)), ncol = 5))
names(results) <- c("vp", "gamma", "alpha", "theta", "dev")

para <- c(gamma, alpha, 2)
for (id in unique(RTData$vp)) {
  data <- subset(RTData, vp == id)
  minvalue <- 10^10
  
  for(run in 1:50){
    startpar <- runif(3, 0.0001, 10)
    fit <- optim(startpar, 
                 fn = DevWald, 
                 data = (data$rt/1000),method = "L-BFGS-B", 
                 lower = c(0.0001, 0.0001, 0.0001), 
                 upper = c(Inf, Inf, (min(data$rt/1000)- 0.001)))
  
    if(fit$value < minvalue){
      bestfit <- fit
      minvalue <- bestfit$value
    }
  }
  
  results$vp[id] <- id
  results[id,2:4] <- bestfit$par
  results$dev[id] <- bestfit$value
  
}


# vergleich mittlere Parameter zu aggregierten Parametern:
fitagg <- optim(para, fn = DevWald, data = (RTData$rt/1000), 
      method = "L-BFGS-B", 
      lower = c(0.0001, 0.0001, 0.0001), 
      upper = c(Inf, Inf, (min(RTData$rt/1000)- 0.001)))

colMeans(results[,2:4])
fitagg$par

# Vergleich korrekt / inkorrekt
resultscorr <- as.data.frame(matrix(data = NA, nrow = length(unique(RTData$vp)), ncol = 9))
names(resultscorr) <- c("vp", "gamma_corr", "alpha_corr", "theta_corr", "dev_corr", "gamma_err", "alpha_err", "theta_err", "dev_err")

para <- c(gamma, alpha, 2)
for (id in unique(RTData$vp)) {
  data <- subset(RTData, vp == id)
  
  resultscorr$vp[id] <- id
  
  for (type in c("correct", "error")) {
    data <- subset(data, corr == type)
    minvalue <- 10^10
    
    for(run in 1:50){
      startpar <- runif(3, 0.0001, 10)
      fit <- optim(startpar, 
                   fn = DevWald, 
                   data = (data$rt/1000),method = "L-BFGS-B", 
                   lower = c(0.0001, 0.0001, 0.0001), 
                   upper = c(Inf, Inf, (min(data$rt/1000)- 0.001)))
      
      if(fit$value < minvalue){
        bestfit <- fit
        minvalue <- bestfit$value
      }
    }
    
    if(type == "correct") resultscorr[id, c("gamma_corr", "alpha_corr", "theta_corr", "dev_corr")] <- c(bestfit$par, bestfit$value)
    if(type == "error") resultscorr[id, c("gamma_err", "alpha_err", "theta_err", "dev_err")] <- c(bestfit$par, bestfit$value)

    
  }
}




