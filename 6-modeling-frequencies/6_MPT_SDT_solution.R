##-------------------------------------------------------------
## Summer School Day 6 - Demo MPTinR
## Date: August 2022 
## Marie Jakob  <marie.jakob@psychologie.uni-freiburg.de>
## ------------------------------------------------------------

# This script contains the solution for the MPTinR exercise from the
# cognitive modeling summer school (Day 6).

library(MPTinR)

data("d.broeder")


#------------------------------------------------------------------------------#
#### Aufgabe 1 ####


# Specify model with 3 guessing parameters
# -> changes in the last two trees
model_g_3 <- "
  # tree for old items (10-%-old-condition) first 'old' then 'new'
  Do + (1 - Do) * g10
  (1 - Do) * (1 - g10)

  (1 - Dn) * g10 
  Dn + (1 - Dn) * (1 - g10)

  # 25-%-old-condition
  Do + (1 - Do) * g25
  (1 - Do) * (1 - g25)

  (1 - Dn) * g25 
  Dn + (1 - Dn) * (1 - g25)

  # 50-%-old-condition
  Do + (1 - Do) * g50
  (1 - Do) * (1 - g50)

  (1 - Dn) * g50 
  Dn + (1 - Dn) * (1 - g50)

  # 75-%-old-condition
  # -> no additional guessing parameters
  Do + (1 - Do) * (1 - g25)
  (1 - Do) * g25
  # corresponds to: (1 - Do) * (1 - (1 - g25))

  (1 - Dn) * (1 - g25)
  Dn + (1 - Dn) * g25
  # corresponds to: Dn + (1 - Dn) * (1 - (1 - g25))

  # 90-%-old-condition
  # -> same here
  Do + (1 - Do) * (1 - g10)
  (1 - Do) * g10
  # corresponds to: (1 - Do) * (1 - (1 - g10))

  (1 - Dn) * (1 - g10)
  Dn + (1 - Dn) * g10
  # corresponds to: Dn + (1 - Dn) * (1 - (1 - g10))
"


model_g_5 <- "
  # tree for old items (10-%-old-condition) first 'old' then 'new'
  Do + (1 - Do) * g10
  (1 - Do) * (1 - g10)

  (1 - Dn) * g10 
  Dn + (1 - Dn) * (1 - g10)

  # 25-%-old-condition
  # order stays the same, only thing that changes is g
  Do + (1 - Do) * g25
  (1 - Do) * (1 - g25)

  (1 - Dn) * g25 
  Dn + (1 - Dn) * (1 - g25)

  # 50-%-old-condition
  # order stays the same, only thing that changes is g
  Do + (1 - Do) * g50
  (1 - Do) * (1 - g50)

  (1 - Dn) * g50 
  Dn + (1 - Dn) * (1 - g50)

  # 75-%-old-condition
  # order stays the same, only thing that changes is g
  Do + (1 - Do) * g75
  (1 - Do) * (1 - g75)

  (1 - Dn) * g75 
  Dn + (1 - Dn) * (1 - g75)

  # 90-%-old-condition
  # order stays the same, only thing that changes is g
  Do + (1 - Do) * g90
  (1 - Do) * (1 - g90)

  (1 - Dn) * g90 
  Dn + (1 - Dn) * (1 - g90)
"

# Fit the models
fit_g_3 <- fit.mpt(data = d.broeder,
                   model.file = textConnection(model_g_3))

fit_g_5 <- fit.mpt(data = d.broeder,
                   model.file = textConnection(model_g_5))


fit_g_3$parameters$aggregated

fit_g_5$parameters$aggregated

# Compare the two models: 

deviance_g_5 <- - 2 * fit_g_5$goodness.of.fit$aggregated$Log.Likelihood
deviance_g_3 <- - 2 * fit_g_3$goodness.of.fit$aggregated$Log.Likelihood

likelihood_ratio <- deviance_g_3 - deviance_g_5
likelihood_ratio


k_g_3 <- fit_g_3$model.info$aggregated$n.parameters
k_g_5 <- fit_g_5$model.info$aggregated$n.parameters

df <- k_g_5 - k_g_3

pchisq(likelihood_ratio, df = df, lower.tail = F)


#------------------------------------------------------------------------------#
#### Aufgabe 2 ####


compute_evsdt_params <- function(p_h, p_fa) {
  lambda <- - qnorm(p_fa)
  mu <- qnorm(p_h) - qnorm(p_fa)
  print(lambda)
  return(c(
    "lambda" = lambda,
    "mu" = mu
  ))
}

compute_evsdt_params(.964, .788)
compute_evsdt_params(.579, .211)



