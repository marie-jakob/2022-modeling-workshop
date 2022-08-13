##-------------------------------------------------------------
## Summer School Day 6 - Demo MPTinR
## Date: August 2022 
## Marie Jakob  <marie.jakob@psychologie.uni-freiburg.de>
## ------------------------------------------------------------

# This script contains a short demonstration of the basic
# functionality of the MPTinR package using the 2HT model
# and data of Bröder & Schütz (2009)


library(MPTinR)

data("d.broeder")

View(d.broeder)
# rows: participants


# columns:
# 1 - 4: 10 % old condition
#   1: old - "old"
#   2: old - "yes"
#   3: new - "old"
#   4: new - "new"
# 5 - 8: 25 % old condition
# etc.


# build the model string
# -> each row contains the model equation for one category
# row 1 -> column 1
model_g_varies <- "
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

  
# Was müssen wir verändern? 

model_g_fixed <- "
  # tree for old items (10-%-old-condition) first 'old' then 'new'
  Do + (1 - Do) * g
  (1 - Do) * (1 - g)

  (1 - Dn) * g 
  Dn + (1 - Dn) * (1 - g)

  # 25-%-old-condition
  Do + (1 - Do) * g
  (1 - Do) * (1 - g)

  (1 - Dn) * g 
  Dn + (1 - Dn) * (1 - g)

  # 50-%-old-condition
  Do + (1 - Do) * g
  (1 - Do) * (1 - g)

  (1 - Dn) * g 
  Dn + (1 - Dn) * (1 - g)

  # 75-%-old-condition
  Do + (1 - Do) * g
  (1 - Do) * (1 - g)

  (1 - Dn) * g 
  Dn + (1 - Dn) * (1 - g)

  # 90-%-old-condition
  Do + (1 - Do) * g
  (1 - Do) * (1 - g)

  (1 - Dn) * g 
  Dn + (1 - Dn) * (1 - g)
"


# Modelle fitten
# standard: fittet 5 mal, wegen möglicher lokaler Minima
# -> fittet Modelle sowohl auf individuellen, als auch auf aggregierten Daten
# Wir beachten hier erstmal nur aggregierte
fit_g_varies <- fit.mpt(data = d.broeder,
                           model.file = textConnection(model_g_varies))

fit_g_fixed <- fit.mpt(data = d.broeder,
                           model.file = textConnection(model_g_fixed))

# Warn-Meldungen -> manche individuellen Datensätze lassen sich nicht fitten

str(fit_g_varies)

# inspect parameters simple model
fit_g_fixed$parameters$aggregated

# Interpretation?


# Was erwarten wir für die Rate-Parameter des komplexeren Modells?
# Rate Wahrscheinlichkeit wird immer höher, je höher der Anteil alter Wörter ist
fit_g_varies$parameters$aggregated

# Vergleich der d-Parameter zwischen den beiden Modellen


# -> Wie können wir die beiden Modelle vergleichen? 
# -> Likelihood Ratio Test

deviance_g_varies <- - 2 * fit_g_varies$goodness.of.fit$aggregated$Log.Likelihood
deviance_g_fixed <- - 2 * fit_g_fixed$goodness.of.fit$aggregated$Log.Likelihood

likelihood_ratio <- deviance_g_fixed - deviance_g_varies
likelihood_ratio

# Testen anhand einer chi^2 Verteilung
# -> Wie viele Freiheitsgrade? 
# -> Differenz aus der Anzahl der Parameter der beiden Modelle

k_g_fixed <- fit_g_fixed$model.info$aggregated$n.parameters
k_g_varies <- fit_g_varies$model.info$aggregated$n.parameters

df <- k_g_varies - k_g_fixed

# Logik des Tests: die Differenz der Deviance beiden genesteten Modelle folgt
# (asymptotisch) unter der Nullhypothese (die Daten kommen aus dem einfacheren Modell,
# die zusätzlichen Parameter im komplexeren Modell verbessern den Fit also nur "zufällig")
# einer Chi^2-Verteilung mit Freiheitsgrad k (Different in der Anzahl der Parameter)

# -> Wie wahrscheinlich ist die beobachtete Teststatistik unter der Nullhypothese? 

# compute p value

curve(dchisq(x, df), from = 0, to = 20)

pchisq(likelihood_ratio, df = df, lower.tail = F)
# -> sehr unwahrscheinlich


# Vergleich von nicht genesteten Modellen
select.mpt(list(fit_g_fixed, fit_g_varies))



