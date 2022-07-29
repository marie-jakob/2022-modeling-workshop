##-------------------------------------------------------------
## 02. Plots - Solutions
## Cognitive Modeling Summer School Freiburg
## Date: August 2022
## Marie Jakob  <marie.jakob@psychologie.uni-freiburg.de>
## ------------------------------------------------------------

library(tidyr)

penguins <- read.csv("data/penguins_tidy.csv")


#------------------------------------------------------------------------------#
#### Aufgabe 1: Balkendiagramm ####

# Gruppierter Barplot mit Anzahl pro Spezies

# Erstmal aggregieren
bar_plot_data <- table(penguins$species)

barplot(
  height = bar_plot_data,
  # Breite proportional zur Anzahl
  width = bar_plot_data,
  col = c("blue", "green", "red"),
  main = "Frequency of penguin species in sample",
  xlab = "Species",
  ylab = "Frequency",
  # horiz = T # -> stellt die Balken horizontal dar
)


#------------------------------------------------------------------------------#
#### Aufgabe 2: Boxplots ####


cols <- c(rep("blue", 2), rep("green", 2), rep("red", 2))
boxplot(body_mass_g ~ sex + species, 
        data = penguins,
        col = cols,
        names = c("Adelie-f", "Adelie-m",
                  "Gentoo-f", "Gentoo-m",
                  "Chinstrap-f", "Chinstrap-m"))


#------------------------------------------------------------------------------#
#### Aufgabe 3: Scatterplots ####

# Ein Plot für alle Spezies & Geschlechter

# -> die Variable, die die Farbe kodiert, muss dabei vom Typ "factor" sein
# Datentyp nach dem Einlesen:
typeof(penguins$species)

# Ändern zu factor:
penguins$species <- as.factor(penguins$species)

plot(body_mass_g ~ culmen_length_mm, 
     col = species, 
     data = penguins,
     main = "Gewicht ~ Schnabellänge",
     xlab = "Schnabellänge",
     ylab = "Gewicht",
     # pch = 16 erstellt ausgefüllte Kreise
     pch = 16
)


####### Separate Plots

# Fenster aufteilen in 2 Abschnitte:
par(mfrow = c(1, 2))

# Einzelne Bedingungen auswählen kann man z.B. indem man dem "data" Argument
# nur ein Subset des Datensatzes übergibt, das der gewünschten Bedingung entspricht

plot(body_mass_g ~ culmen_length_mm, 
     # Datensatz wird gefiltert: Auswahl nur von Zeilen, die in der Variable "sex" 
     # den Wert "female" haben -> nur weibliche Pinguine
     data = penguins[penguins$sex == "female", ],
     main = "weiblich",
     xlab = "Schnabellänge",
     ylab = "Gewicht",
     # pch = 16 erstellt ausgefüllte Kreise
     pch = 16
)
# -> Hier nur exemplarisch für eine Bedingung, für die anderen Bedingungen filtert
# man den Datensatz analog


# Lösungen mit for Loop:
par(mfrow = c(1, 2))

# Wir gehen alle unterschiedlichen Werte der Variable "sex" durch (also "male" und "female")
# Im Loop heißt diese Variable dann sex_tmp
for (sex_tmp in unique(penguins$sex)) {
  plot(body_mass_g ~ culmen_length_mm, 
       # Auswahl aus dem Datensatz mit Pinguinen, die das Geschlecht sex_tmp haben
       data = penguins[penguins$sex == sex_tmp, ],
       # die Variable übergeben wir auch als Title des Plots
       main = sex_tmp,
       xlab = "Schnabellänge",
       ylab = "Gewicht",
       # pch = 16 erstellt ausgefüllte Kreise
       pch = 16
  )
}



#------------------------------------------------------------------------------#
#### Bonus ####



# Eine schöne Übersicht zu unterschiedlichen Base R Plots findet ihr z.B. hier:
# http://www.sthda.com/english/wiki/r-base-graphs


