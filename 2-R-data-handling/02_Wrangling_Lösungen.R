##-------------------------------------------------------------
## 02. Data Wrangling - Solutions
## Cognitive Modeling Summer School Freiburg
## Date: August 2022
## Marie Jakob  <marie.jakob@psychologie.uni-freiburg.de>
## ------------------------------------------------------------


# install.packages("tidyr")
library(tidyr)

penguins_raw <- read.csv("data/penguins_raw.csv")


#------------------------------------------------------------------------------#
#### Aufgabe 1 ####


# Anzahl Variablen
ncol(penguins_raw)

# Anzahl Beobachtungen
nrow(penguins_raw)

# Pinguinarten
length(unique(penguins_raw$Species))

# Wie viele Adelie
length(which(penguins_raw$Species == "Adelie Penguin (Pygoscelis adeliae)"))


#------------------------------------------------------------------------------#
#### Aufgabe 2 ####


# Subset erstellen
names(penguins_raw)
penguins_tidy <- penguins_raw[, c("Species", "Individual.ID", "Island", "Culmen.Length..mm.", "Culmen.Depth..mm.", 
                                  "Flipper.Length..mm.", "Body.Mass..g.", "Sex")]

# snake case
names(penguins_tidy) <- c("species", "ID", "island", "culmen_length_mm", "culmen_depth_mm", "flipper_length_mm",
                          "body_mass_g", "sex")

# recode variables
penguins_tidy$sex <- ifelse(penguins_tidy$sex == "MALE", "male", "female")

penguins_tidy$species <- ifelse(penguins_tidy$species == "Adelie Penguin (Pygoscelis adeliae)", "Adelie",
                                ifelse(penguins_tidy$species == "Gentoo penguin (Pygoscelis papua)", "Gentoo", "Chinstrap"))


# exclude rows with missing values
penguins_tidy <- penguins_tidy[complete.cases(penguins_tidy), ]


#------------------------------------------------------------------------------#
#### Aufgabe 3 ####


# Zusammenfassung
summary(penguins_tidy)


# Wie viele Pinguinarten auf welcher Insel?
table(penguins_tidy$species, penguins_tidy$island)

# Wie viele weibliche und männliche Pinguine für die unterschiedlichen Spezies? 
table(penguins_tidy$species, penguins_tidy$sex)

# Mittelwerte der metrischen Variablen
means_culmen_length <- aggregate(culmen_length_mm ~ species, data = penguins_tidy, FUN = mean)
means_culmen_depth <- aggregate(culmen_depth_mm ~ species, data = penguins_tidy, FUN = mean)
means_flipper_length <- aggregate(flipper_length_mm ~ species, data = penguins_tidy, FUN = mean)
means_body_mass <- aggregate(body_mass_g ~ species, data = penguins_tidy, FUN = mean)

# Zusammenfügen
# Zwei Möglichkeiten: 
# (1) mit merge()
means <- merge(means_culmen_length, means_culmen_depth, by = "species")
means <- merge(means, means_flipper_length, by = "species")
means <- merge(means, means_body_mass, by = "species")


# (2) mit cbind() -> Hier von allen dfs außer einem nur die zweite Spalte benutzen
means <- cbind(means_culmen_length, means_culmen_depth[, 2], means_flipper_length[, 2], means_body_mass[, 2])
# -> umbenennen
names(means) <- c("species", "mean_culmen_length", "mean_culmen_depth", "mean_flipper_length", "mean_body_mass")
 

# Der Datensatz bestehend aus den Mittelwerten ist im Wide Format

means_long <- pivot_longer(means,
                           cols = c("mean_culmen_length", "mean_culmen_depth", "mean_flipper_length", "mean_body_mass"),
                           values_to = "mean",
                           names_to = "variable")
means_wide <- pivot_wider(means_long,
                          id_cols = "species",
                          names_from = "variable",
                          values_from = "mean")

# Leichteste Pinguine jeder Art
min_species <- aggregate(body_mass_g ~ species, data = penguins_tidy, FUN = min)
min_species

# Schwerste männliche und weibliche Pinguine
max_sex <- aggregate(body_mass_g ~ sex, data = penguins_tidy, FUN = max)
max_sex

# Welche Spezies? 
penguins_tidy$species[which(penguins_tidy$body_mass_g[penguins_tidy$sex == "female"] == max_sex$body_mass_g[1])]
penguins_tidy$species[which(penguins_tidy$body_mass_g[penguins_tidy$sex == "male"] == max_sex$body_mass_g[2])]


#------------------------------------------------------------------------------#
#### 4 - Bonus ####

# aggregate() im for-Loop
# In diesen Variablen soll nach dem Loop das Ergebnis stehen
max_male <- 0
max_female <- 0

# Wir gehen im Loop jede einzelne Beobachtung durch
for (row in 1:nrow(penguins_tidy)) {
  # speichern der relevanten Werte aus der aktuellen Reihe ("tmp" = temporary)
  body_mass_tmp <- penguins_tidy$body_mass_g[row]
  sex_tmp <- penguins_tidy$sex[row]
  # Wenn die aktuelle Beobachtung von einem weiblichen Pinguin stammt und größer ist, als
  # unser bisheriges Maximum, setzen wir unser Maximum auf den aktuellen Wert
  if (sex_tmp == "female" & body_mass_tmp > max_female) {
    max_female <- body_mass_tmp
    # das gleiche für die männlichen Pinguine
  } else if (sex_tmp == "male" & body_mass_tmp > max_male) {
    max_male <- body_mass_tmp
  }
}
max_male
max_female


# Welche Variante ist schneller?

# Startzeit erfassen
start_for_loop <- Sys.time()
# Wir führen den Code 10000 mal aus
for (i in 1:10000) {
  for (row in 1:nrow(penguins_tidy)) {
    body_mass_tmp <- penguins_tidy$body_mass_g[row]
    sex_tmp <- penguins_tidy$sex[row]
    if (sex_tmp == "female" & body_mass_tmp > max_female) {
      max_female <- body_mass_tmp
    } else if (sex_tmp == "male" & body_mass_tmp > max_male) {
      max_male <- body_mass_tmp
    }
  }
}
# Endzeit erfassen
end_for_loop <- Sys.time()
time_taken_for_loop <- end_for_loop - start_for_loop
time_taken_for_loop

# Das gleiche für die aggregate() Funktion
start_aggregate <- Sys.time()
for (i in 1:10000) {
  max_sex <- aggregate(body_mass_g ~ sex, data = penguins_tidy, FUN = max)
}
end_aggregate <- Sys.time()
time_taken_aggregate <- end_aggregate - start_aggregate
time_taken_aggregate



