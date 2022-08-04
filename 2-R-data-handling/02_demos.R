##-------------------------------------------------------------
## 02. Data Wrangling - Demos
## Date: August 2022
## Marie Jakob  <marie.jakob@psychologie.uni-freiburg.de>
## ------------------------------------------------------------


#### Folie 4: RStudio ####
# R zeigen
# RStudio zeigen 
# Panes erklären



#### Folie 7: Daten einlesen ####
d_starwars <- read.csv("C:/Users/majakob/Nextcloud/Documents/2022-modeling-summer-school/2-R-data/data/starwars.csv")
d_starwars <- read_csv("data/starwars.csv")



#### Folie 10: RProjects ####


#### Folie 12: Skripte schreiben ####


#### Folie 18: Befehle ausprobieren ####



#### Folie 19 ff. ####

d_starwars$name
d_starwars[1, 2]

d_starwars$is_human <- ifelse(d_starwars$species == "Human", TRUE, FALSE)

non_humans <- d_starwars[d_starwars$species != "Human", ]

names(d_starwars)[names(d_starwars) == "mass"] <- "mass_kg"

d_starwars$gender <- ifelse(d_starwars$gender == "MASCULINE", "masculine", 
                            ifelse(d_starwars$gender == "FEMININE", "feminine", NA))


table(d_starwars$sex, d_starwars$is_human)

aggregate(d_starwars$height ~ d_starwars$species, FUN = mean)
aggregate(d_starwars$height ~ d_starwars$is_human, FUN = mean)

min_height <- aggregate(d_starwars$height ~ d_starwars$species, FUN = min)
max_height <- aggregate(d_starwars$height ~ d_starwars$species, FUN = max)

names(min_height) <- c("species", "min_height")
names(max_height) <- c("species", "max_height")

height_stats <- merge(min_height, max_height, by = "species")

height_stats_bind <- cbind(min_height, max_height$max_height)
height_stats_bind == height_stats


# Reshaping

data_reshaping <- read_csv("data/df_reshaping.csv")

# Welches Format? 

data_long <- pivot_longer(data_reshaping, 
                          cols = c("con", "incon"),
                          values_to = "RT",
                          names_to = "condition")

data_wide <- pivot_wider(data_long,
                         id_cols = "ID",
                         names_from = "condition",
                         values_from = "RT")

data_wide == data_reshaping


#------------------------------------------------------------------------------#
#### Plots ####

# Histogram

# schrittweise einzelne Parameter aufbauen
hist(x = d_starwars$height,
     breaks = 30,
     xlim = c(100, 250),
     ylim = c(0, 12),
     xlab = "Height of Various Star Wars Characters",
     ylab = "Frequency",
     main = "A Basic Histogram")


# Scatterplots
plot(x = d_starwars$height, 
     y = d_starwars$mass_kg,
     type = "p",
     col = "blue",
     pch = 5,
     cex = 1,
     xlab = "Height",
     ylab = "Weight")


# Boxplot

boxplot(d_starwars$height)

boxplot(height ~ is_human, 
        data = d_starwars,
        cex.lab = 2,
        cex.axis = 0.5,
        main = "Star Wars Boxplots",
        cex.main = 1.5,
        notch = F)

# Barplot

barplot(d_starwars$height)
# Nee

# -> Erstmal aggregieren
# Welcher Befehl? 

height_by_species <- aggregate(d_starwars$height ~ d_starwars$species,
                               FUN = mean)
names(height_by_species) <- c("species", "height")

barplot(height_by_species$height,
        names.arg = height_by_species$species,
        las = 2,
        col = "darkblue")





