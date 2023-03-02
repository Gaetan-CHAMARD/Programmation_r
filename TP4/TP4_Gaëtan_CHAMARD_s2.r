# TP 4

# Exercice 1

# 1) Produire les tailles de la population 
set.seed(1789)
population <- rnorm(n = 10000000, mean = 171, sd = 9)
print(population)

# 2) Moyenne et ecart type de la population 
moyenne_population <- mean(population)
print(moyenne_population)

ecart_type_population <- sd(population)
print(ecart_type_population)

# 3) Histogramme de la taille 

hist(population, breaks = 100, main = "Distribution de la taille des français",
     xlab = "Taille en cm", ylab = "Nombre de personnes")
# On retrouve bien la forme

# 4) Personne faisant plus 190 cm.

population_cherché <- 1-pnorm(q=190, mean= 171, sd=9)
population_190cm <- population_cherché*10000000
print(population_190cm)

# Théoriquement 

population_190cm <- population[population > 190]
length(population_190cm)

# 5) Personne faisant moins de 144 cm

population_cherché <- pnorm(q=144, mean= 171, sd=9)
population_144cm <- population_cherché*10000000
print(population_144cm)

# Théoriquement 

population_144cm <- population[population < 144]
length(population_144cm)

# Exercice 2

# population initiale
pop <- rnorm(10000, 171, 5)

# 1. échantillon de taille 100
ech <- sample(pop, 100)
mean_ech <- mean(ech)
sd_ech <- sd(ech)
mean_pop <- mean(pop)
sd_pop <- sd(pop)

# 2. intervalle de confiance à 95%
se <- sd_ech/sqrt(100)
demi_intervalle <- qt(0.975, 99)*se
IC_inf <- mean_ech - demi_intervalle
IC_sup <- mean_ech + demi_intervalle

# 3. 1000 échantillons de taille 100
echs <- replicate(1000, sample(pop, 100))
means <- apply(echs, 2, mean)
sds <- apply(echs, 2, sd)

# 4. histogramme des moyennes des échantillons
hist(means, breaks = 30)

# 5. moyenne et écart-type des moyennes des échantillons
mean_means <- mean(means)
se_means <- sd_pop/sqrt(100)
sd_means <- sd(means)

# 6. nombre d'échantillons avec moyenne > 172.8cm
nb_sup <- sum(means > 172.8)
nb_theo_sup <- pnorm((172.8 - mean_pop)/se)

# 7. intervalles de confiance de chaque échantillon
demi_intervalle_all <- qt(0.975, 99)*sds/sqrt(100)
IC_inf_all <- means - demi_intervalle_all
IC_sup_all <- means + demi_intervalle_all
df <- data.frame(mean = means, sd = sds, IC_inf = IC_inf_all, IC_sup = IC_sup_all)

# 8. graphique des intervalles de confiance
library(gplots)
plotCI(mean = df$mean, lb = df$IC_inf, ub = df$IC_sup, err = "y", 
       col = "blue", main = "Intervalles de confiance à 95%")
abline(h = mean_pop, col = "red", lwd = 2)
nb_outside <- sum(mean_pop < df$IC_inf | mean_pop > df$IC_sup)
