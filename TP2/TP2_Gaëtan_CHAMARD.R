# TP 2
# Exercice 1

# a) Importer le jeu de données 

library(readxl)
pokemon <- read_excel("pokemon.xlsx", sheet = 2)

# b) Connaitre le nombre de ligne / colonnes

nlignes <- nrow(pokemon)
ncolonnes <- ncol(pokemon)
cat("Le nombre de  lignes est :", nlignes, "\n Le nombre de colonnees est :", ncolonnes)

# c) Afficher le noms des colonnes

colnoms <- colnames(pokemon)
print(colnoms)

# d) Afficher le type des colonnes 

types <- sapply(pokemon, class)
print(types)

# e) Transformer les variables generations

pokemon$generation <- as.factor(pokemon$generation)
pokemon$is_legendary <- as.factor(pokemon$is_legendary)
pokemon$type <- as.factor(pokemon$type)

# f) Pour connaitre le nombre de niveaux

nlevels_gen <- nlevels(pokemon$generation)
nlevels_legendary <- nlevels(pokemon$is_legendary)
nlevels_type <- nlevels(pokemon$type)
cat("Le nombre de niveaux pour generation est :", nlevels_gen, "\nLe nombre de niveaux pour is_legendary est :", nlevels_legendary, "\nLe nombre de niveau pour type est :", nlevels_type)

# g) Afficher un résumer des données 

résumé_pokemon <- summary(pokemon)
print(résumé_pokemon)

# Exercice 2 

# a) Determiner la moyenne de la variable weight_kg

moyenne_weight_kg <- mean(pokemon$weight_kg, na.rm = TRUE)
cat("La moyenne de la variable weight_kg est :", moyenne_weight_kg)

# b) Determiner la médiane de la variable weight_kg

median_weight_kg <- median(pokemon$weight_kg, na.rm = TRUE)
cat("La médiane de la variable weight_kg est :", median_weight_kg)

# c) Determiner les quartiles de la variable height_m

quartiles_height_m <- quantile(pokemon$height_m, na.rm = TRUE)
print(quartiles_height_m)

# d) Determiner les déciles de la variable height_m 

deciles_height_m <- quantile(pokemon$height_m, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
cat("Les déciles de la variable height_m sont :", deciles_height_m)

# e) Determiner la variance et l'écart type de la variable weight_kg

var_weight_kg <- var(pokemon$weight_kg, na.rm = TRUE)
sd_weight_kg <- sqrt(var_weight_kg)
cat("La variance de la variable weight_kg est :", var_weight_kg,"\nL'écart-type de la variable weight_kg est :", sd_weight_kg)

# f) Determiner un tri à plat pour compter les effectifs des modalités de chaque variable

type_table <- table(pokemon$type)
gen_table <- table(pokemon$generation)
legendary_table <- table(pokemon$is_legendary)

type_table_decroi <- sort(type_table, decreasing = TRUE)
gen_table_decroi <- sort(gen_table, decreasing = TRUE)
legendary_table_decroi <- sort(legendary_table, decreasing = TRUE)

cat("Table des effectifs des modalités de la variable generation :\n")
print(gen_table_decroi)

cat("\nTable des effectifs des modalités de la variable is_legendary :\n")
print(legendary_table_decroi)

cat("\nTable des effectifs des modalités de la variable type :\n")
print(type_table_decroi)

# Exercice 3 

# a) Sélectionnez la colonne nom et is_legendary.

colonne_a <- pokemon[,c("nom","is_legendary")]
dim(colonne_a)

# b) Sélectionnez les 50 premières lignes et les deux premières colonnes.

premieres_lignes <- pokemon[1:50,1:2]
dim(premieres_lignes)

# c) Sélectionnez les 10 premières lignes et toutes les colonnes.

lignes_et_colonnes <- pokemon[1:10,]
dim(lignes_et_colonnes)

# d) Sélectionnez toutes les colonnes sauf la dernière.

colonnes <- pokemon[, -ncol(pokemon)]
dim(colonnes)

# e) Triez le dataset par ordre alphabétique et afficher le nom du pokemon de la première ligne.

data_alphabet <- pokemon[order(pokemon$nom),]
data_alphabet[1, "nom"]

# f) Triez le dataset par weight_kg en ordre décroissant, et afficher le nom du pokemon de la première ligne.
weight_décroissant <- pokemon[order(pokemon$weight_kg, decreasing = TRUE),]
weight_décroissant[1, "nom"]

# g) Triez le dataset par attack en ordre décroissant puis par speed en ordre croissant, et afficher le nom des pokemons des 10 premières lignes.

attack_and_speed <- pokemon[order(-pokemon$attack, pokemon$speed),]
attack_and_speed[1:10, "nom"]

# Exercice 4 

#a. Filtrez sur les pokemons qui ont 150 ou plus d'attack puis trier le résultat par ordre décroissant d'attack.

pokemon_attack <- pokemon[pokemon$attack >= 150, c("nom", "attack")]
pokemon_attack <- pokemon_attack[order(pokemon_attack$attack, decreasing = TRUE),]
dim(pokemon_attack)

#b. Filtrez sur les pokemons de type dragon,ghost,psychic et dark

type_pokemon <- pokemon[pokemon$type %in% c("dragon", "ghost", "psychic", "dark"), c("nom", "type")]
dim(type_pokemon)

#c. Filtrez sur les pokemons de type fire avec plus de 100 d'attack, puis trier le résultat par ordre décroissant d'attack.

type_fire <- pokemon[pokemon$type == "fire" & pokemon$attack > 100, c("nom", "attack", "type")]
type_fire <- type_fire[order(type_fire$attack, decreasing = TRUE),]
dim(type_fire)

#d. Filtrez sur les pokemons qui ont entre 100 et 150 de speed. Les trier par speed décroissant.

speed_décroissant <- pokemon[pokemon$speed >= 100 & pokemon$speed <= 150, c("nom", "speed")]
speed_décroissant <- speed_décroissant[order(speed_décroissant$speed, decreasing = TRUE),]
dim(speed_décroissant)

#e. Filtrez sur les pokémons qui ont des valeurs manquantes sur la variable height_m.

weight_manquant <- pokemon[is.na(pokemon$height_m), c("nom", "height_m")]
dim(weight_manquant)

#f. Filtrez sur les pokemons qui ont des valeurs renseignées à la fois pour la variable weight_kg et la variable height.

weight_height <- pokemon[!is.na(pokemon$weight_kg) & !is.na(pokemon$height_m), c("nom", "weight_kg", "height_m")]
dim(weight_height)

#g. Filtrez sur les pokemons pesant plus de 250 kg et affichez le résultat pour vérifier.

kilos_pokemon <- pokemon[pokemon$weight_kg > 250, c("nom", "weight_kg")]
print(kilos_pokemon )