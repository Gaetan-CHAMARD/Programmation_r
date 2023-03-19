# TP 5 NBA

# Exercice 1

# a)
nba <- read.csv("NBA.csv", header = TRUE, sep = ",")

# b)

nb_ligne <- nrow(nba) 
cat("Le nombre de ligne est :", nb_ligne)

nb_col <- ncol(nba) 
cat("Le nombre de colonnes est :", nb_col)

# c)

resume_nba <- summary(nba)
print(resume_nba)

# d) 

nba$PERIOD <- as.factor(nba$PERIOD)
nba$PTS_TYPE <- as.factor(nba$PTS_TYPE)
# Vérifié
resume_nba <- summary(nba)
print(resume_nba)

# Exercice 2 

# a)

quantile(nba$CLOSE_DEF_DIST, probs = seq(0, 1, 0.1))

# b)

nb_match <- length(unique(nba$GAME_ID))
cat("Le nombre de match est:", nb_match )

# c) 

nb_tireur <- length(unique(nba$SHOOTER))
cat("Le nombre de shooteur est:", nb_tireur )

# d) 

nba$SHOT_DIST_METRE <- nba$SHOT_DIST * 0.30
print(SHOT_DIST_METRE)

# Exercice 3 

# a) 

barplot(table(nba$PERIOD), xlab = "Période", ylab = "Nombre de tirs", main = "Répartition des tirs par période")

# b) 
tirs <- prop.table(table(nba$SHOT_RESULT),)
pie(tirs, labels = paste(rownames(tirs), tirs))

# c)

hist(table(nba$CLOSE_DEF_DIST), xlab = "Distance de tirs", ylab = " Nombre de tirs", main = "Répartition des distance de tirs")

# d) 

prop_tirs <- with(nba, table(PERIOD, PTS_TYPE))
prop_tirs <- prop.table(prop_tirs, margin = 1)

barplot(prop_tirs, beside = TRUE,
        ylim = c(0, 1),
        xlab = "Période", ylab = "Pourcentage",
        main = "Répartition en pourcentage de tirs à 2 et 3 points pour chaque période")

# e) 

boxplot(nba$SHOT_DIST_METRE, main = "Distribution des distances de tirs", ylab = "Distance de tir (mètres)")

# f) 

boxplot(nba$SHOT_DIST_METRE ~ nba$PERIOD, main = "Distribution des distances de tirs par période", xlab = "Période", ylab = "Distance de tir (mètres)")

# Exercice 4 

# a)

dist_tirs <- aggregate(nba$SHOT_DIST, by = list(nba$SHOOTER), mean)
top_dist <- head(dist_tirs[order(dist_tirs$x, decreasing = TRUE),], 10)
names(top_dist) <- c("SHOOTER", "DISTANCE")
top_dist

# b)

nba$PTS_TYPE <- as.numeric(nba$PTS_TYPE)
pts_plus <- aggregate(nba$PTS_TYPE, by = list(nba$SHOOTER), sum)
top_pts <- head(pts_plus[order(pts_plus$x, decreasing = TRUE),], 5)
names(top_pts) <- c("SHOOTER", "POINTS")
top_pts

# c) 

pts_period <- aggregate(nba$PTS_TYPE, by = list(nba$PERIOD), sum)
names(pts_period) <- c("PERIOD", "POINTS")
pts_period