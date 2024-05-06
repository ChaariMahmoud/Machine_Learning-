# Charger la bibliothèque pour la manipulation des données
library(readr)

# Spécifier le chemin complet vers votre fichier CSV
chemin_fichier <- "E:/services_annual_dataset.csv"

# Lire le fichier CSV en utilisant la fonction read_csv de la bibliothèque readr
df <- read_csv(chemin_fichier, locale = locale(encoding = "latin1"))

# Afficher les premières lignes du DataFrame pour vérifier la lecture
print(head(df))

# Identifier la variable endogène Y
variable_endogène <- 'Value'

# Variables potentiellement explicatives X
variables_explicatives <- c('ReporterCode', 'ReporterISO3A', 'PartnerCode', 'PartnerISO3A',
                             'IndicatorCategory', 'Indicator', 'Year', 'ProductClassificationCode')

# Formuler des hypothèses sur les relations statistiques
hypothèses_Y <- c(
  "Les montants monétaires des échanges commerciaux (Y) augmentent avec le temps (Year).",
  "Les montants monétaires des échanges commerciaux (Y) varient en fonction des pays impliqués (Reporter, Partner) et des catégories/indicateurs commerciaux (IndicatorCategory, Indicator)."
)

hypothèses_X <- c(
  "Il existe une corrélation entre les pays rapporteurs et les pays partenaires (ReporterCode vs PartnerCode).",
  "Certains indicateurs commerciaux peuvent avoir des corrélations spécifiques avec les pays rapporteurs ou les pays partenaires."
)

# Afficher les hypothèses
cat("\nHypothèses sur les relations avec la variable endogène Y :\n")
for (hypothèse in hypothèses_Y) {
  cat(paste("- ", hypothèse, "\n"))
}

cat("\nHypothèses sur les relations entre les variables explicatives X :\n")
for (hypothèse in hypothèses_X) {
  cat(paste("- ", hypothèse, "\n"))
}

# Installer le package reshape2 s'il n'est pas déjà installé
if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2")
}

# Charger le package reshape2 pour la manipulation des données
library(reshape2)

# Sélectionner les variables quantitatives pour le boxplot
quantitative_vars <- df[, sapply(df, is.numeric)]

# Convertir le data frame en format long pour ggplot
df_long <- melt(quantitative_vars)

# Afficher un boxplot pour chaque variable quantitative
print(ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot for Quantitative Variables", x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip())

# Sélectionner les variables catégoriques pour les diagrammes à barres
categorical_vars <- df %>% select_if(is.factor)

# Fonction pour mapper les labels aux numéros
map_labels <- function(col) {
  value_counts <- table(col)
  mapping_dict <- setNames(as.integer(col), col)
  df_mapped <- data.frame(Label = names(mapping_dict), Num = unname(mapping_dict))
  return(df_mapped)
}

# Afficher un diagramme à barres pour chaque variable catégorique
for (col in names(categorical_vars)) {
  df_mapped <- map_labels(categorical_vars[[col]])
  
  print(ggplot(df_mapped, aes(x = factor(Num), fill = Label)) +
    geom_bar() +
    labs(title = paste("Bar Chart for", col),
         x = "Num (Mapped Label)\n\nMapping:",
         y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_discrete(name = "Label"))
}

# Vérifier si les packages sont installés, sinon les installer
required_packages <- c("ggplot2", "corrplot")

# Installer les packages manquants
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Charger les bibliothèques nécessaires
library(ggplot2)
library(corrplot)

# Sélectionner les variables numériques pertinentes pour l'analyse de corrélation
numerical_vars <- c('Year', 'Value')

# Calculer la matrice de corrélation
correlation_matrix <- cor(df[numerical_vars])

# Afficher la matrice de corrélation sous forme de heatmap avec ggplot2
ggplot(data = melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  labs(title = "Correlation Matrix Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed()

# Personnaliser l'affichage de la matrice de corrélation avec corrplot
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7)

# Sélectionner les variables d'intérêt X et Y
X <- df$Year
Y <- df$Value

# Calculer la corrélation entre X et Y
correlation <- cor(X, Y)
print(paste("Correlation between X and Y:", correlation))

# Installer les packages nécessaires s'ils ne sont pas déjà installés
required_packages <- c("ggplot2", "dplyr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)

# Charger les données
# Votre code pour charger les données ici

# Sélectionner les variables d'intérêt X et Y
# Votre code pour sélectionner les variables X et Y ici

# Effectuer une régression linéaire
model <- lm(Y ~ X)

# Obtenir les informations sur la régression
summary_model <- summary(model)

# Extraire les valeurs pertinentes
slope <- coef(model)[2]  # Coefficient de la variable X
intercept <- coef(model)[1]  # Terme constant
r_squared <- summary_model$r.squared  # R au carré
p_value <- summary_model$coefficients[2, 4]  # p-valeur de la variable X

# Afficher les résultats
cat(paste("Slope:", slope, ", Intercept:", intercept, "\n"))
cat(paste("R-squared value:", r_squared, ", p-value:", p_value, "\n"))

# Charger les bibliothèques nécessaires
library(ggplot2)

# Utiliser les données
X <- df$Year
Y <- df$Value

# Utiliser les valeurs de pente (slope) et d'interception (intercept)
slope <- -211.21950052361478
intercept <- 428206.1963112778
predictions <- slope * X + intercept

# Nuage de points avec la droite de régression
scatter_data <- data.frame(X = X, Y = Y, predictions = predictions)
ggplot(data = scatter_data, aes(x = X, y = Y)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predictions), color = 'red') +
  labs(x = 'Year', y = 'Value', title = 'Régression linéaire') +
  theme_minimal()

# Graphique des résidus
residuals <- Y - predictions
residual_data <- data.frame(predictions = predictions, residuals = residuals)
ggplot(data = residual_data, aes(x = predictions, y = residuals)) +
  geom_point(color = 'green') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
  labs(x = 'Prédictions', y = 'Résidus', title = 'Graphique des résidus') +
  theme_minimal()
# Installer les packages nécessaires s'ils ne sont pas déjà installés
required_packages <- c("ggplot2", "psych")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Charger les bibliothèques nécessaires
library(ggplot2)
library(psych)  # Pour la fonction principal() utilisée par PCA

# Charger les données depuis votre fichier CSV (à adapter selon votre structure de données)
# Exemple fictif de chargement des données pour illustrer le code
df <- read.csv('chemin_vers_votre_fichier.csv')

# Sélectionner les variables pertinentes pour l'analyse PCA
quantitative_vars <- df[, c('Year', 'Value')]

# Normalisation des données
scaled_data <- scale(quantitative_vars)

# PCA
pca_result <- principal(scaled_data, nfactors = 2, rotate = "none")

# Récupérer les composantes principales
pca_data <- pca_result$scores[, 1:2]

# Créer un DataFrame pour les résultats de PCA
pca_df <- data.frame(PC1 = pca_data[, 1], PC2 = pca_data[, 2])

# Analyser les résultats de PCA
explained_variance_ratio <- pca_result$values / sum(pca_result$values)
cat("Explained Variance Ratio:", explained_variance_ratio, "\n")

# Visualiser les résultats de PCA
ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(x = 'Principal Component 1', y = 'Principal Component 2', title = 'PCA') +
  theme_minimal()
