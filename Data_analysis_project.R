# Charger la biblioth�que pour la manipulation des donn�es
library(readr)

# Sp�cifier le chemin complet vers votre fichier CSV
chemin_fichier <- "E:/services_annual_dataset.csv"

# Lire le fichier CSV en utilisant la fonction read_csv de la biblioth�que readr
df <- read_csv(chemin_fichier, locale = locale(encoding = "latin1"))

# Afficher les premi�res lignes du DataFrame pour v�rifier la lecture
print(head(df))

# Identifier la variable endog�ne Y
variable_endog�ne <- 'Value'

# Variables potentiellement explicatives X
variables_explicatives <- c('ReporterCode', 'ReporterISO3A', 'PartnerCode', 'PartnerISO3A',
                             'IndicatorCategory', 'Indicator', 'Year', 'ProductClassificationCode')

# Formuler des hypoth�ses sur les relations statistiques
hypoth�ses_Y <- c(
  "Les montants mon�taires des �changes commerciaux (Y) augmentent avec le temps (Year).",
  "Les montants mon�taires des �changes commerciaux (Y) varient en fonction des pays impliqu�s (Reporter, Partner) et des cat�gories/indicateurs commerciaux (IndicatorCategory, Indicator)."
)

hypoth�ses_X <- c(
  "Il existe une corr�lation entre les pays rapporteurs et les pays partenaires (ReporterCode vs PartnerCode).",
  "Certains indicateurs commerciaux peuvent avoir des corr�lations sp�cifiques avec les pays rapporteurs ou les pays partenaires."
)

# Afficher les hypoth�ses
cat("\nHypoth�ses sur les relations avec la variable endog�ne Y :\n")
for (hypoth�se in hypoth�ses_Y) {
  cat(paste("- ", hypoth�se, "\n"))
}

cat("\nHypoth�ses sur les relations entre les variables explicatives X :\n")
for (hypoth�se in hypoth�ses_X) {
  cat(paste("- ", hypoth�se, "\n"))
}

# Installer le package reshape2 s'il n'est pas d�j� install�
if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2")
}

# Charger le package reshape2 pour la manipulation des donn�es
library(reshape2)

# S�lectionner les variables quantitatives pour le boxplot
quantitative_vars <- df[, sapply(df, is.numeric)]

# Convertir le data frame en format long pour ggplot
df_long <- melt(quantitative_vars)

# Afficher un boxplot pour chaque variable quantitative
print(ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot for Quantitative Variables", x = "Variable", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip())

# S�lectionner les variables cat�goriques pour les diagrammes � barres
categorical_vars <- df %>% select_if(is.factor)

# Fonction pour mapper les labels aux num�ros
map_labels <- function(col) {
  value_counts <- table(col)
  mapping_dict <- setNames(as.integer(col), col)
  df_mapped <- data.frame(Label = names(mapping_dict), Num = unname(mapping_dict))
  return(df_mapped)
}

# Afficher un diagramme � barres pour chaque variable cat�gorique
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

# V�rifier si les packages sont install�s, sinon les installer
required_packages <- c("ggplot2", "corrplot")

# Installer les packages manquants
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Charger les biblioth�ques n�cessaires
library(ggplot2)
library(corrplot)

# S�lectionner les variables num�riques pertinentes pour l'analyse de corr�lation
numerical_vars <- c('Year', 'Value')

# Calculer la matrice de corr�lation
correlation_matrix <- cor(df[numerical_vars])

# Afficher la matrice de corr�lation sous forme de heatmap avec ggplot2
ggplot(data = melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  labs(title = "Correlation Matrix Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed()

# Personnaliser l'affichage de la matrice de corr�lation avec corrplot
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7)

# S�lectionner les variables d'int�r�t X et Y
X <- df$Year
Y <- df$Value

# Calculer la corr�lation entre X et Y
correlation <- cor(X, Y)
print(paste("Correlation between X and Y:", correlation))

# Installer les packages n�cessaires s'ils ne sont pas d�j� install�s
required_packages <- c("ggplot2", "dplyr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Charger les biblioth�ques n�cessaires
library(ggplot2)
library(dplyr)

# Charger les donn�es
# Votre code pour charger les donn�es ici

# S�lectionner les variables d'int�r�t X et Y
# Votre code pour s�lectionner les variables X et Y ici

# Effectuer une r�gression lin�aire
model <- lm(Y ~ X)

# Obtenir les informations sur la r�gression
summary_model <- summary(model)

# Extraire les valeurs pertinentes
slope <- coef(model)[2]  # Coefficient de la variable X
intercept <- coef(model)[1]  # Terme constant
r_squared <- summary_model$r.squared  # R au carr�
p_value <- summary_model$coefficients[2, 4]  # p-valeur de la variable X

# Afficher les r�sultats
cat(paste("Slope:", slope, ", Intercept:", intercept, "\n"))
cat(paste("R-squared value:", r_squared, ", p-value:", p_value, "\n"))

# Charger les biblioth�ques n�cessaires
library(ggplot2)

# Utiliser les donn�es
X <- df$Year
Y <- df$Value

# Utiliser les valeurs de pente (slope) et d'interception (intercept)
slope <- -211.21950052361478
intercept <- 428206.1963112778
predictions <- slope * X + intercept

# Nuage de points avec la droite de r�gression
scatter_data <- data.frame(X = X, Y = Y, predictions = predictions)
ggplot(data = scatter_data, aes(x = X, y = Y)) +
  geom_point(color = 'blue') +
  geom_line(aes(y = predictions), color = 'red') +
  labs(x = 'Year', y = 'Value', title = 'R�gression lin�aire') +
  theme_minimal()

# Graphique des r�sidus
residuals <- Y - predictions
residual_data <- data.frame(predictions = predictions, residuals = residuals)
ggplot(data = residual_data, aes(x = predictions, y = residuals)) +
  geom_point(color = 'green') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
  labs(x = 'Pr�dictions', y = 'R�sidus', title = 'Graphique des r�sidus') +
  theme_minimal()
# Installer les packages n�cessaires s'ils ne sont pas d�j� install�s
required_packages <- c("ggplot2", "psych")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Charger les biblioth�ques n�cessaires
library(ggplot2)
library(psych)  # Pour la fonction principal() utilis�e par PCA

# Charger les donn�es depuis votre fichier CSV (� adapter selon votre structure de donn�es)
# Exemple fictif de chargement des donn�es pour illustrer le code
df <- read.csv('chemin_vers_votre_fichier.csv')

# S�lectionner les variables pertinentes pour l'analyse PCA
quantitative_vars <- df[, c('Year', 'Value')]

# Normalisation des donn�es
scaled_data <- scale(quantitative_vars)

# PCA
pca_result <- principal(scaled_data, nfactors = 2, rotate = "none")

# R�cup�rer les composantes principales
pca_data <- pca_result$scores[, 1:2]

# Cr�er un DataFrame pour les r�sultats de PCA
pca_df <- data.frame(PC1 = pca_data[, 1], PC2 = pca_data[, 2])

# Analyser les r�sultats de PCA
explained_variance_ratio <- pca_result$values / sum(pca_result$values)
cat("Explained Variance Ratio:", explained_variance_ratio, "\n")

# Visualiser les r�sultats de PCA
ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(x = 'Principal Component 1', y = 'Principal Component 2', title = 'PCA') +
  theme_minimal()
