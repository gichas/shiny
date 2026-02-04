# server.R

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(plotly)

# --- fonctions de simulation ---


#' Simulation Monte Carlo pour distributions prédéfinies ou personnalisées
#' 
#' @param n Nombre de simulations
#' @param dist Type de distribution ("norm", "unif", "exp", "custom")
#' @param params Liste des paramètres pour les distributions prédéfinies
#' @param custom_method Méthode pour loi personnalisée ("density", "quantile", "rejection")
#' @param custom_formula Formule mathématique pour la loi personnalisée
#' @param x_min Borne inférieure du support (pour densité personnalisée)
#' @param x_max Borne supérieure du support (pour densité personnalisée)
#' 
#' @return Vecteur numérique de n valeurs simulées
monte_carlo <- function(n, dist, params = NULL, 
                        custom_method = NULL, 
                        custom_formula = NULL,
                        x_min = NULL, x_max = NULL) {
  
  # Distributions prédéfinies
  if (dist != "custom") {
    dist_func <- switch(dist,
                        "norm" = rnorm,
                        "unif" = runif,
                        "exp"  = rexp,
                        stop("Distribution inconnue"))
    return(do.call(dist_func, c(list(n = n), params)))
  }
  
  # Loi personnalisée - Méthode par rejet (Rejection Sampling)
  if (custom_method == "density") {
    return(monte_carlo_rejection(n, custom_formula, x_min, x_max))
  }
  
  # Loi personnalisée - Fonction quantile inverse
  if (custom_method == "quantile") {
    u <- runif(n)
    return(sapply(u, function(p) {
      tryCatch({
        eval(parse(text = custom_formula), 
             envir = list(u = p, pi = pi, e = exp(1)))
      }, error = function(e) NA)
    }))
  }
  
  stop("Méthode de génération invalide")
}


#' Rejection Sampling pour densité personnalisée
#' 
#' @param n Nombre d'échantillons
#' @param formula_text Expression de la densité f(x)
#' @param x_min Borne inférieure
#' @param x_max Borne supérieure
monte_carlo_rejection <- function(n, formula_text, x_min, x_max) {
  
  # Fonction de densité brute
  f_raw <- function(x) {
    tryCatch({
      result <- eval(parse(text = formula_text), 
                     envir = list(x = x, pi = pi, e = exp(1)))
      if (is.na(result) || !is.finite(result)) return(0)
      return(max(0, result))  # Densité toujours positive
    }, error = function(e) 0)
  }
  
  # Évaluer la densité sur une grille
  x_test <- seq(x_min, x_max, length.out = 1000)
  pdf_vals <- sapply(x_test, f_raw)
  
  # Vérifier que la densité n'est pas nulle partout
  if (all(pdf_vals == 0) || !any(is.finite(pdf_vals))) {
    stop("La densité est nulle ou invalide partout sur [x_min, x_max]")
  }
  
  # Normalisation : calculer l'intégrale approximative
  dx <- (x_max - x_min) / 1000
  integrale <- sum(pdf_vals * dx, na.rm = TRUE)
  
  if (integrale <= 0 || !is.finite(integrale)) {
    stop("Impossible de normaliser la densité (intégrale nulle ou infinie)")
  }
  
  # Fonction de densité normalisée
  f <- function(x) f_raw(x) / integrale
  
  # Maximum de la densité normalisée
  M <- max(pdf_vals / integrale, na.rm = TRUE) * 1.1  # Marge 10%
  
  # Rejection sampling
  samples <- numeric(n)
  accepted <- 0
  attempts <- 0
  max_attempts <- min(n * 100, 1000000)  # Max 100 essais par échantillon OU 1M total
  
  while(accepted < n && attempts < max_attempts) {
    # Proposer un x uniforme
    x_prop <- runif(1, x_min, x_max)
    u <- runif(1, 0, M)
    
    # Accepter si u < f(x)
    if (u <= f(x_prop)) {
      accepted <- accepted + 1
      samples[accepted] <- x_prop
    }
    
    attempts <- attempts + 1
  }
  
  # Si pas assez d'échantillons générés
  if (accepted < n) {
    taux <- round(100 * accepted / attempts, 2)
    stop(paste0(
      "Rejection sampling échoué : seulement ", accepted, " échantillons générés sur ", n, 
      " demandés (taux d'acceptation : ", taux, "%). ",
      "La densité est probablement mal définie ou trop complexe."
    ))
  }
  
  return(samples)
}


# --- Statistiques descriptives ---

#' calcul des statistiques descriptives
#' 
#' @param x Vecteur numérique
#' @return data.frame avec les statistiques
stat_desc <- function(x) {
  x <- x[is.finite(x)]  # Retirer les NA et Inf
  
  data.frame(
    Statistique = c("Nombre", "Moyenne", "Médiane", "Écart-type", 
                    "Minimum", "Maximum", "Q1", "Q3", "IQR"),
    Valeur = c(
      length(x),
      mean(x),
      median(x),
      sd(x),
      min(x),
      max(x),
      quantile(x, 0.25),
      quantile(x, 0.75),
      IQR(x)
    ),
    stringsAsFactors = FALSE
  )
}


#' Test de normalité (Shapiro-Wilk)
#' 
#' @param x Vecteur numérique
#' @return Liste avec résultats du test
test_normalite <- function(x) {
  x <- x[is.finite(x)]
  
  if (length(x) < 3 || length(x) > 5000) {
    return(list(
      statistic = NA,
      p_value = NA,
      conclusion = "Échantillon trop petit ou trop grand pour le test"
    ))
  }
  
  test <- shapiro.test(x)
  
  list(
    statistic = test$statistic,
    p_value = test$p.value,
    conclusion = ifelse(test$p.value > 0.05, 
                        "✓ Hypothèse de normalité non rejetée (p > 0.05)",
                        "✗ Hypothèse de normalité rejetée (p < 0.05)")
  )
}


#' Test de comparaison entre deux échantillons
#' 
#' @param x1 Premier échantillon
#' @param x2 Deuxième échantillon
#' @return Liste avec résultats du test
test_comparaison <- function(x1, x2) {
  x1 <- x1[is.finite(x1)]
  x2 <- x2[is.finite(x2)]
  
  # Test de Student
  t_test <- t.test(x1, x2)
  
  # Test de Wilcoxon (non-paramétrique)
  w_test <- wilcox.test(x1, x2)
  
  # Test F de variance
  var_test <- var.test(x1, x2)
  
  list(
    t_test = list(
      statistic = t_test$statistic,
      p_value = t_test$p.value,
      conclusion = ifelse(t_test$p.value > 0.05,
                          "✓ Pas de différence significative de moyenne (p > 0.05)",
                          "✗ Différence significative de moyenne (p < 0.05)")
    ),
    wilcox_test = list(
      statistic = w_test$statistic,
      p_value = w_test$p.value,
      conclusion = ifelse(w_test$p.value > 0.05,
                          "✓ Pas de différence significative (test non-param, p > 0.05)",
                          "✗ Différence significative (test non-param, p < 0.05)")
    ),
    var_test = list(
      statistic = var_test$statistic,
      p_value = var_test$p.value,
      conclusion = ifelse(var_test$p.value > 0.05,
                          "✓ Variances homogènes (p > 0.05)",
                          "✗ Variances hétérogènes (p < 0.05)")
    )
  )
}


# --- Latex ---

#' Convertir formule R vers LaTeX (simplifié)
#' 
#' @param formula_text Expression R
#' @return Expression LaTeX
r_to_latex <- function(formula_text) {
  latex <- formula_text
  
  # Conversions de base
  latex <- gsub("\\*", " \\\\cdot ", latex)
  latex <- gsub("sqrt\\(([^)]+)\\)", "\\\\sqrt{\\1}", latex)
  latex <- gsub("exp\\(([^)]+)\\)", "e^{\\1}", latex)
  latex <- gsub("\\^\\(([^)]+)\\)", "^{\\1}", latex)
  latex <- gsub("pi", "\\\\pi", latex)
  
  return(latex)
}