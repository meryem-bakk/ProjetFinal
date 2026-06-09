#' Evaluate SDM model performance
#'
#' Evalue les performances du modele de
#' distribution des especes fourrageres.
#'
#' @param observed Valeurs observees binaires
#' (0 = absence, 1 = presence).
#'
#' @param predicted Valeurs predites par le modele
#' (probabilites comprises entre 0 et 1).
#'
#' @param show_plots Afficher ou non les graphiques.
#' Par defaut FALSE.
#'
#' @return Liste contenant :
#' - performance : tableau AUC, Accuracy, RMSE
#' - roc : objet ROC (pROC)
#'
#' @details
#' Cette fonction calcule :
#'
#' - AUC pour les modeles de distribution,
#' - Accuracy pour presence/absence,
#' - RMSE pour estimation continue.
#'
#' Elle peut egalement produire :
#'
#' - ROC curve,
#' - graphique observed vs predicted.
#'
#' Les vecteurs observed et predicted doivent
#' avoir la meme longueur. observed doit etre
#' binaire (0/1) pour le calcul de l'AUC.
#'
#' @references
#' Fielding, A.H. & Bell, J.F. (1997).
#' A review of methods for the assessment
#' of prediction errors in conservation
#' presence/absence models.
#'
#' @examples
#' observed  <- c(1, 0, 1, 1, 0)
#' predicted <- c(0.8, 0.2, 0.7, 0.9, 0.1)
#'
#' result <- evaluate_model(
#'   observed,
#'   predicted,
#'   show_plots = FALSE
#' )
#'
#' result$performance
#'
#' @export

evaluate_model <- function(

  observed,
  predicted,
  show_plots = FALSE

){

  # ==========================
  # Verifications
  # ==========================

  if(length(observed) != length(predicted)){
    stop("observed et predicted doivent avoir la meme longueur.")
  }

  unique_vals <- unique(stats::na.omit(observed))

  if(!all(unique_vals %in% c(0, 1))){
    stop(
      "observed doit etre binaire (0/1) pour le calcul de l'AUC. ",
      "Valeurs detectees : ", paste(unique_vals, collapse = ", ")
    )
  }

  if(any(predicted < 0 | predicted > 1, na.rm = TRUE)){
    warning(
      "Certaines valeurs de predicted sont hors de [0, 1]. ",
      "Verifier que predicted contient des probabilites."
    )
  }

  # ==========================
  # Accuracy
  # ==========================

  predicted_class <- ifelse(predicted >= 0.5, 1, 0)

  accuracy <- mean(predicted_class == observed, na.rm = TRUE)

  # ==========================
  # RMSE
  # ==========================

  rmse <- sqrt(
    mean((observed - predicted)^2, na.rm = TRUE)
  )

  # ==========================
  # AUC
  # ==========================

  roc_obj <- pROC::roc(
    response  = observed,
    predictor = predicted,
    quiet     = TRUE
  )

  auc <- as.numeric(pROC::auc(roc_obj))

  # ==========================
  # Visualisations (optionnel)
  # ==========================

  if(show_plots){

    plot(roc_obj, main = "ROC Curve")

    plot(
      observed,
      predicted,
      main = "Observed vs Predicted",
      xlab = "Observed",
      ylab = "Predicted"
    )

  }

  # ==========================
  # Tableau performances
  # ==========================

  performance <- data.frame(
    AUC      = auc,
    Accuracy = accuracy,
    RMSE     = rmse
  )

  return(

    list(
      performance = performance,
      roc         = roc_obj
    )

  )

}
