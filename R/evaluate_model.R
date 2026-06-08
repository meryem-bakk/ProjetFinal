#' Evaluate SDM model performance
#'
#' Evalue les performances du modele de
#' distribution des especes fourrageres.
#'
#' @param observed Valeurs observees
#' (presence/absence ou biomasse mesuree).
#'
#' @param predicted Valeurs predites par le modele.
#'
#' @param show_plots Afficher ou non les graphiques.
#'
#' @return Liste contenant :
#' - tableau des performances
#' - objet ROC
#'
#' @details
#' Cette fonction calcule :
#'
#' - AUC pour les modeles de distribution,
#' - Accuracy pour presence/absence,
#' - RMSE pour estimation biomasse.
#'
#' Elle peut egalement produire :
#'
#' - ROC curve,
#' - graphique observed vs predicted.
#'
#' @references
#' Fielding, A.H. & Bell, J.F. (1997).
#' A review of methods for the assessment
#' of prediction errors in conservation
#' presence/absence models.
#'
#' @examples
#' observed <- c(1,0,1,1,0)
#' predicted <- c(0.8,0.2,0.7,0.9,0.1)
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

  show_plots = TRUE

){

  # ==========================
  # Verification
  # ==========================

  if(length(observed) != length(predicted)){

    stop(
      "Observed and predicted must have the same length"
    )

  }


  # ==========================
  # Accuracy
  # ==========================

  predicted_class <- ifelse(

    predicted >= 0.5,

    1,

    0

  )

  accuracy <- mean(

    predicted_class == observed

  )


  # ==========================
  # RMSE
  # ==========================

  rmse <- sqrt(

    mean(

      (observed - predicted)^2,

      na.rm = TRUE

    )

  )


  # ==========================
  # AUC
  # ==========================

  roc_obj <- pROC::roc(

    observed,

    predicted,

    quiet = TRUE

  )

  auc <- as.numeric(

    pROC::auc(

      roc_obj

    )

  )


  # ==========================
  # Visualisations
  # ==========================

  if(show_plots){

    plot(

      roc_obj,

      main = "ROC Curve"

    )

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

    AUC = auc,

    Accuracy = accuracy,

    RMSE = rmse

  )


  # ==========================
  # Output
  # ==========================

  return(

    list(

      performance = performance,

      roc = roc_obj

    )

  )

}
