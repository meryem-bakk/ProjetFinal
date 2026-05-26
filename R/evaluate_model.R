#' Evaluation du modèle SDM
#'
#' Calcule les performances du modèle de distribution
#' des espèces fourragères.
#'
#' @param observed Valeurs observées
#' @param predicted Valeurs prédites
#'
#' @return Dataframe contenant Accuracy et RMSE
#'
#' @details
#' Les métriques calculées sont :
#' - Accuracy
#' - RMSE
#'
#' @references
#' Fielding, A.H. & Bell, J.F. (1997).
#' A review of methods for the assessment
#' of prediction errors in conservation presence/absence models.
#'
#' @examples
#' observed <- c(1,0,1,1,0)
#' predicted <- c(1,0,1,0,0)
#'
#' evaluate_model(observed, predicted)
#'
#' @export

evaluate_model <- function(observed, predicted){

  accuracy <- mean(observed == predicted)

  rmse <- sqrt(mean((observed - predicted)^2))

  results <- data.frame(

    Accuracy = round(accuracy, 3),

    RMSE = round(rmse, 3)

  )

  return(results)

}
