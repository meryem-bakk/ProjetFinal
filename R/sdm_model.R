#' Train Species Distribution Model
#'
#' Entraîne un modèle Random Forest pour prédire
#' la présence des espèces fourragères.
#'
#' @param data Dataframe contenant les variables environnementales
#' et la colonne presence
#'
#' @return Modèle Random Forest entraîné
#'
#' @details
#' Le modèle utilise :
#' - NDVI
#' - température
#' - précipitations
#' pour prédire la présence des espèces.
#'
#' @examples
#' data <- data.frame(
#'   presence = c(1,0,1,0,1),
#'   ndvi = c(0.7,0.2,0.8,0.1,0.6),
#'   temperature = c(18,35,20,38,19),
#'   rainfall = c(400,90,350,70,420)
#' )
#'
#' model <- train_sdm_model(data)
#'
#' @export

train_sdm_model <- function(data){

  model <- randomForest::randomForest(

    as.factor(presence) ~ ndvi + temperature + rainfall,

    data = data

  )

  return(model)

}
