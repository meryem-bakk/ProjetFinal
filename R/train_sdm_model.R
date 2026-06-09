#' Train Species Distribution Model (SDM)
#'
#' Modelisation des especes fourrageres par Random Forest.
#'
#' @param data Dataframe contenant les variables environnementales
#' et la variable presence.
#'
#' @param target Variable cible (presence/absence). Par defaut "presence".
#'
#' @param predictors_raster Raster environnemental optionnel.
#' Si fourni, un raster de suitabilite est produit (probabilites).
#'
#' @param ntree_values Valeurs de ntree a tester lors du tuning.
#'
#' @return Liste contenant :
#' - model : meilleur modele Random Forest
#' - best_ntree : valeur ntree selectionnee
#' - train_accuracy : precision sur le jeu d'entrainement
#' - test_accuracy : precision sur le jeu de test
#' - importance : importance des variables
#' - suitability_raster : raster de probabilites (si predictors_raster fourni)
#'
#' @details
#' Le tuning selectionne le ntree maximisant l'accuracy
#' sur le jeu de test (30 pourcent des donnees).
#'
#' Le raster de suitabilite contient les probabilites
#' de presence (classe "1") issues de predict(..., type = "prob").
#'
#' @examples
#' \dontrun{
#' model <- train_sdm_model(
#'   data,
#'   target = "presence"
#' )
#' }
#'
#' @export

train_sdm_model <- function(

  data,
  target            = "presence",
  predictors_raster = NULL,
  ntree_values      = c(100, 300, 500)

){

  # ==========================
  # Verifications
  # ==========================

  if(!target %in% names(data)){
    stop("Variable cible '", target, "' introuvable dans data.")
  }

  # Conversion en facteur pour classification
  data[[target]] <- as.factor(data[[target]])

  set.seed(123)

  index <- sample(
    seq_len(nrow(data)),
    size = round(0.7 * nrow(data))
  )

  train <- data[index, ]
  test  <- data[-index, ]

  formula <- stats::as.formula(paste(target, "~ ."))

  # ==========================
  # Tuning ntree
  # ==========================

  best_model    <- NULL
  best_accuracy <- -Inf
  best_ntree    <- NULL

  for(nt in ntree_values){

    model_tmp <- randomForest::randomForest(
      formula,
      data      = train,
      ntree     = nt,
      importance = TRUE
    )

    pred_tmp <- stats::predict(model_tmp, test)

    acc_tmp <- mean(pred_tmp == test[[target]], na.rm = TRUE)

    if(acc_tmp > best_accuracy){
      best_accuracy <- acc_tmp
      best_model    <- model_tmp
      best_ntree    <- nt
    }

  }

  # ==========================
  # Validation train / test
  # ==========================

  pred_train <- stats::predict(best_model, train)
  pred_test  <- stats::predict(best_model, test)

  train_accuracy <- mean(pred_train == train[[target]], na.rm = TRUE)
  test_accuracy  <- mean(pred_test  == test[[target]],  na.rm = TRUE)

  # ==========================
  # Importance des variables
  # ==========================

  importance <- randomForest::importance(best_model)

  # ==========================
  # Raster de suitabilite
  # (probabilites de presence, classe "1")
  # ==========================

  suitability_raster <- NULL

  if(!is.null(predictors_raster)){

    suitability_raster <- terra::predict(
      predictors_raster,
      best_model,
      type    = "prob",
      index   = 2      # colonne de la classe "1" (presence)
    )

    names(suitability_raster) <- "suitability"

  }

  return(

    list(
      model              = best_model,
      best_ntree         = best_ntree,
      train_accuracy     = train_accuracy,
      test_accuracy      = test_accuracy,
      importance         = importance,
      suitability_raster = suitability_raster
    )

  )

}
