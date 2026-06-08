#' Train Species Distribution Model (SDM)
#'
#' Modélisation des espèces fourragères.
#'
#' @param data Dataframe contenant les variables environnementales
#' et la variable présence.
#'
#' @param target Variable cible.
#'
#' @param predictors_raster Raster environnemental optionnel.
#'
#' @param ntree_values Valeurs de ntree à tester.
#'
#' @return Liste contenant :
#' - modèle SDM
#' - importance variables
#' - performances train/test
#' - suitability raster (optionnel)
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

  target = "presence",

  predictors_raster = NULL,

  ntree_values = c(100, 300, 500)

){

  if(!target %in% names(data)){

    stop("Target variable not found")

  }

  # Conversion en facteur pour classification

  data[[target]] <- as.factor(
    data[[target]]
  )

  set.seed(123)

  index <- sample(

    1:nrow(data),

    size = round(0.7 * nrow(data))

  )

  train <- data[index, ]

  test <- data[-index, ]



  # ==========================
  # Tuning simple
  # ==========================

  best_model <- NULL

  best_accuracy <- -Inf

  best_ntree <- NULL

  formula <- stats::as.formula(

    paste(target, "~ .")

  )


  for(nt in ntree_values){

    model_tmp <- randomForest::randomForest(

      formula,

      data = train,

      ntree = nt,

      importance = TRUE

    )

    pred_tmp <- stats::predict(

      model_tmp,

      test

    )

    acc_tmp <- mean(

      pred_tmp == test[[target]]

    )

    if(acc_tmp > best_accuracy){

      best_accuracy <- acc_tmp

      best_model <- model_tmp

      best_ntree <- nt

    }

  }



  # ==========================
  # Validation train/test
  # ==========================

  pred_train <- stats::predict(

    best_model,

    train

  )

  pred_test <- stats::predict(

    best_model,

    test

  )

  train_accuracy <- mean(

    pred_train == train[[target]]

  )

  test_accuracy <- mean(

    pred_test == test[[target]]

  )



  # ==========================
  # Importance variables
  # ==========================

  importance <- randomForest::importance(

    best_model

  )



  # ==========================
  # Suitability raster
  # ==========================

  suitability_raster <- NULL

  if(!is.null(predictors_raster)){

    suitability_raster <- terra::predict(

      predictors_raster,

      best_model,

      type = "response"

    )

  }



  # ==========================
  # Output
  # ==========================

  return(

    list(

      model = best_model,

      best_ntree = best_ntree,

      train_accuracy = train_accuracy,

      test_accuracy = test_accuracy,

      importance = importance,

      suitability_raster = suitability_raster

    )

  )

}
