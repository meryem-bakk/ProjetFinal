#' Preparation des variables environnementales
#'
#' Prepare les predicteurs environnementaux utilises
#' pour la modelisation des especes fourrageres (SDM).
#'
#' @param occurrence Objet sf contenant les points
#' de presence des especes.
#'
#' @param predictors Raster contenant les variables
#' environnementales (NDVI, LAI, climat, altitude).
#'
#' @param remove_cor Logical. Supprimer ou non les
#' variables fortement correlees.
#'
#' @return Dataframe pret pour la modelisation ML.
#'
#' @details
#' Cette fonction permet :
#'
#' - extraction des valeurs raster aux points GPS,
#' - combinaison presence + environnement,
#' - suppression des valeurs manquantes,
#' - suppression optionnelle des variables correlees.
#'
#' Variables possibles :
#' - NDVI,
#' - LAI,
#' - temperature,
#' - precipitation,
#' - altitude.
#'
#' Les donnees produites sont utilisees par :
#' train_sdm_model().
#'
#' @references
#' Elith, J. & Leathwick, J.R. (2009).
#' Species Distribution Models:
#' Ecological explanation and prediction.
#' Annual Review of Ecology Evolution and Systematics.
#'
#' @examples
#'
#' \dontrun{
#'
#' data <- prepare_predictors(
#'   occurrence = species$sf_object,
#'   predictors = climate
#' )
#'
#' }
#'
#' @export

prepare_predictors <- function(
    occurrence,
    predictors,
    remove_cor = TRUE
){

  # ============================
  # Verification
  # ============================

  if(!inherits(occurrence, "sf")){
    stop("occurrence doit etre un objet sf")
  }


  # ============================
  # Conversion sf -> terra
  # ============================

  points <- terra::vect(occurrence)


  # ============================
  # Extraction raster
  # ============================

  values <- terra::extract(
    predictors,
    points
  )


  # Supprimer ID terra
  values <- values[,-1]


  # ============================
  # Creation dataframe ML
  # ============================

  data_ml <- cbind(

    sf::st_drop_geometry(occurrence),

    values

  )

  # Variable cible SDM

  data_ml$presence <- 1


  # Supprimer NA

  data_ml <- stats::na.omit(data_ml)



  # ============================
  # Suppression correlations
  # ============================

  if(remove_cor){

    numeric_data <- data_ml[
      ,
      sapply(data_ml, is.numeric)
    ]


    cor_matrix <- stats::cor(
      numeric_data,
      use = "complete.obs"
    )


    high_cor <- which(
      abs(cor_matrix) > 0.9 &
        abs(cor_matrix) < 1,
      arr.ind = TRUE
    )


    if(nrow(high_cor) > 0){

      remove <- unique(
        rownames(high_cor)
      )

      data_ml <- data_ml[
        ,
        !names(data_ml) %in% remove
      ]

    }

  }


  return(data_ml)

}
