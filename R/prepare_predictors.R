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
#' @param target Nom de la variable cible a exclure
#' du calcul de correlation. Par defaut "presence".
#'
#' @param remove_cor Logical. Supprimer ou non les
#' variables fortement correlees (seuil > 0.9).
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
#' La variable cible (par defaut "presence") est
#' exclue du calcul de correlation pour ne pas
#' etre supprimee par erreur.
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
#'   occurrence  = species$sf_object,
#'   predictors  = climate
#' )
#'
#' }
#'
#' @export

prepare_predictors <- function(

  occurrence,
  predictors,
  target     = "presence",
  remove_cor = TRUE

){

  # ============================
  # Verification
  # ============================

  if(!inherits(occurrence, "sf")){
    stop("occurrence doit etre un objet sf.")
  }

  # ============================
  # Conversion sf -> terra
  # ============================

  points <- terra::vect(occurrence)

  # ============================
  # Extraction raster
  # ============================

  values <- terra::extract(predictors, points)

  # Supprimer l'ID terra (premiere colonne)
  values <- values[, -1]

  # ============================
  # Creation dataframe ML
  # ============================

  data_ml <- cbind(
    sf::st_drop_geometry(occurrence),
    values
  )

  # Variable cible SDM
  data_ml$presence <- 1

  # Suppression NA
  data_ml <- stats::na.omit(data_ml)

  # ============================
  # Suppression correlations
  # (en excluant la variable cible)
  # ============================

  if(remove_cor){

    # Colonnes numeriques sans la variable cible
    numeric_cols <- names(data_ml)[sapply(data_ml, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, target)

    numeric_data <- data_ml[, numeric_cols, drop = FALSE]

    cor_matrix <- stats::cor(numeric_data, use = "complete.obs")

    high_cor <- which(
      abs(cor_matrix) > 0.9 & abs(cor_matrix) < 1,
      arr.ind = TRUE
    )

    if(nrow(high_cor) > 0){

      # Supprimer uniquement les colonnes de predicteurs
      # (jamais la variable cible)
      remove_cols <- setdiff(
        unique(rownames(high_cor)),
        target
      )

      data_ml <- data_ml[
        , !names(data_ml) %in% remove_cols,
        drop = FALSE
      ]

    }

  }

  return(data_ml)

}
