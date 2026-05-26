#' Estimation simplifiée de la biomasse à partir du NDVI
#'
#' Estime la biomasse végétale aérienne à partir des valeurs NDVI
#' en utilisant une relation linéaire simplifiée inspirée des études
#' de télédétection appliquées aux écosystèmes pastoraux.
#'
#' @param ndvi Valeurs NDVI comprises entre -1 et 1
#'
#' @return Biomasse estimée en kg/ha
#'
#' @details
#' La biomasse est estimée selon la relation :
#' \deqn{Biomasse = 100 \times NDVI}
#'
#' Cette relation simplifiée suppose qu'une augmentation du NDVI
#' correspond à une augmentation de la densité et de la productivité
#' de la végétation.
#'
#' Interprétation :
#' - Biomasse faible : végétation sparse ou zones dégradées
#' - Biomasse modérée : parcours exploitables
#' - Biomasse élevée : forte disponibilité fourragère
#'
#' @references
#' Tucker, C.J. (1979).
#' Red and photographic infrared linear combinations
#' for monitoring vegetation.
#' Remote Sensing of Environment, 8(2), 127-150.
#'
#' Paruelo, J.M., Epstein, H.E., Lauenroth, W.K.,
#' & Burke, I.C. (1997).
#' ANPP estimates from NDVI for the central grassland
#' region of the United States.
#' Ecology, 78(3), 953-958.
#'
#' @examples
#' # Biomasse d'une végétation dense
#' calculate_biomass(0.75)
#'
#' # Biomasse sur plusieurs pixels
#' ndvi_vals <- c(0.1, 0.3, 0.5, 0.8)
#'
#' calculate_biomass(ndvi_vals)
#'
#' @export

calculate_biomass <- function(ndvi){

  if(any(ndvi < -1 | ndvi > 1, na.rm = TRUE)){

    warning("Certaines valeurs NDVI sont hors de l'intervalle [-1, 1]")

  }

  biomass <- 100 * ndvi

  return(round(biomass, 2))

}



#' Classification de la biomasse pastorale
#'
#' Classe les valeurs de biomasse en catégories pastorales.
#'
#' @param biomass Valeurs de biomasse en kg/ha
#'
#' @return Facteur contenant les classes de biomasse
#'
#' @details
#' Les classes utilisées sont :
#' - Très faible
#' - Faible
#' - Modérée
#' - Élevée
#' - Très élevée
#'
#' @examples
#' biomasse <- c(5, 20, 45, 70, 95)
#'
#' classify_biomass(biomasse)
#'
#' @export

classify_biomass <- function(biomass){

  classes <- cut(
    biomass,

    breaks = c(-Inf, 10, 30, 60, 80, Inf),

    labels = c(
      "Tres faible",
      "Faible",
      "Moderee",
      "Elevee",
      "Tres elevee"
    ),

    include.lowest = TRUE
  )

  return(classes)

}
