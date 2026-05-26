#' Estimation de la capacité de charge pastorale
#'
#' Estime le nombre d'animaux qu'une zone pastorale
#' peut supporter à partir de la biomasse disponible.
#'
#' @param biomass Biomasse disponible en kg/ha
#' @param consumption Consommation moyenne d'un animal en kg
#'
#' @return Capacité de charge animale
#'
#' @details
#' La capacité de charge est calculée selon :
#' \deqn{
#' Capacite = \frac{Biomasse}{Consommation animale}
#' }
#'
#' Une capacité élevée indique une bonne disponibilité
#' fourragère et une pression pastorale faible.
#'
#' @references
#' Holechek, J.L., Pieper, R.D., & Herbel, C.H. (2010).
#' Range Management: Principles and Practices.
#'
#' @examples
#' # Biomasse disponible
#' biomass <- 500
#'
#' # Consommation moyenne d'un animal
#' consumption <- 250
#'
#' estimate_carrying_capacity(biomass, consumption)
#'
#' @export

estimate_carrying_capacity <- function(biomass, consumption){

  if(consumption <= 0){

    stop("La consommation animale doit être positive")

  }

  capacity <- biomass / consumption

  return(round(capacity, 2))

}



#' Classification de la capacité pastorale
#'
#' Classe les valeurs de capacité de charge en catégories.
#'
#' @param capacity Valeurs de capacité pastorale
#'
#' @return Facteur contenant les classes pastorales
#'
#' @examples
#' values <- c(0.2, 0.8, 1.5, 3, 5)
#'
#' classify_carrying_capacity(values)
#'
#' @export

classify_carrying_capacity <- function(capacity){

  classes <- cut(

    capacity,

    breaks = c(-Inf, 0.5, 1, 2, 4, Inf),

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
