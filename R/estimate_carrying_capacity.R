#' Estimation de la capacité de charge pastorale
#'
#' Estime le nombre d'unités animales qu'une zone pastorale
#' peut supporter à partir de la biomasse disponible.
#'
#' @param biomass Biomasse disponible en kg/ha.
#' Peut être une valeur numérique ou un raster terra.
#'
#' @param animal_demand Consommation moyenne d'une unité animale
#' en kg de matière sèche.
#'
#' @return Liste contenant :
#' - capacity : capacité de charge (UBT/ha)
#' - pressure : classe de pression pastorale
#'
#' @details
#' La capacité de charge est calculée selon :
#'
#' \deqn{
#' CarryingCapacity =
#' \frac{AvailableBiomass}{AnimalDemand}
#' }
#'
#' Classification :
#' - Faible : < 0.5 UBT/ha
#' - Modérée : 0.5 - 1 UBT/ha
#' - Elevée : > 1 UBT/ha
#'
#' @references
#' Holechek, J.L., Pieper, R.D.,
#' & Herbel, C.H. (2010).
#' Range Management:
#' Principles and Practices.
#'
#' @examples
#' capacity <- estimate_carrying_capacity(
#'   biomass = 500,
#'   animal_demand = 250
#' )
#'
#' @export


estimate_carrying_capacity <- function(

  biomass,

  animal_demand = 250

){


  # ==========================
  # Vérification
  # ==========================


  if(animal_demand <= 0){

    stop("Animal demand must be positive")

  }



  # ==========================
  # Calcul capacité UBT/ha
  # ==========================


  capacity <- biomass / animal_demand



  # ==========================
  # Classification pression
  # ==========================


  if(inherits(capacity, "SpatRaster")){


    pressure <- terra::classify(

      capacity,

      rbind(

        c(-Inf, 0.5, 1),

        c(0.5, 1, 2),

        c(1, Inf, 3)

      )

    )


  } else {


    pressure <- cut(

      capacity,

      breaks = c(-Inf,0.5,1,Inf),

      labels = c(

        "Faible",

        "Moderee",

        "Elevee"

      )

    )


  }



  # ==========================
  # Output
  # ==========================


  return(

    list(

      capacity = capacity,

      pressure = pressure

    )

  )

}
