# rangelandSDM <img src="https://img.shields.io/badge/R-%3E%3D4.0-blue" align="right"/>

> Package R pour la modélisation des espèces fourragères et la gestion pastorale durable dans les zones arides et semi-arides.

---

## Table des matières

- [Description](#description)
- [Installation](#installation)
- [Sources de données](#sources-de-données)
- [Workflow complet](#workflow-complet)
- [Exemples détaillés et outputs](#exemples-détaillés-et-outputs)
- [Fonctions disponibles](#fonctions-disponibles)
- [Dépendances](#dépendances)
- [Références](#références)

---

## Description

`rangelandSDM` est un package R dédié à l'analyse des parcours pastoraux.
Il couvre l'ensemble de la chaîne analytique, depuis l'import des données
terrain et télédétection jusqu'à la production de cartes et de rapports
de gestion.

**Cas d'usage typique :** évaluation de la capacité de charge et
identification des zones refuges dans un parcours aride marocain
à partir de données GBIF, MODIS et WorldClim.

---

## Installation

```r
# Depuis le dossier local du package
devtools::install()

# Ou en mode développement (sans installation)
devtools::load_all()
```

---

## Sources de données

| Source | Données | Fonction |
|---|---|---|
| [GBIF](https://www.gbif.org) | Occurrences d'espèces fourragères | `import_forage_species()` |
| [MODIS MOD13Q1](https://lpdaac.usgs.gov/products/mod13q1v006/) | NDVI 250 m, 16 jours | `download_ndvi_lai()` |
| [MODIS MOD15A2H](https://lpdaac.usgs.gov/products/mod15a2hv006/) | LAI 500 m, 8 jours | `download_ndvi_lai()` |
| [WorldClim v2](https://www.worldclim.org) | Variables bioclimatiques | `load_climate_data()` |
| Terrain CSV / Excel | Biomasse mesurée (kg/ha) | `import_biomass_data()` |

---

## Workflow complet

```
import_forage_species()          # occurrences GBIF ou CSV terrain
        |
clean_occurrences()              # nettoyage spatial
        |
download_ndvi_lai()              # NDVI + LAI (MODIS ou fichiers locaux)
load_climate_data()              # variables bioclimatiques WorldClim
import_biomass_data()            # biomasse terrain
        |
calculate_ndvi()                 # calcul NDVI depuis bandes NIR/Red
calculate_biomass()              # estimation biomasse (kg/ha)
prepare_predictors()             # extraction raster + mise en forme ML
        |
train_sdm_model()                # Random Forest SDM avec tuning ntree
evaluate_model()                 # AUC, Accuracy, RMSE
        |
estimate_carrying_capacity()     # capacité de charge (UBT/ha)
calculate_grazing_pressure()     # indice de pression pastorale
identify_refuge_areas()          # zones refuges prioritaires
        |
generate_recommendations()       # recommandations de gestion
plot_rangeland_maps()            # export cartes PNG / PDF
generate_report()                # rapport HTML ou PDF automatique
```

---

## Exemples détaillés et outputs

### 1. Import des espèces fourragères (GBIF)

```r
library(rangelandSDM)

species <- import_forage_species(
  species_name = "Stipa tenacissima",
  limit        = 200
)

head(species$dataframe)
```

**Output :**
```
       species longitude latitude
1 Stipa tenacissima    -5.342   33.812
2 Stipa tenacissima    -4.871   34.102
3 Stipa tenacissima    -6.120   32.958
4 Stipa tenacissima    -3.445   35.201
5 Stipa tenacissima    -5.780   33.421
6 Stipa tenacissima    -4.234   34.670
```

```r
nrow(species$dataframe)
# [1] 187

class(species$sf_object)
# [1] "sf"         "data.frame"
```

---

### 2. Téléchargement NDVI et LAI (MODIS)

```r
# Mode automatique via MODISTools
veg <- download_ndvi_lai(
  lat   = 31.5,
  lon   = -7.5,
  start = "2023-01-01",
  end   = "2023-12-31",
  km_lr = 20,
  km_ab = 20
)
# Téléchargement NDVI (MOD13Q1)...
# Téléchargement LAI (MOD15A2H)...

veg$NDVI
# class       : SpatRaster
# dimensions  : 85, 85, 1  (nrow, ncol, nlyr)
# resolution  : 0.002083, 0.002083  (x, y)
# extent      : -7.688, -7.312, 31.312, 31.688  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat WGS 84 (EPSG:4326)
# name        : value
# min value   :  0.12
# max value   :  0.61

# Mode local (fichiers déjà téléchargés)
veg <- download_ndvi_lai(
  ndvi_file = "modis_data/ndvi.tif",
  lai_file  = "modis_data/lai.tif"
)
```

---

### 3. Calcul du NDVI depuis bandes brutes

```r
# Avec des valeurs scalaires
ndvi_val <- calculate_ndvi(nir = 0.8, red = 0.1)
ndvi_val
# [1] 0.7777778

# Avec des rasters
nir_rast  <- terra::rast("nir_band.tif")
red_rast  <- terra::rast("red_band.tif")
ndvi_rast <- calculate_ndvi(nir_rast, red_rast)

terra::global(ndvi_rast, "range", na.rm = TRUE)
#        min       max
# value 0.03  0.78
```

---

### 4. Estimation de la biomasse

```r
# Scalaire
biomass_val <- calculate_biomass(ndvi = 0.65)
biomass_val
# [1] 650   (kg/ha)

# Avec LAI
biomass_val2 <- calculate_biomass(ndvi = 0.65, lai = 2.1)
biomass_val2
# [1] 940   (kg/ha)

# Sur raster
biomass_rast <- calculate_biomass(ndvi = veg$NDVI, lai = veg$LAI)
terra::global(biomass_rast, "mean", na.rm = TRUE)
#       mean
# value  487.3
```

---

### 5. Modélisation SDM (Random Forest)

```r
climate <- load_climate_data(path = "climate_data", resolution = 10)

data_ml <- prepare_predictors(
  occurrence  = species$sf_object,
  predictors  = climate,
  remove_cor  = TRUE
)

dim(data_ml)
# [1] 183  12    (183 occurrences, 12 variables après filtrage corrélations)

model_result <- train_sdm_model(
  data              = data_ml,
  target            = "presence",
  ntree_values      = c(100, 300, 500),
  predictors_raster = climate
)

model_result$best_ntree
# [1] 300

model_result$train_accuracy
# [1] 0.9845

model_result$test_accuracy
# [1] 0.9286

head(model_result$importance)
#        MeanDecreaseAccuracy MeanDecreaseGini
# bio_1             12.34           8.21
# bio_12            18.67          14.53
# bio_4              9.11           6.78
# bio_15            15.22          11.34
# bio_7              7.45           5.02
```

---

### 6. Évaluation du modèle

```r
pred_proba <- stats::predict(
  model_result$model,
  data_ml,
  type = "prob"
)[, 2]

eval <- evaluate_model(
  observed   = as.numeric(as.character(data_ml$presence)),
  predicted  = pred_proba,
  show_plots = FALSE
)

eval$performance
#        AUC  Accuracy      RMSE
# 1  0.9712    0.9344    0.2187
```

---

### 7. Capacité de charge et pression pastorale

```r
capacity <- estimate_carrying_capacity(
  biomass       = biomass_rast,
  animal_demand = 250
)

capacity$capacity
# class       : SpatRaster
# name        : value
# min value   :  0.38  (UBT/ha)
# max value   :  2.44  (UBT/ha)

capacity$pressure
# class       : SpatRaster
# name        : value
# min value   :  1  (Faible)
# max value   :  3  (Elevée)

pressure <- calculate_grazing_pressure(
  biomass           = biomass_rast,
  carrying_capacity = capacity$capacity,
  animal_density    = 1.2
)

terra::global(pressure, "mean", na.rm = TRUE)
#       mean
# value  0.847   # < 1 : pression modérée
```

---

### 8. Identification des zones refuges

```r
refuge <- identify_refuge_areas(
  biomass          = biomass_rast,
  grazing_pressure = pressure,
  climate_score    = 0.8
)

refuge$refuge_index
# class       : SpatRaster
# name        : value
# min value   :  0.02
# max value   :  1.87

# Proportion de zones prioritaires
mean(terra::values(refuge$priority_area), na.rm = TRUE)
# [1] 0.49   # ~49% de la surface classée zone refuge
```

---

### 9. Recommandations de gestion

```r
recs <- generate_recommendations(
  carrying_capacity = capacity$capacity,
  grazing_pressure  = pressure,
  refuge_area       = refuge$priority_area
)

recs
# [1] "La gestion pastorale actuelle est compatible avec une utilisation durable des parcours."
# [2] "Proteger les zones refuges a forte valeur ecologique."
# [3] "Limiter le paturage dans les zones prioritaires de conservation."
```

---

### 10. Cartographie et rapport

```r
# Export des cartes
plot_rangeland_maps(
  biomass           = biomass_rast,
  ndvi              = veg$NDVI,
  carrying_capacity = capacity$capacity,
  grazing_pressure  = pressure,
  output_dir        = "outputs/maps",
  format            = "png"
)
# Fichiers produits :
# outputs/maps/biomass.png
# outputs/maps/ndvi.png
# outputs/maps/carrying_capacity.png
# outputs/maps/grazing_pressure.png

# Génération du rapport HTML
report_path <- generate_report(
  species           = species,
  biomass           = biomass_rast,
  carrying_capacity = capacity$capacity,
  grazing_pressure  = pressure,
  recommendations   = recs,
  output_format     = "html",
  output_dir        = "outputs"
)

report_path
# [1] "outputs/report.html"
```

---

## Fonctions disponibles

### Importation et préparation des données

| Fonction | Description |
|---|---|
| `import_forage_species()` | Import occurrences GBIF ou CSV terrain |
| `clean_occurrences()` | Nettoyage spatial des occurrences (doublons, coordonnées invalides) |
| `import_biomass_data()` | Import données biomasse terrain CSV / Excel |
| `download_ndvi_lai()` | Téléchargement NDVI + LAI depuis MODIS ou fichiers locaux |
| `load_climate_data()` | Téléchargement variables bioclimatiques WorldClim |
| `prepare_predictors()` | Extraction raster + mise en forme pour modèles ML |

### Analyse environnementale

| Fonction | Description |
|---|---|
| `calculate_ndvi()` | Calcul NDVI depuis bandes NIR et Rouge |
| `calculate_biomass()` | Estimation biomasse végétale (kg/ha) |

### Modélisation SDM

| Fonction | Description |
|---|---|
| `train_sdm_model()` | Entraînement Random Forest avec tuning ntree |
| `evaluate_model()` | Évaluation AUC, Accuracy, RMSE |

### Analyse pastorale

| Fonction | Description |
|---|---|
| `estimate_carrying_capacity()` | Capacité de charge (UBT/ha) |
| `calculate_grazing_pressure()` | Indice de pression pastorale |
| `identify_refuge_areas()` | Identification zones refuges prioritaires |

### Aide à la décision et outputs

| Fonction | Description |
|---|---|
| `generate_recommendations()` | Recommandations de gestion durable |
| `plot_rangeland_maps()` | Export cartes PNG / PDF |
| `generate_report()` | Rapport HTML ou PDF automatique |

---

## Dépendances

```r
install.packages(c(
  "terra", "sf", "randomForest", "readxl",
  "pROC", "geodata", "rgbif", "rmarkdown", "MODISTools"
))
```

---

## Références

- Tucker, C.J. (1979). Red and photographic infrared linear combinations for monitoring vegetation. *Remote Sensing of Environment*.
- Holechek, J.L., Pieper, R.D. & Herbel, C.H. (2010). *Range Management: Principles and Practices*.
- Fick, S.E. & Hijmans, R.J. (2017). WorldClim 2: new climate surfaces for global land areas. *International Journal of Climatology*.
- Elith, J. & Leathwick, J.R. (2009). Species Distribution Models. *Annual Review of Ecology, Evolution, and Systematics*.
- Fielding, A.H. & Bell, J.F. (1997). A review of methods for the assessment of prediction errors in conservation presence/absence models.
- Franklin, J. (2010). *Mapping Species Distributions: Spatial Inference and Prediction*.
- Didan, K. (2015). MOD13Q1 MODIS/Terra Vegetation Indices 16-Day L3 Global 250m. NASA EOSDIS Land Processes DAAC.
- Myneni, R. et al. (2015). MOD15A2H MODIS/Terra Leaf Area Index 8-Day L4 Global 500m. NASA EOSDIS Land Processes DAAC.

---

*Package développé par Meryem EL BAKKOURI — IAV Hassan II, Département de Statistique et d'Informatique Appliquées, 2026.*
