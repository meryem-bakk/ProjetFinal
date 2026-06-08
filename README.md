
# ProjetFinal

## Description

ProjetFinal est un package R dédié à la modélisation des espèces
fourragères et à l’évaluation pastorale dans les zones arides et
semi-arides.

Le package permet :

- l’import des données d’espèces fourragères ;
- le nettoyage des occurrences spatiales ;
- le téléchargement et l’exploitation des données NDVI et LAI ;
- l’estimation de la biomasse végétale ;
- la modélisation de la distribution des espèces (SDM) ;
- l’évaluation de la capacité de charge animale ;
- l’analyse de la pression pastorale ;
- l’identification des zones refuges ;
- la génération de recommandations de gestion durable ;
- la production de rapports automatiques ;
- la cartographie pastorale.

## Sources de données

Le package est conçu pour utiliser :

- GBIF pour les occurrences d’espèces ;
- WorldClim pour les données climatiques ;
- MODIS NDVI ;
- MODIS LAI.

## Fonctions principales

### Importation et préparation des données

- `import_forage_species()`
- `clean_occurrences()`
- `import_biomass_data()`
- `download_ndvi_lai()`
- `load_climate_data()`
- `prepare_predictors()`

### Analyse environnementale

- `calculate_ndvi()`
- `calculate_biomass()`

### Modélisation

- `train_sdm_model()`
- `evaluate_model()`

### Analyse pastorale

- `estimate_carrying_capacity()`
- `calculate_grazing_pressure()`
- `identify_refuge_areas()`

### Aide à la décision

- `generate_recommendations()`
- `generate_report()`

### Cartographie

- `plot_rangeland_maps()`

## Installation

``` r
devtools::load_all()
```

## Exemple

``` r
calculate_ndvi(0.8, 0.1)

calculate_biomass(0.7)
```

## Objectif scientifique

Le package vise à :

- identifier les zones favorables aux espèces fourragères ;
- estimer la biomasse disponible ;
- calculer la capacité de charge animale ;
- évaluer la pression pastorale ;
- identifier les zones refuges prioritaires ;
- aider à la gestion durable des parcours.

## Types de données utilisés

- `sf`
- `SpatRaster`
- `data.frame`

## Technologies utilisées

- R
- terra
- sf
- randomForest
- pROC
- geodata
- rgbif
- rmarkdown
