###### Libraries ###### 
library(dplyr)
library(tidyr)
library(tibble)
library(SnowballC)
library(reshape2)
library(tm)
library(RISmed)
library(lattice)
library(ggplot2)
library(reshape2)
library(caret)
library(mlr)
library(BoutrosLab.plotting.general)
options(scipen=999)
DATE <-format.Date(Sys.Date(),format = "%Y-%m-%d")
source("fun_with_pubmed/pubmed_models_per_year_functions.R")

target.years <- c(2010:2015)

all.features.year <- lapply(target.years, function(x) {
  target.set <- load.prep.data(seed=123, target.year=x)
  model.results <- model.data(
    train.data = target.set$train,
    test.data = target.set$test,
    target.year = x
  )
  
  feature.ranks <- rank.features(model.results[[1]])
  return(feature.ranks)
})

names(all.features.year) <- paste0('y', target.years)
#
#saveRDS(all.features.year, '2017-09-30_all_feature_importance.rda')
