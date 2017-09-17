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
options(scipen=999)
DATE <-format.Date(Sys.Date(),format = "%Y-%m-%d")


target.set <- load.prep.data(seed=123, target.year=2010)
model.results <- model.data(
  train.data = target.set$train,
  test.data = target.set$test
  )

feature.ranks <- rank.features(model.results[[1]])
assess.model(model.results[[2]], model.results[[3]] )


target.years <- c(2010:2015)

all.features.year <- lapply(target.years, function(x) {
  target.set <- load.prep.data(seed=123, target.year=x)
  model.results <- model.data(
    train.data = target.set$train,
    test.data = target.set$test
  )
  
  feature.ranks <- rank.features(model.results[[1]])
  return(feature.ranks)
})

names(all.features.year) <- paste0('y', target.years)
temp <- all.features.year
#### Pull feature ranks per year ####
# All year to everything in list
for (i in 1:length(target.years)) {
  all.features.year[[i]]$year <- target.years[i]
  
  # Fix the ranks so all models with a coefficient of 0 are assigned the same ranks 
  temp <- all.features.year[[i]]
  rank.min <- temp[temp$V1 ==0, 3][1]
  all.features.year[[i]]$rank[all.features.year[[i]]$V1 == 0 ] <- rank.min
}

# Collect top 5 from each year
best.features <- sapply(all.features.year, function(x) {
  features.toget <- x$feature[1:3]
  return(features.toget)
})

# Condense and remove dups
best.features <- unique(as.vector(best.features))

# pull target features from all years into matrix
features.df <- do.call(rbind, all.features.year)
best.features.df <- features.df %>% 
  filter(feature %in% best.features)

best.features.matrix <- dcast(best.features.df, formula = feature ~ year, value.var = 'rank')

best.features.matrix[order(best.features.matrix$`2010`),]
# Plot changes over time 

