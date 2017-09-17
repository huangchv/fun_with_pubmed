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
#
#saveRDS(all.features.year, '2017-09-17_all_feature_importance.rda')
all.features.year <- readRDS('2017-09-17_all_feature_importance.rda')

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


# Convert to factors
best.features.df$feature <- as.factor(best.features.df$feature)
feature.order <- levels(best.features.df$feature)

best.features.matrix <- dcast(best.features.df, formula = feature ~ year, value.var = 'rank')

best.features.matrix[order(best.features.matrix$`2010`),]

# To do
# Set floor to 20, anything beyond that goes to 20
# Relabel yaxis
best.features.df$rank[best.features.df$rank > 15] <-15

# Plot changes over time 
create.scatterplot(
  data = best.features.df,
  formula = rank ~ year,
  groups = feature,
  col = default.colours(12),
  xlimits = c(2009.8, 2015.2),
  xat = 2010:2016,
  type = 'b',
  cex = 1.25,
  ylimits = c(15,0),
  yat = seq(0,15,3),
  yaxis.lab = c(seq(0,12,3), '\u2265 15'),
  xaxis.cex = 1.5,
  yaxis.cex = 1.5,
  xlab.cex = 1.75,
  ylab.cex = 1.75,
  xlab.label = '',
  ylab.label = 'Rank',
  lwd = 2,
  key = list(
    text = list(
      lab = feature.order,
      cex = 1,
      col = 'black'
    ),
    lines = list(
      pch = 19,
      col = default.colours(12),
      cex = 1,
      lwd = 2
    ),
    x = 1, 
    y = 0.75
  ),
  right.padding = 23,
  style = 'Nature'
)
