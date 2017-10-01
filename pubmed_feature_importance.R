###### Libraries ###### 
library(tibble)
library(reshape2)
library(syuzhet)
library(tm)
library(RISmed)
library(lattice)
library(ggplot2)
library(caret)
library(mlr)
library(tidyr)
library(dplyr)
library(BoutrosLab.plotting.general)
options(scipen=999)
DATE <-format.Date(Sys.Date(),format = "%Y-%m-%d")

# Load
# Also load the train and test predictions 
load("2017-09-30_xgb_model_all.Rdata")
xgboost.model <- readRDS('2017-09-30_xgb_model_all.rda')
all.features.year <- readRDS('2017-09-30_all_feature_importance.rda')

target.years <- c(2010:2015)
##### Plot Regression Results ####################
test.pred$data$class <- 'test'
test.pred$data$id <- rownames(test.pred$data)
train.pred$data$class <- 'train'
predict.toplot <- data.frame(rbind(test.pred$data, train.pred$data))

predict.toplot$class <- as.factor(predict.toplot$class)
levels(predict.toplot$class)

create.scatterplot(
  formula = response ~ truth,
  data = predict.toplot,
  height = 4,
  width = 6,
  pch = 19,
  cex = 0.5,
  alpha = 0.6,
  filename = generate.filename('main_model', 'predicted','png'),
  type = c('p','g'),
  col = c('dodgerblue','darkorange'),
  xlab.cex = 1.5,
  xaxis.cex = 1.25,
  ylab.cex = 1.5,
  yaxis.cex = 1.25,
  xlab.label = 'Truth',
  ylab.label = 'Predicted',
  style = 'Nature',
  key = list(
    text = list(
      lab = c('Test', 'Train'),
      cex = 1, 
      col = 'black'
    ),
    points = list(
      pch = 19,
      col = c('dodgerblue','darkorange'),
      cex = 1
    ),
    x = 0.04,
    y = 0.94,
    padding.text = 2
  )
  
  
)

#### Feature importance for main model #### 

feat.iwant <-getFeatureImportance(xgboost.model)
feature.iwant <- feat.iwant$res
feature.iwant <- as.data.frame(t(feature.iwant))
feature.iwant$feature <- rownames(feature.iwant)
feature.iwant <- feature.iwant[order(feature.iwant$V1, decreasing=TRUE),]
features.good <- filter(feature.iwant, V1>0.01)
features.good$order <- nrow(features.good):1

create.barplot(
  formula = order ~ V1,
  data = features.good,
  plot.horizontal = TRUE,
  yaxis.lab = rev(features.good$feature),
  ylab.label = '',
  xlab.label = 'Importance',
  xlab.cex = 1.7,
  xaxis.cex = 1.5,
  yaxis.cex = 1.5,
  xlimits = c(-0.01,0.21),
  col = 'dodgerblue',
  style ='Nature'
  )

#### Pull feature ranks per year ####


# Collect top 0.01 from each year
best.features <- sapply(all.features.year, function(x) {
  features.toget <- filter(x, V1 > 0.05)$feature
  return(features.toget)
})

# Condense and remove dups
best.features <- unique(as.vector(unlist(best.features)))

# pull target features from all years into matrix
features.df <- do.call(rbind, all.features.year)
features.df$year <- gsub('y', '', substr(rownames(features.df), 1,5))
best.features.df <- features.df %>% 
  filter(feature %in% best.features) %>%
  complete(feature, year, fill = list(V1 = 0))

best.features.df$year <- as.factor(best.features.df$year)

create.barplot(
  formula = feature ~ V1 | year,
  data = best.features.df,
  plot.horizontal = TRUE,
  #yaxis.lab = rev(features.good$feature),
  ylab.label = '',
  xlab.label = 'Importance',
  xlab.cex = 1.7,
  xaxis.cex = 1.5,
  yaxis.cex = 1.5,
  #xlimits = c(-0.01,0.21),
  col = 'dodgerblue',
  style ='Nature',
  layout = c(6,1)
)

#### Merge feature importance set ####
combined.features <- union(features.good$feature, best.features)
plot.cols <- rep('darkorange', length(combined.features))
plot.cols[match(features.good$feature, combined.features)] <- 'dodgerblue'
test.cols <- as.vector(unlist(sapply(plot.cols, function(x) rep(x, 7))))

year.features.df <- features.df %>% 
  filter(feature %in% combined.features) %>%
  complete(feature, year, fill = list(V1 = 0)) %>%
  dplyr::select(feature, year, V1)

whole.features.df <- feature.iwant %>%
  filter(feature %in% combined.features) %>%
  complete(feature, fill = list(V1 = 0))

# prep for rbind
whole.features.df$period <- 'All'
colnames(year.features.df)[2] <- 'period'

all.features.toplot <- rbind(whole.features.df, year.features.df)

# Order using features from whole model, and then tack on the rest 
all.features.toplot$feature <- factor(all.features.toplot$feature, levels = rev(combined.features))
all.features.toplot$period <- factor(all.features.toplot$period, levels = c('All', 2010:2015))


create.barplot(
  formula = feature ~ V1 | period,
  data = all.features.toplot,
  filename = generate.filename('all_models','importance','png'),
  width = 11,
  height = 5,
  plot.horizontal = TRUE,
  #yaxis.lab = rev(features.good$feature),
  ylab.label = '',
  xlab.label = 'Importance',
  xlab.cex = 1.5,
  xaxis.cex = 1.25,
  yaxis.cex = 1.25,
  #xlimits = c(-0.01,0.21),
  col = 'dodgerblue',
  style ='Nature',
  layout = c(7,1),
  xat = c(0 , 0.25),
  xlimits = c(-0.02, 0.53),
  abline.h = 5.5,
  abline.lwd = 2, 
  abline.col = 'darkred',
  x.spacing = 0.2,
  add.grid = TRUE,
  grid.lwd = 2
  )

######### 
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
