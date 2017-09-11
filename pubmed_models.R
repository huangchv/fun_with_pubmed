###### Libraries ###### 
library(dplyr)
library(tidyr)
library(tidytext)
library(tibble)
library(readr)
library(stringr)
library(SnowballC)
library(reshape2)
library(syuzhet)
library(tm)
library(RISmed)
library(lattice)
library(ggplot2)
library(reshape2)
library(caret)
library(mlr)
library(mxnet)
options(scipen=999)


###### Data ########

all.data <- readRDS('G://Analyses/pubmed/data/2017-09-09_pubmed_processed_data.rda')

# Add ceiling to n_authors and center
summary(all.data$n_author)
authors_upper <- quantile(all.data$n_author, 0.99)
temp.n <- all.data$n_author
temp.n[temp.n>authors_upper] <- authors_upper

all.data$n_authors_scaled <- scale(temp.n, center = TRUE, scale = TRUE)
########## regression ##########
omit.cols <- c(1,2,4,5,6)
#train.data <- all.data[,-omit.cols]

# Split data 
data.part <- createDataPartition(all.data$citations, p = 0.75, list=FALSE)
train.data <- all.data[data.part, -omit.cols]
test.data <- all.data[-data.part, -omit.cols]


# Task
regression.task = makeRegrTask(id = "pubmed_regression", data = train.data, target = 'citations')

# Define learner
xgboost.lrn <- makeLearner(cl = 'regr.xgboost')

#getParamSet(xgboost.lrn)
xgboost.lrn <- setHyperPars(
  xgboost.lrn, 
  eta = .03333, 
  subsample = .7, 
  max_depth=4, 
  nrounds=300,
  eval_metric = 'rmse', 
  nthread=4, 
  gamma=1)


# Tune nrounds
rounds.params <- makeParamSet(
  makeDiscreteParam('nrounds', values = seq(25,650,25))
  )

# CV strat
re.strat <- makeResampleDesc("CV", iters=4)

# tuning strat - random fun
ctrl <- makeTuneControlGrid(resolution=10)
#ctrl <- makeTuneControlRandom(maxit = 100L)


# Tune =| 
mytune <- tuneParams(
  learner = xgboost.lrn, 
  task = regression.task, 
  resampling = re.strat, 
  measures = rmse, 
  par.set = rounds.params, 
  control = ctrl, 
  show.info = T)

# nrounds25? RMSE 19.6
xgboost.model <- mlr::train(learner = xgboost.lrn, task = regression.task)

test.pred <- predict(xgboost.model, newdata = test.data)
train.pred <- predict(xgboost.model, task = regression.task)
plot(test.pred$data$truth, test.pred$data$response)
plot(train.pred$data$truth, train.pred$data$response)

cor(test.pred$data$truth, test.pred$data$response, method='spearman')


boop<-getFeatureImportance(xgboost.model)
feature.iwant <- boop$res
feature.iwant <- as.data.frame(t(feature.iwant))
feature.iwant$feature <- rownames(feature.iwant)
feature.iwant <- feature.iwant[order(feature.iwant$V1, decreasing=TRUE),]
head(feature.iwant, 25)

plot(test.pred$data$truth, test.pred$data$response)
plot(train.pred$data$truth, train.pred$data$response)

cor(test.pred$data$truth, test.pred$data$response)
plot(density(train.data$citations))
