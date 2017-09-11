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


###### Data ########

all.data <- readRDS('G://Analyses/pubmed/data/2017-09-09_pubmed_processed_data.rda')

########## regression ##########
omit.cols <- c(1,2)
#train.data <- all.data[,-omit.cols]

# Split data 
data.part <- createDataPartition(all.data$year, p = 0.75, list=FALSE)
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
  nrounds=250,
  eval_metric = 'rmse', 
  nthread=4, 
  gamma=1)


# Tune hyperparameters
rounds.params <- makeParamSet(
  makeDiscreteParam('nrounds', values = seq(25,350,25))
  #makeIntegerParam("max_depth",lower = 7L,upper = 10L),
  #makeNumericParam("subsample",lower = 0.5,upper = 1), 
  #makeNumericParam("colsample_bytree",lower = 0.5,upper = 1)
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


xgboost.model <- mlr::train(learner = xgboost.lrn, task = regression.task)

test.pred <- predict(xgboost.model, newdata = test.data)
#test.pred <- predict(xgboost.model, task = regression.task)

boop<-getFeatureImportance(xgboost.model)
feature.iwant <- boop$res
feature.iwant <- as.data.frame(t(feature.iwant))
feature.iwant$feature <- rownames(feature.iwant)
feature.iwant <- feautre.iwant[order(feature.iwant$V1, decreasing=TRUE),]
head(feature.iwant, 25)

plot(test.pred$data$truth, test.pred$data$response)

cor(test.pred$data$truth, test.pred$data$response)
plot(density(train.data$citations))
