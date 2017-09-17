###### Libraries ###### 
library(dplyr)
library(tibble)
library(reshape2)
library(syuzhet)
library(tm)
library(RISmed)
library(lattice)
library(ggplot2)
library(caret)
library(mlr)
options(scipen=999)
DATE <-format.Date(Sys.Date(),format = "%Y-%m-%d")

###### Data ########

all.data <- readRDS('G://Analyses/pubmed/data/2017-09-09_pubmed_processed_data.rda')

# Add ceiling to n_authors and center
summary(all.data$n_author)
authors_upper <- quantile(all.data$n_author, 0.99)
temp.n <- all.data$n_author
temp.n[temp.n>authors_upper] <- authors_upper

all.data$n_authors_scaled <- scale(temp.n, center = TRUE, scale = TRUE)

summary(all.data$impact_factor)
if_upper <- quantile(all.data$impact_factor, 0.99)
temp.if <- all.data$impact_factor
temp.if[temp.if>if_upper] <- if_upper

all.data$impact_factor_scaled <- scale(temp.if, center = TRUE, scale = TRUE)



########## regression ##########
omit.cols <- c(1,2,4,5,6,18)
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
  eval_metric = 'mae', 
  nthread=4, 
  gamma=1)


# Tune nrounds
rounds.params <- makeParamSet(
  makeDiscreteParam('nrounds', values = seq(10,100,10))
  )

# CV strat
# re.strat <- makeResampleDesc("CV", iters=4)

# tuning strat - random fun
ctrl <- makeTuneControlGrid(resolution=10)

# Nested cross validation cause there's no such thing as overkill? 
inner = makeResampleDesc("CV", iters = 4)

inner.learner <- makeTuneWrapper(
  xgboost.lrn, 
  resampling = inner, 
  par.set = rounds.params, 
  control = ctrl, 
  measures = list(mae, rmse, mse),
  show.info = TRUE
)

outer.wrap <- makeResampleDesc("CV", iters = 10)

# Tuning
tune.someparams <- resample(
  inner.learner, 
  regression.task, 
  resampling = outer.wrap, 
  extract = getTuneResult, 
  measures = list(mae, rmse, mse),
  show.info = TRUE
)

tuning_results <- getNestedTuneResultsOptPathDf(tune.someparams)

# Find best nrounds
best_results <- tuning_results %>% 
  group_by(iter) %>%
  slice(which.min(mae.test.mean)) %>%
  as.data.frame

# Determine best nrounds
best.param <- as.numeric(as.character(best_results[which.min(best_results$mse.test.mean),1]))

# Adjust learner
xgboost.lrn <- setHyperPars(
  xgboost.lrn, 
  nrounds=best.param,
  eval_metric = 'rmse'
)


# nrounds25? RMSE 19.6
xgboost.model <- mlr::train(learner = xgboost.lrn, task = regression.task)

test.pred <- predict(xgboost.model, newdata = test.data)
train.pred <- predict(xgboost.model, task = regression.task)
plot(test.pred$data$truth, test.pred$data$response)
plot(train.pred$data$truth, train.pred$data$response)

cor(test.pred$data$truth, test.pred$data$response)


boop<-getFeatureImportance(xgboost.model)
feature.iwant <- boop$res
feature.iwant <- as.data.frame(t(feature.iwant))
feature.iwant$feature <- rownames(feature.iwant)
feature.iwant <- feature.iwant[order(feature.iwant$V1, decreasing=TRUE),]
head(feature.iwant, 25)

out.filename <- paste0(DATE, '_xgb_model_all', '.rda')
saveRDS(xgboost.model, out.filename)
