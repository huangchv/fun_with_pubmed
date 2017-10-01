###### Libraries ###### 
library(dplyr)
library(tidyr)
library(tibble)
library(SnowballC)
library(reshape2)
library(reshape2)
library(caret)
library(mlr)
options(scipen=999)
DATE <-format.Date(Sys.Date(),format = "%Y-%m-%d")


# Load function that processes the data
load.prep.data <- function(seed=1000, target.year=2010, filter = TRUE) {
  set.seed(seed)
  # Load data
  all.data <- readRDS('G://Analyses/pubmed/data/2017-09-09_pubmed_processed_data.rda')
  
  # Add ceiling to n_authors and center
  
  # summary(all.data$n_author)
  authors_upper <- quantile(all.data$n_author, 0.99)
  temp.n <- all.data$n_author
  temp.n[temp.n>authors_upper] <- authors_upper
  
  all.data$n_authors_scaled <- scale(temp.n, center = TRUE, scale = TRUE)[,1]
  
  # summary(all.data$impact_factor)
  if_upper <- quantile(all.data$impact_factor, 0.99)
  temp.if <- all.data$impact_factor
  temp.if[temp.if>if_upper] <- if_upper
  
  all.data$impact_factor_scaled <- scale(temp.if, center = TRUE, scale = TRUE)[,1]
  
  # Subset data to target year
  all.data <- filter(all.data, year == target.year)
  
  # Drop data with more than 1000 citations cause what the fuck?
  if (filter) {
    all.data <- filter(all.data, citations < 1000)
  }
  
  # Drop some colums
  omit.cols <- c(1,2,4,5,6,18)
  
  # Split data 
  data.part <- createDataPartition(all.data$citations, p = 0.75, list=FALSE)
  train.data <- all.data[data.part, -omit.cols]
  test.data <- all.data[-data.part, -omit.cols]
  
  to.return <- list(train.data, test.data)
  names(to.return) <- c('train', 'test')
  return(to.return)
}


# Function to fit the model with the input data
# Hyperparameters are mostly set, just nrounds
# nested cross validation to tune nrounds

model.data <- function(train.data, test.data, target.year, error.metric = 'mae') {
  
  # Set options based on selection of error.metric 
  
  # error measures
  errors.touse <- list(mae,rmse,mse)
  
  # nrounds to test
  # Fewer needed for mae
  rounds.params <- makeParamSet(
    makeDiscreteParam('nrounds', values = seq(10,100,10))
  )
  
  # If using rmse
  if(error.metric == 'rmse') {
    errors.touse <- list(rmse,mae,mse)
    
    # Use more nrounds  
    rounds.params <- makeParamSet(
      makeDiscreteParam('nrounds', values = seq(25,300,25))
    )
  }
  
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
    gamma=1,
    alpha=1,
    colsample_bytree = 0.7)
  
  
  ctrl <- makeTuneControlGrid(resolution=10)
  
  # Nested cross validation cause there's no such thing as overkill? 
  # Inner loop
  inner = makeResampleDesc("CV", iters = 4)
  
  inner.learner <- makeTuneWrapper(
    xgboost.lrn, 
    resampling = inner, 
    par.set = rounds.params, 
    control = ctrl, 
    measures = errors.touse,
    show.info = TRUE
  )
  
  outer.wrap <- makeResampleDesc("CV", iters = 10)
  
  # Tuning
  tune.someparams <- resample(
    inner.learner, 
    regression.task, 
    resampling = outer.wrap, 
    extract = getTuneResult, 
    measures = errors.touse,
    show.info = TRUE
  )
  
  # Process results
  tuning_results <- getNestedTuneResultsOptPathDf(tune.someparams)
  
  # Find best nrounds
  best_results <- tuning_results %>% 
    group_by(iter) %>%
    slice(which.min(mae.test.mean)) %>%
    # slice(which.min(rmse.test.rmse)) %>%
    as.data.frame
  
  # Determine average nrounds
  best.param <- floor(mean(as.numeric(as.character(best_results$nrounds))))
  # best.param <- as.numeric(as.character(best_results[which.min(best_results$mse.test.mean),1]))
  
  
  # Adjust learner
  xgboost.lrn <- setHyperPars(
    xgboost.lrn, 
    nrounds=best.param,
    eval_metric = error.metric
  )
  
  xgboost.model <- mlr::train(learner = xgboost.lrn, task = regression.task)
  
  test.pred <- predict(xgboost.model, newdata = test.data)
  train.pred <- predict(xgboost.model, task = regression.task)
  
  # 
  out.filename <- paste0(DATE , '_xgb_model_', target.year, '_', error.metric  , '.rda')
  saveRDS(xgboost.model, out.filename)
  
  list.toreturn <- list(xgboost.model, test.pred, train.pred)
  names(list.toreturn) <- c('model', 'test', 'train')
  # Return model and predictions 
  return(list.toreturn)
}

# Process the feature importance 
rank.features <- function(xgboost.model) {
  boop<-getFeatureImportance(xgboost.model)
  feature.iwant <- boop$res
  feature.iwant <- as.data.frame(t(feature.iwant))
  feature.iwant$feature <- rownames(feature.iwant)
  feature.iwant <- feature.iwant[order(feature.iwant$V1, decreasing=TRUE),]
  
  # Add ranks
  feature.iwant$rank <- 1:nrow(feature.iwant)
  
  # Return the ranked features 
  return(feature.iwant)
}


assess.model <- function(test.pred, train.pred) {
  test.r2 <- round(caret::R2(test.pred$data$response , test.pred$data$truth), 2)
  train.r2 <- round(caret::R2(train.pred$data$response , train.pred$data$truth), 2)
  test.rmse <- round(RMSE(test.pred$data$response , test.pred$data$truth), 2)
  train.rmse <- round(RMSE(train.pred$data$response , train.pred$data$truth), 2)
  plot(train.pred$data$truth, train.pred$data$response)
  plot(test.pred$data$truth, test.pred$data$response)
  
  r2.toprint <- paste0('R2 - TEST: ', test.r2, ' ; TRAIN: ', train.r2)
  rmse.toprint <- paste0('RMSE - TEST: ', test.rmse, ' ; TRAIN: ', train.rmse)
  print(r2.toprint)
  print(rmse.toprint)
}
