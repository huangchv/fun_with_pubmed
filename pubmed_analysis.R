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

# Wordcloud
library(wordcloud)
library(RColorBrewer)

######## Load data #########
set.seed(1234)
j_data_all <- readRDS("2017-09-09_2010-2015_abstracts.rda")

journal_if <- read.delim("journal_impact_factors.tsv", check.names=FALSE, sep = "\t", stringsAsFactors = FALSE)
journal_if$lower_title <- tolower(journal_if$`Full Journal Title`)

journal_if$`Journal Impact Factor` <- as.numeric(journal_if$`Journal Impact Factor`)

# Append journal impact factors and eigenwhat not 
all.journals <- data.frame(journal = as.character(j_data_all$journal), impact = NA)

all.journals$trimmed <- tolower(gsub(" \\([^\\]]*\\)", "", all.journals$journal, perl=TRUE));
all.journals$trimmed <- gsub("&amp;", "&", all.journals$trimmed, perl=TRUE)
all.journals$trimmed <- gsub("^the ", "", all.journals$trimmed, perl=TRUE)
#all.journals$trimmed <- gsub(" \\:[^\\]]*$", "", all.journals$trimmed, perl=TRUE)
all.journals$trimmed <- gsub(" \\:.*", "", all.journals$trimmed, perl=TRUE)
all.journals$trimmed <- gsub("\\:.*", "", all.journals$trimmed, perl=TRUE)
all.journals$trimmed <- gsub("physiology. ", "physiology-", all.journals$trimmed, perl=TRUE)
all.journals$trimmed <- gsub("reviews. ", "reviews ", all.journals$trimmed, perl=TRUE)
all.journals$trimmed <- gsub("lancet. ", "lancet ", all.journals$trimmed, perl=TRUE)

all.journals$trimmed <- gsub("\\. ", " ", all.journals$trimmed, perl=TRUE)

poop<-all.journals$trimmed %in% journal_if$lower_title
all.journals$impact <- journal_if[match(all.journals$trimmed, journal_if$lower_title), 5]
all.journals$eigen <- journal_if[match(all.journals$trimmed, journal_if$lower_title), 6]

# Fill in missing values with random low values
all.journals$impact[is.na(all.journals$impact)] <- rnorm(1, mean = 2.5, sd = 0.83)
all.journals$eigen[is.na(all.journals$eigen)] <- rnorm(1, mean = 0.002120, sd = 0.0007066667)

# Push the impact and eigen values back in 
j_data_all$impact_factor <-  all.journals$impact
j_data_all$eigenfactor <-  all.journals$eigen

#saveRDS(j_data_all, '2017-09-09_2010-2015_abstracts_with_if.rda')
####### tf-idf #########

txt.process <- Corpus(VectorSource(j_data_all$abstract))
txt.process <- tm_map(txt.process, stripWhitespace)
txt.process <- tm_map(txt.process, removePunctuation, preserve_intra_word_dashes = TRUE)
txt.process <- tm_map(txt.process, content_transformer(tolower))

txt.process <- tm_map(txt.process, removeWords, stopwords("english"))
txt.process <- tm_map(txt.process, stemDocument, language="english")
txt.process <- tm_map(txt.process, removeNumbers)

dtm <- DocumentTermMatrix(txt.process, control = list(weighting = weightTfIdf))
dtm <- removeSparseTerms(dtm, 0.95)

# Now do the same for the title
txt.process <- Corpus(VectorSource(j_data_all$title))
txt.process <- tm_map(txt.process, stripWhitespace)
txt.process <- tm_map(txt.process, removePunctuation, preserve_intra_word_dashes = TRUE)
txt.process <- tm_map(txt.process, content_transformer(tolower))

txt.process <- tm_map(txt.process, removeWords, stopwords("english"))
txt.process <- tm_map(txt.process, stemDocument, language="english")
txt.process <- tm_map(txt.process, removeNumbers)

title.dtm <- DocumentTermMatrix(txt.process, control = list(weighting = weightTfIdf))
title.dtm <- removeSparseTerms(title.dtm, 0.99)

##
# For funsies, word cloud of stopwords and why we use it 

txt.process <- Corpus(VectorSource(j_data_all$abstract[1:500]))
txt.process <- tm_map(txt.process, stripWhitespace)
txt.process <- tm_map(txt.process, removePunctuation, preserve_intra_word_dashes = TRUE)
txt.process <- tm_map(txt.process, content_transformer(tolower))
txt.process <- tm_map(txt.process, removeNumbers)

stopwords.tm <- DocumentTermMatrix(txt.process)
stopwords.df <- data.frame(as.matrix(stopwords.tm))

words.tokeep <- colnames(stopwords.df) %in% stopwords('english')
toplot.stop <- stopwords.df[,words.tokeep]
toplot.stop.mean <- apply(toplot.stop,2,mean)
toplot.stop <- data.frame(word=names(toplot.stop.mean), freq=toplot.stop.mean)

wordcloud(words = toplot.stop$word, 
  freq = toplot.stop$freq,
  scale = c(6,1.5),
  min.freq = 0.001,
  max.words=200, random.order=FALSE, rot.per=0.35, 
  colors=brewer.pal(8, "Dark2")
  )
##
# modify for parsing purposes
title.dtm.matrix <- as.matrix(title.dtm)
colnames(title.dtm.matrix) <- paste0('title_', colnames(title.dtm.matrix))

dtm.matrix <- as.matrix(dtm)
colnames(dtm.matrix) <- paste0('abstract_', colnames(dtm.matrix))

all.data <- cbind(j_data_all[,-2], dtm.matrix, title.dtm.matrix)

#
#saveRDS(all.data, '2017-09-09_pubmed_processed_data.rda')
all.data <- readRDS('2017-09-09_pubmed_processed_data.rda')

########## regression ##########
omit.cols <- c(1,2)
train.data <- all.data[,-omit.cols]

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
