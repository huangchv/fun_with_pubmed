library(mxnet)
library(stringi)

######## Load data #########
set.seed(1234)

# Read File in 
#write(j_data_all$abstract, 'all_abstracts.txt')
test_abstracts <- scan('all_abstracts.txt', what = 'character', encoding = 'ASCII',  sep = '\n')

head(test_abstracts)
test.vector <- test_abstracts
# j_data_all <- readRDS("2017-08-27_2010-2015_abstracts.rda")
source('text_rnn_functions.R')

seq.len = 64
batch.size = 128

# Check encoding
#text.vector <- j_data_all$abstract
#text.vector <- enc2utf8(text.vector)
#Encoding(text.vector)

#cat(text.vector[911])
#text.vector <- stri_trans_general(text.vector, id = "Any-Latin")
#text.vector <- enc2utf8(text.vector)

#text.vector <- stri_encode(text.vector, "", "UTF-8") # re-mark encodings

ret <- convert.data(test.vector, seq.len=seq.len)

X <- ret$data
dic <- ret$dic
lookup.table <- ret$lookup.table

# sanity checks
paste0(lookup.table[X[,1]], collapse = '')
paste0(lookup.table[X[,2]], collapse = '')





vocab <- length(dic)

shape <- dim(X)
train.val.fraction <- 0.9
size <- shape[2]

X.train.data <- X[, 1:as.integer(size * train.val.fraction)]
X.val.data <- X[, -(1:as.integer(size * train.val.fraction))]
X.train.data <- drop.tail(X.train.data, batch.size)
X.val.data <- drop.tail(X.val.data, batch.size)

X.train.label <- get.label(X.train.data)
X.val.label <- get.label(X.val.data)

X.train <- list(data=X.train.data, label=X.train.label)
X.val <- list(data=X.val.data, label=X.val.label)



num.hidden = 256
num.embed = 256
num.lstm.layer = 3
num.round = 5
learning.rate= 0.00001
wd=0.00001
clip_gradient=1
update.period = 1


model.gpu <- mx.lstm(X.train, X.val,
  dropout = 0.3,
  ctx=mx.gpu(),
  num.round=num.round,
  update.period=update.period,
  num.lstm.layer=num.lstm.layer,
  seq.len=seq.len,
  num.hidden=num.hidden,
  num.embed=num.embed,
  num.label=vocab,
  batch.size=batch.size,
  input.size=vocab,
  initializer=mx.init.uniform(0.1),
  learning.rate=learning.rate,
  wd=wd,
  clip_gradient=clip_gradient,
  epoch.end.callback=mx.callback.save.checkpoint("rnn_abstracts_round_end"),
  batch.end.callback=mx.callback.save.checkpoint("rnn_abstracts_checkpoints", 1000)
  )

