#### Libraries ####
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(BoutrosLab.plotting.general)
library(reshape2)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(tokenizers)

#### Load data ####
j_data_all <- readRDS('data/2017-09-09_2010-2015_abstracts_with_if.rda')

#### Summary stats ####
j_data <- j_data_all[,-c(1,2)]

# Make a quick lookup table I can use for quick joins
if_lookup <- j_data[,c(1,17,18)]
if_lookup <- (unique(if_lookup))

# Known journals
target.journals <- c(
  'Cell', 'The New England journal of medicine', 'Nature',
  'Nature genetics', 'Nature communications',
  'Science (New York, N.Y.)', 'BMC bioinformatics', 'Bioinformatics (Oxford, England)'
  )
target.journals.names <- c(
  'Cell', 'NEJM', 'Nature',
  'Nat Gen', 'Nat Comm',
  'Science', 'BMC Bioinfo',
  'Bioinformatics'
)

# Most frequent journal
journal_counts <- j_data %>% 
  group_by(journal) %>%
  summarize(n = n(), mean_cites = mean(citations), min_cites = min(citations), max_cites = max(citations)) %>%
  arrange(desc(n))

# Add in IF 
journal_counts <- left_join(journal_counts, if_lookup, by='journal')

# Grab the first 10, then add in targets
jcounts.toplot <- rbind(journal_counts[1:10,], journal_counts[journal_counts$journal %in% target.journals,])
jcounts.toplot$order <- nrow(jcounts.toplot):1
j.names <- jcounts.toplot[,1]
j.names$journal[2]  <- 'Physical Review. E - SNSMP'
j.names$journal[3]  <- 'PNAS'
j.names$journal[9] <- 'Chemical communications'
j.names$journal[12] <- 'Science'
j.names$journal[15] <- 'Bioinformatics'
j.names$journal[17] <- 'NEJM'
j.names$journal <- gsub('Journal' ,'J.', j.names$journal)
j.names$journal <- gsub('The ' ,'', j.names$journal)
j.names$journal <- gsub('the ' ,'', j.names$journal)
j.names$journal <- gsub('of ' ,'', j.names$journal)

# colours 
bar.cols <- c(rep('darkorange', 10), rep('dodgerblue', 8))
jcounts.toplot$percent <- jcounts.toplot$n / 29930 * 100

#
count.barplot <- create.barplot(
  formula = order ~ percent ,
  data = jcounts.toplot,
  plot.horizontal = TRUE,
  ylab.label = '',
  yaxis.lab = rev(j.names$journal),
  xlab.label = '', 
  col = rev(bar.cols)
  )

impact.barplot <- create.barplot(
  formula = order ~ impact_factor ,
  data = jcounts.toplot,
  plot.horizontal = TRUE,
  ylab.label = '',
  yaxis.lab = rev(j.names$journal),
  xlab.label = '', 
  col = rev(bar.cols),
  style = 'Nature'
)

citation.seg <- create.segplot(
  #
  formula = order ~ min_cites + max_cites,
  data = jcounts.toplot,
  xlab.label = 'Citation Dist',
  ylab.label = '',
  yaxis.lab = rev(j.names$journal),
  xaxis.cex = 1.5,
  yaxis.cex = 1.5,
  xlab.cex = 1.5,
  ylab.cex = 1.5,
  yat = 1:18,
  draw.bands = FALSE,
  segments.col = (bar.cols),
  segments.lwd = 1.5,
  # Adding center values
  centers = jcounts.toplot$mean_cites,
  styl = 'Nature'
  )

# Multiplot 
create.multiplot(
  #
  filename = generate.filename('popular', 'journals','png'),
  width = 7,
  height = 4,
  plot.objects = list(count.barplot, citation.seg, impact.barplot),
  plot.layout = c(3,1),
  xlab.label = c("% Dataset", "Citations", 'Impact Factor'),
  xlimits = list(c(0,3.2), c(0,450), NULL),
  x.spacing = c(0.2,0.2),
  xat = list(0:3, TRUE, TRUE), 
  # The plotting function throws an error if this is not included
  ylab.label = c(""),
  ylab.padding = 7,
  # Parameters set in the multiplot will override settings in individual plots
  xaxis.cex = 1,
  yaxis.cex = 1,
  xlab.cex = 1.25,
  ylab.cex = 1.25,
  xlab.to.xaxis.padding = -0, 
  yaxis.labels = rev(j.names$journal),
  left.padding = -5,
  style = 'Nature'
  )

##### Number per months #########
months_counts <- j_data %>% 
  group_by(month_sub) %>%
  summarize(n = n(), mean_cites = mean(citations), min_cites = min(citations), max_cites = max(citations)) %>%
  arrange(month_sub)

months.toplot <- j_data[!is.na(j_data$month_sub), c(2,4)]

months.toplot$log_cit <- log2(months.toplot$citations)
months.toplot$log_cit[(is.infinite(months.toplot$log_cit ))] <- -1

months.col <- c('skyblue','skyblue',
                'chartreuse2','chartreuse2','chartreuse2',
                'gold','gold','gold',
                'darkorange2','darkorange2','darkorange2','skyblue')
months.barplot <- create.barplot(
  formula = n ~ month_sub ,
  data = months_counts[1:12,],
  plot.horizontal = FALSE,
  ylab.label = '',
  xlab.label = '',
  col = months.col,
  style = 'Nature'
)

# Violin plot 
cites.month.violin <- create.violinplot(
  formula = log_cit ~ as.factor(month_sub),
  data = months.toplot,
  col = months.col,
  ylimits = c(-1.5, 9),
  scale=TRUE
  )


create.multiplot(
  #
  plot.objects = list(months.barplot, cites.month.violin),
  plot.layout = c(1,2),
  xlab.label = c("Months"),
  y.spacing = 0.5,
  filename = generate.filename('papers', 'months','png'),
  width = 6,
  height = 5,
  # The plotting function throws an error if this is not included
  ylab.label = c('Citations', "Submissions"),
  ylab.padding = 7,
  # Parameters set in the multiplot will override settings in individual plots
  xaxis.cex = 1,
  yaxis.cex = 1,
  ylab.cex = 1.5,
  xlab.cex = 1.5,
  yaxis.labels = list(c(0,1000,2000,3000), c(0,1,16,256)),
  yat = list(c(0,1000,2000,3000), c(-1,0,4,8)),
  ylimits = list(c(-50,3050), c(-2,9)),
  #left.padding = -5,
  style = 'Nature'
)


####### specific journals over time ########

months_journal_counts <- j_data %>% 
  group_by(month_sub,journal) %>%
  summarize(n = n(), mean_cites = mean(citations), min_cites = min(citations), max_cites = max(citations)) %>%
  filter(journal %in% target.journals)

months_journals_temp <- dcast(data = months_journal_counts, formula = journal ~ month_sub, fill=0,value.var = 'n')
months_journals_temp$'12'<-0

months_journals_toplot <- months_journals_temp[,-13]
colnames(months_journals_toplot)[2:13] <- month.abb[1:12]
months_journals_toplot$names <- target.journals.names[c(8,7,1,3,5,4,6,2)]

mj_toplot <- melt(months_journals_toplot)

create.scatterplot(
  #
  formula = value ~ variable | names,
  layout = c(2,4),
  filename = generate.filename('journals', 'months','png'),
  width = 8,
  height = 5,
  data = mj_toplot,
  groups = names,
  col = default.colours(8),
  type = 'o',
  lwd = 2,
  cex = 1,
  ylimits = c(-0.5, 14),
  xaxis.lab = 1:12,
  xlab.label = '',
  ylab.label = 'Submissions',
  ylab.cex = 1.5,
  yaxis.cex = 1,
  xaxis.cex = 1,
  )

###### Distribution of impact factors ########
create.densityplot(
  #
  x = as.data.frame(if_lookup$impact_factor),
  filename = generate.filename('if_all', 'density','png'),
  width = 6,
  height = 3,
  xlab.label = 'Impact Factor',
  style = 'Nature',
  xlab.cex = 1.5,
  ylab.cex = 1.5,
  xat = seq(0,80,10),
  xlimits = c(-2,75),
  ylimits = c(-0.05, 1.2),
  lwd = 3
  )

create.densityplot(
  x = as.data.frame(if_lookup$impact_factor),
  filename = generate.filename('if_ss', 'density','png'),
  width = 6,
  height = 3,
  xlab.label = 'Impact Factor',
  style = 'Nature',
  xlab.cex = 1.5,
  ylab.cex = 1.5,
  xat = 0:5,
  xlimits = c(-0.1,5),
  ylimits = c(-0.05, 1.2),
  lwd = 3
  )
###### Distribution of citation numbers ######

citations.ecdf <- ecdf(j_data$citations)
citations.cdf <- data.frame(citations = 0:150, cdf = citations.ecdf(0:150))

create.scatterplot(
  formula = cdf ~ citations,
  data = citations.cdf,
  filename = generate.filename('citations_', 'cdf','png'),
  width = 6,
  height = 3,
  type = c('l','g'),
  style = 'Nature',
  xlimits = c(-5, 150),
  xat = seq(0,200,25),
  ylimits = c(-0.05, 1.05),
  xlab.label = 'Citations',
  ylab.label = 'Cumulative Dist.',
  xlab.cex = 1.5,
  ylab.cex = 1.5,
  xaxis.cex = 1.25,
  yaxis.cex = 1.25,
  #abline.v = c(0),
  #abline.h = 1,
  abline.col = 'grey75',
  abline.lwd = 2.5,
  lwd = 2.5
  )

create.scatterplot(
  formula = cdf ~ citations,
  data = citations.cdf[1:20,],
  filename = generate.filename('citations_ss', 'cdf','png'),
  width = 6,
  height = 3,
  type = c('s','g'),
  style = 'Nature',
  xlimits = c(-0.25, 15),
  ylimits = c(-0.05, 1.05),
  xat = 0:15,
  xgrid.at = seq(0,20,5),
  ygrid.at = seq(0,1,0.2),
  xlab.label = 'Citations',
  ylab.label = 'Cumulative Dist.',
  xlab.cex = 1.5,
  ylab.cex = 1.5,
  xaxis.cex = 1.25,
  yaxis.cex = 1.25,
  lwd = 2.5
)

create.densityplot(
  x = as.data.frame(j_data$citations),
  filename = generate.filename('citations_', 'density','png'),
  width = 6,
  height = 3,
  xlab.label = 'Citations',
  style = 'Nature',
  xlab.cex = 1.5,
  ylab.cex = 1.5,
  #xat = 0:5,
  #xlimits = c(-0.1,5),
  ylimits = c(-0.01, 0.42),
  lwd = 3
  )

create.densityplot(
  x = as.data.frame(j_data$citations),
  filename = generate.filename('citations_ss', 'density','png'),
  width = 6,
  height = 3,
  xlab.label = 'Citations',
  style = 'Nature',
  xlab.cex = 1.5,
  ylab.cex = 1.5,
  xat = 0:5,
  xlimits = c(-0.1,5),
  ylimits = c(-0.01, 0.42),
  lwd = 3
)

###### Word cloud of sentiments ##### 
# Grab first 500 abstracts 
sentiments_set <- tolower(paste(j_data_all$abstract[1:1000], collapse = ' '))

test <- tokenize_words(sentiments_set)
test2 <- table(test)
words <- data_frame(word = names(test2), count = as.numeric(test2))
words <- words[order(words$count, decreasing = T),]

words <- words %>% 
  filter(!word %in% stop_words$word)
nrc_dict <- get_sentiments('nrc')

words.toplot <- left_join(words, get_sentiments('nrc'), by='word' ) %>%
  filter(!is.na(sentiment)) %>%
  filter(sentiment == 'anticipation' | sentiment == 'anger') %>%
  acast(word ~ sentiment, value.var = 'count', fill=0) 

colnames(words.toplot) <- c('Anger', 'Anticipation')

png(filename = "anger_anticipation_cloud.png",width = 6, height = 4,units = 'in',res = 300)
par(mfrow=c(1,2))
wordcloud(rownames(words.toplot), words.toplot[,1], min.freq =3, scale=c(4, 0.8), random.order = FALSE, random.color = FALSE, fixed.asp = TRUE,  colors= c("darkorange1","darkorange2","darkorange3","darkorange4"))
text(0.5, 1.06, 'Anger', cex = 2)
wordcloud(rownames(words.toplot), words.toplot[,2], min.freq =3, scale=c(4, 0.8), random.order = FALSE, random.color = FALSE, colors= c("dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4"))
text(0.5, 1.06, 'Anticipation', cex= 2)
dev.off()