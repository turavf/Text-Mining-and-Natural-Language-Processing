
# 1. Setting working directory #######################################################################
setwd("~/Users/tura_ventola_franch/Documents/CaseI")
######################################################################################################

# 2. Data ############################################################################################
options(stringsAsFactors = FALSE) #text strings will not be factors of categories
Sys.setlocale('LC_ALL','C') 

#Reading the files with the data
Oct <- read.csv('Case/Case I/Data/A_Oct2019.csv')
Nov <- read.csv('Case/Case I/Data/B_Nov2019.csv')
Dec <- read.csv('Case/Case I/Data/C_Dec2019.csv')

sample_rows_Oct<-sample(1:nrow(Oct),size=0.02*nrow(Oct))
sample_Oct <- Oct[sample_rows_Oct, ]
sample_rows_Nov <-sample(1:nrow(Nov),size=0.02*nrow(Nov))
sample_Nov <- Nov[sample_rows_Nov, ]
sample_rows_Dec <-sample(1:nrow(Dec),size=0.02*nrow(Dec))
sample_Dec <- Dec[sample_rows_Dec, ]

#Binding the datasets into one dataset
data <- rbind(sample_Oct, sample_Nov, sample_Dec)
######################################################################################################

#3. Calling libraries ################################################################################
library(stringi)
library(stringr)
library(tm)
library(qdap)
library(ggplot2)
library(ggthemes)
library(ggdendro)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(pbapply)
######################################################################################################

#4. Popular Players ##################################################################################
#NIKE
LJ <- sum(grep("Lebron|James",data$text, ignore.case=TRUE))
KD <- sum(grep("Kevin|Durant",data$text, ignore.case=TRUE))
GA <- sum(grep("Giannis|Antetokounmpo",data$text, ignore.case=TRUE))
KL <- sum(grep("Kawhi|Leonard",data$text, ignore.case=TRUE))
JM <- sum(grep("Morant",data$text, ignore.case=TRUE))
PG <- sum(grep("Paul|George",data$text, ignore.case=TRUE))

# Organize term objects into a data frame
nike <- data.frame(terms = c('Lebron J.','Kawhi L.','Paul G.','Kevin D.','Giannis A.','Ja M.'),
                   freq  = c(LJ,KL,PG,KD,GA,JM))
#Examine
nike
# Plot
col<-brewer.pal(6,"Reds")
barplot(height= nike$freq, names=nike$terms,col=col)

#ADIDAS
SC <- sum(grep("Stephen|Curry",data$text, ignore.case=TRUE))
JH <- sum(grep("Harden",data$text, ignore.case=TRUE))
DL <- sum(grep("Damian|Lillard",data$text, ignore.case=TRUE))

# Organize term objects into a data frame
adidas <- data.frame(terms = c('James H.','Stephen C.','Damian L.'),
                     freq  = c(JH,SC,DL))
#Examine
adidas
# Plot
col2<-brewer.pal(3,"Blues")
barplot(height= adidas$freq, names=adidas$terms,col=col2)

#POTENTIAL PLAYERS
AD <- sum(grep("Anthony|Davis",data$text, ignore.case=TRUE))
JE <- sum(grep("Joel|Embiid",data$text, ignore.case=TRUE))
RW <- sum(grep("Russel|Westbrook",data$text, ignore.case=TRUE))

# Organize term objects into a data frame
pp <- data.frame(terms = c('Anthony D.','Russel W.','Joel E.'),
                 freq  = c(AD,RW,JE))
#Examine
pp
# Plot
col3<-brewer.pal(3,"Purples")
barplot(height= pp$freq, names=pp$terms,col=col3)
######################################################################################################

#4. Functions ########################################################################################
basicSubs <- function(x){
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', x)
  x <- gsub("[^\x01-\x7F]", "", x) #for emojis
  x <- tolower(x)
  return(x)
}

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
} #closing Tolower funcion

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
} #closing cleanCorpus funcion
#####################################################################################################

#5. DTM #############################################################################################
# Create custom stop words
stops <- c(stopwords('english'),"nba","win","basketball","season","playoffs","san","state",
           "game","live","will","team","kings","just","hong kong","rt","rankings","power","pts",
           "rating","leaders","highlights","league","vs","new","last","years","ago","today",
           "battle","rebounds","points")

# Make a volatile corpus
df <- data.frame(doc_id=row.names(data),
                 text=data$text)
txtCorpus <- VCorpus(DataframeSource(df))
# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Plain text cleaned copy
cleanText <- lapply(txtCorpus, content)
cleanText <- do.call(rbind, cleanText)

# Make uni-gram TDM
tweetTDM  <- TermDocumentMatrix(txtCorpus)
tweetTDMm <- as.matrix(tweetTDM)
######################################################################################################

#6. Fequency 1 Term ##################################################################################
# Get the most frequent terms
tweetSums <- rowSums(tweetTDMm)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)
# Which term is the most frequent?
idx <- which.max(tweetFreq$freq)
tweetFreq[idx, ]
# Remove the row attributes meta family
rownames(tweetFreq) <- NULL
# Simple barplot; values greater than 15
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 1000) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]
# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='deepskyblue3') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)
#####################################################################################################

#7. Bigrams #########################################################################################
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Make bi-gram TDM according to the tokenize control & convert it to matrix
txtTdm  <- TermDocumentMatrix(txtCorpus, 
                              control=list(tokenize=bigramTokens))
txtTdmM <- as.matrix(txtTdm)
#####################################################################################################

#8. Fequency 2 Terms #################################################################################
# Get the most frequent terms
topTerms <- rowSums(txtTdmM)
# Frequency Data Frame
tweetFreq <- data.frame(word=names(topTerms),frequency=topTerms)
# Which term is the most frequent?
idx <- which.max(tweetFreq$freq)
tweetFreq[idx, ]

# Remove the row attributes meta family
rownames(tweetFreq) <- NULL

# Simple barplot
topWords      <- subset(tweetFreq, tweetFreq$frequency >= 250) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='dodgerblue3') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)
#####################################################################################################

#9. Dendograms ######################################################################################
# Reduce TDM
reducedTDM <- removeSparseTerms(txtTdm, sparse=0.994)   

# Organize the smaller TDM
reducedTDM <- as.data.frame(as.matrix(reducedTDM))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedTDM))
plot(hc,yaxt='n')

ggdendrogram(hc, rotate=FALSE) 
######################################################################################################

#8. WordCloud ########################################################################################
tweetsTDMv <- sort(rowSums(txtTdmM), decreasing = TRUE)
tweetsDF   <- data.frame(word = names(tweetsTDMv), freq = tweetsTDMv)

#Simple WC
set.seed(1234)
pal1 <- brewer.pal(8, "RdBu")
wordcloud(tweetsDF$word,
          tweetsDF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal1,
          scale        = c(2,1))

# Regular dynamic WC
pal2 <- brewer.pal(10, "RdYlBu")
wordcloud2(tweetsDF[1:50,], 
           color = pal2, 
           backgroundColor = "white")
######################################################################################################
