#tm - R package that provides predefined sources like VectorSource 
library("tm")
#SnowballC- R package that contain Stemming Algorithms
library("SnowballC")
#wordcloud - R package which is used for generating word cloud
library("wordcloud")
#RColorBrewer-R package that contains a ready-to-use color palettes for creating beautiful graphics
library("RColorBrewer")
library("ggplot2")
#syuzhet - R library used for sentimental analysis.
library("syuzhet")

text <- readLines(file.choose())

docs <- Corpus(VectorSource(text))

inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs
inspect(docs)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, c("the", "and","are","have","that","this","also","for","etc","other","off","too","its","because","list","dont","does","far","how","much","turn","using","could","from","only","day","you","our","not","she","its","get","having","purchase","but","her","when","some","been","now","out","can","they","any","some","getting","has","here","than","all","will","there","more","just","about","very","was","had","once","were","which","media","omitted","joined","who","those","your","what","them","want","used","tell","thing","with","still","would","link")) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)
#D1 = "I like databases"
#D2 = "I hate databases",
#then the document-term matrix would be:
  
#  I	like	hate	databases
#D1	1	1	0	1
#D2	1	0	1	1

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,100)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 37,max.words=100, random.order=FALSE, rot.per=0.20, colors=brewer.pal(8, "Dark2"))

#fetch sentiment words from texts
#The get_nrc_sentiment implements NRC Emotion lexicon. "the NRC emotion lexicon is a list of words and their associations with eight emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive)"
Sentiment <- get_nrc_sentiment(text)
head(Sentiment)
text1 <- cbind(text,Sentiment)

#count the sentiment words by category
TotalSentiment <- data.frame(colSums(text1[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL
ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
    theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score")
