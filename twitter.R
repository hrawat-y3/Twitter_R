library(twitteR)
library(RCurl)
library(tm)
library(wordcloud)
library(SnowballC)

API_key = ''

API_secret_key = ''

Access_Token = ''

Access_Token_Secret = ''

setup_twitter_oauth(API_key, API_secret_key, Access_Token, Access_Token_Secret) #handshake-function

mytweets = searchTwitter("AI", n=400, lang = "en")

tweet_df = do.call("rbind", lapply(mytweets, as.data.frame))

tweet_df$text = sapply(tweet_df$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))

tweets = tweet_df$text

corpus = Corpus(VectorSource(tweets))  

corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeNumbers)

corpus = tm_map(corpus, removeWords, c(stopwords("en")))

corpus = tm_map(corpus, removeWords, c("rt",'\\n\\"','me\\",'))

corpus = Corpus(VectorSource(corpus))

tdm = TermDocumentMatrix(corpus) 

tdm = as.matrix(tdm)

tdm = sort(rowSums(tdm), decreasing = TRUE)

tdm = data.frame(word = names(tdm), freq = tdm)

head(tdm,n=20)

wordcloud(words = tdm$word, freq = tdm$freq, min.freq = 1, 
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(10,"Dark2"))

barplot(tdm[1:10,]$freq, las = 2, names.arg = tdm[1:10,]$word, 
        col = "lightpink", main = "MOST FREQUENT WORD", 
         ylab = "FREQUENCY")
