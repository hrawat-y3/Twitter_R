library(twitteR)
library(RCurl)
library(tm)
library(wordcloud)
library(SnowballC)

API_key = 'pC8dtBCJuqFrmqPmBjdsMmUkl'

API_secret_key = 'hqSTNGuFXQ1yXyczZE0SjHKUWm3TymLbB0hV8kUp0yslzcWPyS'

Access_Token = '1355942621747703811-LHitrAAbvwuKZBkdSN2ESzwRxowWCP'

Access_Token_Secret = 'HAQSdsioX5Fzt8SUS7PQsAXQahbWOQKvS7k94qG35asng'

setup_twitter_oauth(API_key, API_secret_key, Access_Token, Access_Token_Secret) #handshake-function

#send = updateStatus("Hello, how is everyone doing?")

#deleteStatus(send)

#send1 = updateStatus("coranago-gocorona", mediaPath = '/Users/bhagatsingh/Desktop/twitter/c.jpeg')

#deleteStatus(hist_im)

#t = paste("hello! sent at:", time , "This is a histogram of 
          #Sepal Length of iris flowers color coded according to their species")

#tw <- updateStatus(t, mediaPath = '/Users/bhagatsingh/Desktop/twitter/c.jpeg')

#data = datasets::iris
#library(ggplot2)




#time = Sys.time()
#t = paste("hello! sent at:", time , "This is a histogram of 
          #Sepal Length of iris flowers color coded according to their species")

#jpeg("r.jpg")
#plott = ggplot(data)+
  #geom_histogram(mapping = aes(Sepal.Length, fill = Species), bins=10 , alpha = 0.7)+
  #ggtitle('Histogram of Sepal Lenght of Iris Species')+
  #xlab('Sepal Length')
#p
#dev.off()

#hist_im = updateStatus(t, mediaPath = "r.jpg")

#z = getwd(plott)

mytweets = searchTwitter("bitcoin", n=200, lang = "en")

tweet_df = do.call("rbind", lapply(mytweets, as.data.frame))

tweet_df$text = sapply(tweet_df$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))

tweets = tweet_df$text

#tweets = str_replace_all(tweets, "[\n\n]", "")

corpus = Corpus(VectorSource(tweets))  # inside tm pkg

corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeNumbers)

corpus = tm_map(corpus, removeWords, c(stopwords("en")))

corpus = tm_map(corpus, removeWords, c("rt",'\\n\\"','me\\",'))

corpus = Corpus(VectorSource(corpus))

tdm = TermDocumentMatrix(corpus) #from tm

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
