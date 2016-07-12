library(wordcloud)
library(twitteR)
library(SocialMediaLab)
library(RTextTools)
source("helpers.R")
library(tm)
#get your keys from twitter developer page and enter the data
AuthenticateWithTwitterAPI(api_key=consumer_key, 
                           api_secret=consumer_secret,
                           access_token=access_token, 
                           access_token_secret=access_secret)
tweets=c()
EstoniantweetsDf=data.frame(NULL)
maksID=NULL
for(i in 1:10) {
  tweets=searchTwitteR("estonia", resultType="recent", n = 1500, 
                          lang = "en", maxID = maksID)
  EstoniantweetsDf=rbind(EstoniantweetsDf,twListToDF(tweets))
  maksID=EstoniantweetsDf$id[nrow(EstoniantweetsDf)]
  print(i)
}
saveRDS(EstoniantweetsDf, "EstoniaTweets.RDS")
tweets=readRDS("EstoniaTweets.RDS")
#dtm
EstoniantweetClean=cleanTweet(EstoniantweetsDf$text)
#make corpus
EstoniantweetCorpus <- VCorpus(VectorSource(EstoniantweetClean))
#make dtm
dtmEstoniaTweet <- DocumentTermMatrix(EstoniantweetCorpus,
                                   control = list(stemming = TRUE,
                                                  stopwords=T,
                                                  removeNumbers = TRUE, 
                                                  removePunctuation = TRUE, 
                                                  wordLengths = c(3, 140)))
#NYT data, getc key from NYT developer page
apikey='your_key'
save(apikey, file="NYTkey.RData")
EstoniaNYT=getMetaData(apikey = apikey, nrOfArticles = 2000, 
                       fq='"Estonia"', beginDate = "20160711",dayStep = 90 )
#save
saveRDS(EstoniaNYT, "EstoniaNYT.RDS")
#get body
NYTBody=getArticleBody(articleUrls = EstoniaNYT$urls)
EstoniaNYT$body=NYTBody
saveRDS(EstoniaNYT, "EstoniaNYT.RDS")
load("EstoniaNYT.RDS")
EstoniaNYT$bodyTitle=paste(EstoniaNYT$titles, EstoniaNYT$body)
EstoniaNYT$bodyTitle=gsub(" NA$","",EstoniaNYT$bodyTitle)
NYTClean=cleanTweet(EstoniaNYT$bodyTitle[!is.na(EstoniaNYT$titles)])
#make corpus
NYTCorpus <- VCorpus(VectorSource(NYTClean))
##make dtm
dtmEstoniaNYT <- DocumentTermMatrix(NYTCorpus,
                                 control = list(stemming = TRUE,
                                                stopwords=T,
                                                removeNumbers = TRUE, 
                                                removePunctuation = TRUE, 
                                                wordLengths = c(2, 140)))
#compare corpora
dtm.wordcloud(dtmEstoniaNYT,pal = brewer.pal(6, "Dark2"))
dtm.wordcloud(dtmEstoniaTweet, pal = brewer.pal(6, "Dark2"))

library(corpustools)
cmp = corpora.compare(dtmEstoniaNYT, dtmEstoniaTweet)
cmp = arrange(cmp, -chi)
#wordcloud
with(head(cmp, 100),plotWords(x=log(over), words = term, wordfreq = chi, 
                              random.y = T))
#topic modelling
mNYT=lda.fit(dtmEstoniaNYT, K=5, alpha=.1)
#the bigger the alpha the fuzzier topics can be
terms(mNYT, 10)
#tweets topics
mtweets=lda.fit(dtmEstoniaTweet, K=4, alpha=.1)
terms(mtweets, 10)
