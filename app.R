library(shiny)
library(shinythemes)
library(twitteR)
library(ROAuth)
library(RCurl)
library(stringr)
library(tmap)
library(tm)
library(plyr)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(bitops)
library(reshape2)
require(tibble)
library(dismo)
library(maps)
library(RgoogleMaps)
library(ggmap)
library(rsconnect)
# #################################################################
ui <-fluidPage(theme = shinytheme("readable"),
               
  headerPanel("Sentiment Analysis Using Twitter") , 
               
  sidebarPanel(
    textInput("name3", "Enter the Search Term",value = "marvel") ,
    sliderInput("samplesize","Number of Tweets", min=1,max=2000,value = 50),
    dateRangeInput('dateRange', label = 'Date range input: yyyy-mm-dd',
                   start = Sys.Date() - 5, end = Sys.Date()  ),
    actionButton(inputId = "start", label="Analyze!")
                 
  ),

    mainPanel(
      tabsetPanel(
        tabPanel("Sentiment Plot", plotOutput("sentiment1")),
        tabPanel("Line Plot", plotOutput("lineplot1")),
        tabPanel("Top Hashtags",  textInput("user", "Enter user name", "@marvel"), plotOutput("wordcloud2")),
        tabPanel("Word Cloud", plotOutput("wordcloud1")),
        tabPanel("User Statistics", plotOutput("hour"), HTML("Top 10 Tweeters of hashtag"), tableOutput("tweeterstable")),
        tabPanel("Map", HTML("Map is slow since searching 100K tweets"), HTML("<br>Map of Geocoded Tweets"), plotOutput("map"))
      )
    )

  )

# #################################################################
server <- function(input, output) {
  
  #TWITTER OAUTH ---------------------------------------------------
  consumerKey <- 'YOUR KEY HERE'
  consumerSecret <- 'YOUR KEY HERE'
  access_token <- 'YOUR KEY HERE'
  access_secret <- 'YOUR KEY HERE'
  accessURL="https://api.twitter.com/oauth/access_token"
  authURL="https://api.twitter.com/oauth/authorize"
  reqURL="https://api.twitter.com/oauth/request_token"
  
  authenticate <- OAuthFactory$new(consumerKey=consumerKey,
                           consumerSecret=consumerSecret,
                           requestURL=reqURL,
                           accessURL=accessURL,
                           authURL=authURL)
  
  setup_twitter_oauth(consumerKey,consumerSecret,access_token,access_secret)
  token <- get("oauth_token", twitteR:::oauth_cache) #Save the credentials info
  token$cache()
  save(authenticate, file="twitter authentication.Rdata")
  load("twitter authentication.Rdata")

  #CLEAN TWEETS FUNCTION ---------------------------------------------------
  cleantweets=function(tweetname){
    clean_tweet = gsub("&amp", "", tweetname)
    clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
    clean_tweet = gsub("@\\w+", "", clean_tweet)
    clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
    clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
    clean_tweet = gsub("http\\w+", "", clean_tweet)
    clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
    clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet)
    clean_tweet <- str_replace_all(clean_tweet," "," ")
    clean_tweet= gsub("http[^[:space:]]*", "", clean_tweet)
    clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
    clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
    clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")
    clean_tweet
  }
  
  #SENTIMENT BAR GRAPH---------------------------------------------------
  sentimentPlotFunc=function(name, tweet_txt) {
    
    mySentiment <- get_nrc_sentiment(tweet_txt)
    tweets <- cbind(cleantweets(tweet_txt), mySentiment)
    sentimentTotals <- data.frame(colSums(tweets[,c(2:11)]))
    names(sentimentTotals) <- "count"
    sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
    rownames(sentimentTotals) <- NULL
    
    gtitle1<-paste("Total sentiment score on", name, "Tweets")
    
    y = ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
      geom_bar(aes(fill = sentiment), stat = "identity") +
      theme(legend.position = "none") +
      xlab("Sentiment") + ylab("Total Count") + ggtitle(gtitle1) + coord_flip() +
      geom_text(aes(label=count))
    
    return(y)
  }
  
  #SENTIMENT LINE PLOT ---------------------------------------------------
  linePlotfunc=function(searchTerm, list) {
    df <- twListToDF(list)
    df <- df[, order(names(df))]
    df$created <- strftime(df$created, '%Y-%m-%d')
    if (file.exists(paste(searchTerm, '_stack.csv'))==FALSE) 
      write.csv(df, file=paste(searchTerm, '_stack.csv'), row.names=F)
   
    #Merge last access with cumulative file and remove duplicates
    stack <- read.csv(file=paste(searchTerm, '_stack.csv'))
    stack <- rbind(stack, df)
    stack <- subset(stack, !duplicated(stack$text))
    write.csv(stack, file=paste(searchTerm, '_stack.csv'), row.names=F)
    
    #Evaluation tweets function
    score.sentiment <- function(sentences, pos.words, neg.words, .progress='none') {
      library("plyr")
      library("stringr")
      scores <- laply(sentences, function(sentence, pos.words, neg.words){
        
        sentence <- iconv(sentence, "latin1", "ASCII//TRANSLIT")
        sentence <- iconv(sentence, to='ASCII//TRANSLIT')
        
        sentence <- gsub('[[:punct:]]', "", sentence)
        sentence <- gsub('[[:cntrl:]]', "", sentence)
        sentence <- gsub('\\d+', "", sentence)
        sentence <- tolower(sentence)
        word.list <- str_split(sentence, '\\s+')
        words <- unlist(word.list)
        pos.matches <- match(words, pos.words)
        neg.matches <- match(words, neg.words)
        pos.matches <- !is.na(pos.matches)
        neg.matches <- !is.na(neg.matches)
        score <- sum(pos.matches) - sum(neg.matches)
        return(score)
      }, pos.words, neg.words, .progress=.progress)
      scores.df <- data.frame(score=scores, text=sentences)
      return(scores.df)
      detach("package:plyr", unload = TRUE)
    }
    
    library("ggplot2")
    pos <- scan('http://ptrckprry.com/course/ssd/data/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
    neg <- scan('http://ptrckprry.com/course/ssd/data/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
    
    pos.words <- c(pos, 'nice', 'great','good')
    neg.words <- c(neg, 'wtf', 'bad', 'horrible', 'fail')
    Dataset <- stack
    Dataset$text <- as.factor(Dataset$text)
    scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
    write.csv(scores, file=paste(searchTerm, '_scores.csv'), row.names=TRUE) #save evaluation results into the file
    #total evaluation: positive / negative / neutral
    stat <- scores
    stat$created <- stack$created
    stat$created <- as.Date(stat$created)
    stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
    require("dplyr")
    by.tweet <- group_by(stat, tweet, created)
    by.tweet <- summarise(by.tweet, number=n())
    detach("package:dplyr", unload = TRUE)
    write.csv(by.tweet, file=paste(searchTerm, '_opin.csv'), row.names=TRUE)
    
      y = (ggplot(by.tweet, aes(created,number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
             geom_point(aes(group=tweet, color=tweet), size=4) +
             theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
             stat_summary(fun.y = 'sum', fun.ymin='sum', fun.ymax='sum', colour = 'black', size=2, geom = 'line') + 
             ggtitle(searchTerm) + 
             xlab("Date of Tweet") + 
             ylab("Sentiments")   
      )
    
    return(y)
  }
  
  #WORD CLOUD ------- ---------------------------------------------------
  wordcloudfunc=function(tweets.text){
    tweets.text <- iconv(tweets.text, "latin1", "ASCII//TRANSLIT")
    tweets.text <- iconv(tweets.text, to='ASCII//TRANSLIT')
    
    #Replace @UserName
    tweets.text <- gsub("@\\w+", "", tweets.text)
    #Remove links
    tweets.text <- gsub("http\\w+", "", tweets.text)
    
    #Create corpus
    tweets.text.corpus <- Corpus(VectorSource(tweets.text))
    #Clean up by removing stop words
    tweets.text.corpus <- tm_map(tweets.text.corpus, 
                                function(x)removeWords(x,stopwords()))

    y = wordcloud(tweets.text.corpus, 
                  min.freq = 2, scale=c(7,0.5),
                  colors=brewer.pal(8, "Dark2"),  
                  random.color= TRUE, 
                  random.order = FALSE, max.words = 150)
    return(y)
  }
  
  #TOP ASSOCIATED HASHTAGS  ---------------------------------------------------
  hashtagFunc=function(tw){
    text1<- tw$text
    extract.hashes = function(vec){
      hash.pattern = "#[[:alpha:]]+"
      have.hash = grep(x = vec, pattern = hash.pattern)
      
      hash.matches = gregexpr(pattern = hash.pattern, text = vec[have.hash])
      extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
      
      df = data.frame(table(tolower(unlist(extracted.hash))))
      colnames(df) = c("tag","freq")
      df = df[order(df$freq,decreasing = TRUE),]
      return(df)
    }
    
    temp1<- head(extract.hashes(text1),30)
    temp2<- transform(temp1,tag = reorder(tag,freq))
    
    y<- ggplot(temp2, aes(x = tag, y = freq)) + geom_bar(stat="identity", fill = "red") +
      coord_flip() +
      ggtitle("Top hashtags associated with Brand") +
      xlab("Frequency") +
      ylab("Hashtag")
    
    return(y)
    
  }
  
  #TWEETS PER HOUR ---------------------------------------------------------
  countFunc=function(tweets2df) {
    count <- function(x){
      two <- vector(mode = "numeric",length=0)
      for (i in (1:length(x))){
        first = strsplit(as.character(x[i]),' ')
        first = first[[1]][2]
        first = strsplit(first,":")
        two = c(two,first[[1]][1])
      }
      return(two)
    }
    hour1 <-count(tweets2df$created)
    tweets2df$hour1 <- hour1
    y <- ggplot(tweets2df,aes(x=hour1))+geom_bar(aes(y=(..count..))) +
      ggtitle("Tweets per Hour") + xlab("Hour") + ylab("Count")
    return(y)
  }
  
  #TOP TWEETERS OF HASHTAGS  ---------------------------------------------------------
  toptweeters<-function(tweetlist) {
    tweets <- unique(tweetlist)
    # Make a table of the number of tweets per user
    d <- as.data.frame(table(tweets$screenName)) 
    #descending order according to frequency
    d <- d[order(d$Freq, decreasing=T), ]
    names(d) <- c("User","Tweets")
    return (d)
  }

  observeEvent(input$start,{
    
    #GET AND CLEAN TWEETS --> PASS IN AS PARAMETERS TO MAKE PLOTS/WORD CLOUD
    tweet1 <- searchTwitter(input$name3, n=input$samplesize, lang='en', since=input$date, until=input$date2)
    tweets2 <- userTimeline("Marvel", n =input$samplesize)

    tweet1_txt <- sapply(tweet1, function(x) x$getText())
    tweet1_txt=cleantweets(tweet1_txt)
    
    tweets1df <- twListToDF(tweet1)
    tweets2df <- twListToDF(tweets2)
    tweets2df[100, c("id", "created", "screenName", "replyToSN","favoriteCount", "retweetCount", "longitude", "latitude", "text")]
    tweets1df[100, c("id", "created", "screenName", "replyToSN","favoriteCount", "retweetCount", "longitude", "latitude", "text")]
    
    #Sentiment plot
    output$sentiment1 <-renderPlot(sentimentPlotFunc(input$name3, tweet1_txt))
    
    #Line Plot
    output$lineplot1 <-renderPlot(linePlotfunc(input$name3, tweet1))
    
    #Top hashtags
    output$wordcloud2 <-renderPlot(hashtagFunc(tweets2df))
    
    #Word Cloud
    output$wordcloud1 <-renderPlot(wordcloudfunc(tweet1_txt))
    
    #Tweets per Hour
    output$hour <- renderPlot(countFunc(tweets2df))
    
    #Top 20 tweeters of hashtags
    d<-reactive({d<-toptweeters(tweets1df)})
    output$tweeterstable<-renderTable(head(d(),10))
    
    #Followers Stats - map, when tweeting 
    output$map <-renderPlot({
      map('world')
      test <- searchTwitter('marvel', n= 6000)
      # convert tweets to a data frame
      testdf <- twListToDF(test)
      # tweet #100
      testdf[100, c("id", "created", "screenName", "replyToSN","favoriteCount", "retweetCount", "longitude", "latitude", "text")]
      
      points(testdf$longitude,testdf$latitude,pch=20, cex =2, col="red")
    })
    
    })

}

shinyApp(ui = ui, server = server)
