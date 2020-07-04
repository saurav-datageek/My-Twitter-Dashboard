#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(highcharter)
library(plotly)
library(rtweet)
library(syuzhet)
library(tidytext)
library(stringr)
library(wordcloud) 
library(DT)
library(tokenizers)
library(shinycssloaders)
library(tm)
library(htmltools)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ### This tokens are obtained after making a Twitter Developer Account. Which is very easy to make following some Google Search steps. 
  
  consumer_key = "7o0tvmH08Crv81aJa0Pz9XVnD" #Consumer key from twitter app
  consumer_secret = "BqcbxByLHolAhHxhbTeUCRUgXnNqfrgvzpPKbDIJzdTqAjwd0Z" #Consumer secret from twitter app
  access_token = "998921185273438213-TG6v3EQu4OSE8ydeXkg0xYZSPWPYQzj" #access token from twitter app
  access_secret ="2HE4tOZxaZLMYTLoZlpYRo6fkHzYbloWL1iTpKQTNY08d" #access secret from twitter app
  
 
  
  reactive_head <- eventReactive(input$go, {
    
    validate(
      need(input$word != "", "Please enter the Word/User")
    )
    
    
    twitter_token <- create_token(
      app = "mytwitterapp",
      consumer_key = consumer_key,
      consumer_secret = consumer_secret,
      access_token = access_token,
      access_secret = access_secret,
      set_renv = TRUE)
    
    ### General Part 
    
    if(input$ModeChoice=="Word"){
      my_tweets <- search_tweets(input$word, n=input$number, include_rts=FALSE,
                                 `-filter` = "replies", 
                                 lang='en')
      
    }else{
      my_tweets <- get_timeline(input$word, n= input$number)
    }
    
    validate(
      need(dim(my_tweets)[1] != 0, "No tweets for your selected Word/User. 
           Please recheck your selected Word/User or Internet Connectivity.")
      )
    
    print(dim(my_tweets))
    cat("Did the reading part work at least?")
    ### Taking organic tweets only. That is removing retweets and replies 
    # Remove retweets
    my_tweets_organic <- my_tweets[my_tweets$is_retweet==FALSE, ] 
    # Remove replies
    my_tweets_organic <- subset(my_tweets_organic, is.na(my_tweets_organic$reply_to_status_id))
    
    
    my_tweets_organic$text <-  gsub("https\\S*", "", my_tweets_organic$text)
    my_tweets_organic$text <-  gsub("@\\S*", "", my_tweets_organic$text) 
    my_tweets_organic$text <-  gsub("amp", "", my_tweets_organic$text) 
    my_tweets_organic$text <-  gsub("[\r\n]", "", my_tweets_organic$text)
    my_tweets_organic$text <-  gsub("[[:punct:]]", "", my_tweets_organic$text)
    #my_tweets_organic$text <-  gsub("[[:digit:]]", "", my_tweets_organic$text)
    
    validate(
      need(dim(my_tweets_organic)[1] != 0, "No tweets for your selected Word/User. 
           Please recheck your selected Word/User or Internet Connectivity.")
      )
    my_tweets_organic
    
    
  })
  
  
  
  
  output$HashtagPlot <- renderHighchart({
    ### Hashtag Analysis 
    validate(
      need(all(is.na(reactive_head()$hashtags)) != TRUE, "No Hashtags Found")
      )
    
    

    hashtags_collection <- as.character(reactive_head()$hashtags)
    hashtags_collection <- gsub("c\\(", "", hashtags_collection)
    total_hashtags_collection <- unlist(tokenize_words(hashtags_collection))
    hashtags_collection_df <- as.data.frame(table(total_hashtags_collection))%>% rename(Hashtag=total_hashtags_collection,Frequency=Freq) %>% arrange(-Frequency) %>% 
      filter(Hashtag!= str_to_lower(input$word) & Hashtag!=str_to_title(input$word) & Hashtag!=input$word)%>%
      top_n(10) 
    hashtags_collection_df <- head(hashtags_collection_df,10)
    
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = "Most Frequent Hashtags Occuring in the Tweets for the selected Word/User") %>% 
      hc_colors("#00B2FF") %>% 
      hc_xAxis(categories = hashtags_collection_df$Hashtag) %>% 
      hc_add_series(data = hashtags_collection_df$Frequency,
                    name = "Bar Graph")

    
  })
  
  output$WordPlot <- renderHighchart({
    
    
    cat("Word Plot Part")
    tweets <- reactive_head() %>% select(text) %>% unnest_tokens(word, text)
    tweets <- tweets %>% anti_join(stop_words)
    
    
    # Make sure you remove the first one in case of words 
    frequent_words <- tweets %>% # gives you a bar chart of the most frequent words found in the tweets 
      count(word, sort = TRUE) %>%  filter(word!= str_to_lower(input$word) & word!=str_to_title(input$word) & word!=input$word) %>%
      top_n(10) 
    
    frequent_words <- head(frequent_words,10)
    cat("Before highcharter plot of Frequent Words")
    
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = "Most Frequent Words Occuring in the Tweets for the selected Word/User") %>% 
      hc_colors("#00B2FF") %>% 
      hc_xAxis(categories = frequent_words$word) %>% 
      hc_add_series(data = frequent_words$n,
                    name = "Bar Graph")
    
    
  })
  
  
  output$MostRetweetedTable <- renderDataTable({
    
    cat("Most Retweeted Tweets Table")
    top_retweets <- reactive_head() %>% arrange(-retweet_count)
    
    datatable(top_retweets[1:100,c("created_at","text","retweet_count","screen_name")],
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center;',
                 htmltools::code('Most Retweeted Tweets for the selected Word/User.')))
    
    
  })
  
  
  output$TopTweeters <- renderDataTable({
       
      cat("Frequent Users Part")
      top_tweeters <- reactive_head() %>% count(screen_name, sort = TRUE) %>%
      top_n(30) %>%
      mutate(screen_name = paste0("@", screen_name))
      datatable(top_tweeters,caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center;',
        htmltools::code('Most Frequent Tweeters for the selected Word/User')))
  
  })
  
  
  
  
  output$SentimentPlot <- renderHighchart({
    
    cat("Sentiment Analysis Part")
    # Converting tweets to ASCII to trackle strange characters
    tweets <- reactive_head() %>% select(text) %>% unnest_tokens(word, text)
    tweets <- tweets %>% anti_join(stop_words)
    tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")
    # removing retweets, in case needed 
    tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
    # removing mentions, in case needed
    tweets <-gsub("@\\w+","",tweets)
    tweets <-gsub("[[:digit:]]", "", tweets)
    ew_sentiment<-get_nrc_sentiment((tweets))
    sentimentscores<-data.frame(colSums(ew_sentiment[,]))
    names(sentimentscores) <- "Score"
    sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
    rownames(sentimentscores) <- NULL
    sentimentscores <- arrange(sentimentscores,desc(Score))
    
    
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = "Sentiment Analysis of the Tweets for selected Word/User") %>% 
      hc_colors("#00B2FF") %>% 
      hc_xAxis(categories = sentimentscores$sentiment) %>% 
      hc_add_series(data = sentimentscores$Score,
                    name = "Bar Graph")
  })
  
  output$SetimentWords <- renderDataTable({
    
    cat("Words under Sentiment Table Part")
    
    ## Words under each Sentiment 
    dict <- get_sentiment_dictionary('nrc')
    dict_df <- data.frame(word=dict$word, sentiment=dict$sentiment)
    
    total_words <- unlist(tokenize_words(reactive_head()$text))

    df_total_words <- data.frame(word = total_words[nzchar(gsub("[0-9]+", "", total_words))])
    
    common_df <- merge(df_total_words, dict_df, by='word', all.x = FALSE, all.y = FALSE)
    
    sentiment_trust <- filter(common_df, sentiment=='trust')$word
    words_trust <- as.data.frame(sort(table(sentiment_trust),decreasing=TRUE)[1:10])$sentiment_trust
    
    sentiment_anger <- filter(common_df, sentiment=='anger')$word
    words_anger <- as.data.frame(sort(table(sentiment_anger),decreasing=TRUE)[1:10])$sentiment_anger
    
    sentiment_fear <- filter(common_df, sentiment=='fear')$word
    words_fear <- as.data.frame(sort(table(sentiment_fear),decreasing=TRUE)[1:10])$sentiment_fear
    
    sentiment_joy <- filter(common_df, sentiment=='joy')$word
    words_joy <- as.data.frame(sort(table(sentiment_joy),decreasing=TRUE)[1:10])$sentiment_joy
    
    sentiment_sadness <- filter(common_df, sentiment=='sadness')$word
    words_sadness <- as.data.frame(sort(table(sentiment_sadness),decreasing=TRUE)[1:10])$sentiment_sadness
    
    sentiment_positive <- filter(common_df, sentiment=='positive')$word
    words_positive <- as.data.frame(sort(table(sentiment_positive),decreasing=TRUE)[1:10])$sentiment_positive
    
    sentiment_negative <- filter(common_df, sentiment=='negative')$word
    words_negative <- as.data.frame(sort(table(sentiment_negative),decreasing=TRUE)[1:10])$sentiment_negative
    
    sentiment_anticipation <- filter(common_df, sentiment=='anticipation')$word
    words_anticipation <- as.data.frame(sort(table(sentiment_anticipation),decreasing=TRUE)[1:10])$sentiment_anticipation
    
    sentiment_surprise <- filter(common_df, sentiment=='surprise')$word
    words_surprise <- as.data.frame(sort(table(sentiment_surprise),decreasing=TRUE)[1:10])$sentiment_surprise
    
    sentiment_disgust <- filter(common_df, sentiment=='disgust')$word
    words_disgust<- as.data.frame(sort(table(sentiment_disgust),decreasing=TRUE)[1:10])$sentiment_disgust
    
    # words_df <- data.frame(Positive=words_positive,Negative=words_negative, Trust=words_trust, Joy=words_joy, 
    #                        Anticipation=words_anticipation, Sadness=words_sadness, Fear=words_fear,Anger=words_anger,
    #                        Disgust=words_disgust,Surprise=words_surprise )
    
    words_df <- data.frame(Trust=words_trust, Joy=words_joy, 
                           Anticipation=words_anticipation, Sadness=words_sadness, Fear=words_fear,Anger=words_anger,
                           Disgust=words_disgust,Surprise=words_surprise )
    
    datatable(words_df,caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: center;',
      htmltools::code('Word under each Sentiment for the selected Word/User')))
    
    
  })
  
  
  
})
