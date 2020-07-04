#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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


dashboardPage(
  dashboardHeader(title = "Twitter Analytics App"),
  dashboardSidebar(
                   radioButtons("ModeChoice", "Please select Word mode or User mode:", 
                                choices = c("Word", "User")),
                   textInput("word", "Enter your word or Hashtag"),
                   sliderInput("number","Enter the number of tweets you want to include",min=100,max=5000,value = 200),
                   actionButton("go","Submit")),
  dashboardBody(
                
                h3("Twitter Analytics App"),
                
                p("The Aim of this App is to do Twitter Analytics of the last 5000 (or the user selected number of) Tweets
                  for the selected Word/User."),
                
                p("The App works in two modes:"),
                code("Word Mode and User Mode"),
                
                p("In Word Mode, you can enter the Word for which you want to do Twitter 
                  Analytics. And in User Mode, you can enter the User Handle whose Tweets you
                  want to analyse."),
     
                code("Try out the App by selecting your Word/User!"),
               
          
               
    
                dataTableOutput("MostRetweetedTable") %>% withSpinner(color="#0dc5c1"),
               
                highchartOutput("HashtagPlot") %>% withSpinner(color="#0dc5c1"),
              
                highchartOutput("WordPlot") %>% withSpinner(color="#0dc5c1"),
               
                dataTableOutput("TopTweeters") %>% withSpinner(color="#0dc5c1"),
               
                highchartOutput("SentimentPlot") %>% withSpinner(color="#0dc5c1"),
               
                dataTableOutput("SetimentWords") %>% withSpinner(color="#0dc5c1")
              )
)
