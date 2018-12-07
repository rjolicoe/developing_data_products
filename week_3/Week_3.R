install.packages("Quandl")

library(Quandl)
library(readr)
library(curl)
library(dplyr)
library(zoo)
library(ggplot2)
library(plotly)

api <- "ij6nAJPwMWJCbmVbEh-j"
twitter_sentiment <- Quandl("NS1/TWTR_US", api_key=api)

twitter_sentiment <- twitter_sentiment %>%
                      filter(Date >= '2017-01-01')

twitter_sentiment <- zoo(twitter_sentiment[,-1], 
                         order.by = as.Date(twitter_sentiment[,1], 
                         format = "%d/%m/%Y"))

twitter_sentiment  <- aggregate(twitter_sentiment, as.yearmon, mean)

twitter_sentiment  <- data.frame(Date = index(twitter_sentiment),
                                coredata(twitter_sentiment))

twitter_sentiment  <- twitter_sentiment %>% 
                        select(-contains("News"))


p <- plot_ly(twitter_sentiment, x = ~Date, y = ~Sentiment,name = 'Sentiment',
             type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(0,100,80)')) %>%
     add_trace(y = ~Sentiment.High, name = 'High Sentiment',  mode = 'lines',
               type = 'scatter',line = list(color = 'transparent'), fill = 'tonexty',
                fillcolor='rgba(0,100,80,0.2)') %>%
     add_trace(y = ~Sentiment.Low, name = 'Low Sentiment', mode = 'lines',
               type = 'scatter', fill = 'tonexty', 
               fillcolor='rgba(0,100,80,0.2)') %>%
    # add_trace(y =~ Sentiment.Low, name = 'Low Sentiment', mode = 'lines') %>%
     layout(title = 'Twitter Sentiment by Month')
p