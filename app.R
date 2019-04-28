# Installation guid
###################

# 1. You'll need RTools to build R packages so download it from the website

# 2. this is shiny so you need to install shiny package:
# install.packages("shiny") 

# 3. rlang is necessary for shiny app, we install it from github using the following lines:
# install.packages("devtools") 
# devtools::install_github("r-lib/rlang", build_vignettes = TRUE)

# 4. we use ggplot to visualize our content
# install.package("ggplot2")

# 5. finally install twitteR package to deal with twitter data
# install.packages("twitteR")

# In case any other package is NOT on your machine, kindly contact the team, Thank you.


library(shiny)
library(twitteR)
library(httr)
library(jsonlite)
library(DT)
library(ggplot2)
library(dplyr)
library(plyr)
library(rsconnect)
library(stringr)
library(stringi)




# Define UI for application that draws a histogram
ui <- navbarPage("Statisitcs Project",
                 
                 
                 
                 
                 tabPanel("Opinion Analysis - Mining",
                          # Application title
                          titlePanel("Opinion Analysis Tool - Opinion Mining"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              textInput("searchInput", "Enter Name","", placeholder = "Salah"),
                              textInput("sampleSize", "Enter Sample Size (positive intger between 1:100)","", placeholder = "100"),
                              
                              hr(),
                              actionButton("searchBtn", "Analyze")
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Analysis", plotOutput("opinionHistPlot", height = "700px"), plotOutput("opinionPiePlot", width = "100%", height = "400px")),
                                tabPanel("Mined Tweets", dataTableOutput('tweetsTable'))
                              
                              )
                            )
                          )
                 ),
                 
                 
                 
                 
                 
                 
                 tabPanel("Trends Analytical Tool",
                          # Application title
                          titlePanel("World Trends Statistics Demo"),
                          
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("select", label = h3("Select Country"), 
                                          choices = list("World" = 1, "Egypt" = 23424802,
                                                         "United Arab Emirates" = 23424738,
                                                         "USA" = 23424977, "United Kingdom" = 23424975), 
                                          selected = 1),
                              hr(),
                              actionButton("button", "Show")
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              tabsetPanel(

                                tabPanel("Plot",  plotOutput("trendsBars", height = "900px")),
                                tabPanel("Table", dataTableOutput('table'))
                              )
                            )
                          )
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 tabPanel("About",
                          h1("Statistics project"),
                          hr(),
                          h3("Team members:"),
                          h5("1. Ahmed Mohammed Abdulazim Abuamra (sec 3)"),
                          h5("2. Ahmed Wael Mohammed Elmayyah (sec 3)"),
                          h5("3. Ahmed Mahmoud Ahmed Korany (sec 3)"),
                          h5("4. AbdulRahman Yousry Muhammad Mahrous (sec 9)"),
                          h5("5. Andrew Awni Nasri Mahrous (sec 5)"),
                          hr(),
                          h5("FCIS 2019 - Dr. Mahmoud Mounir")    
                 )
                 
)








displayTrendsData <- function(input, output, init){
  
  consumer_key <- "JlIb2YzmMxLkKeNRKNUPPwHcQ"
  consumer_secret <- "n7tfGKYjmDykj8iPwsnMXxfjHAX3cnDGLNpIIZ2sThePQ9144G"
  access_token <- "325277816-J8oo7uBcaGYHAtcr6t3d3GtbomM5EWCTumwXydZA"
  access_secret <- "GNgQemUkczg58TT571kw3lJhg2vdmnKYxd4sGrBHts85l"
  
  # myapp login permission
  myapp <- oauth_app("twitter", key=consumer_key, secret=consumer_secret)
  sig <- sign_oauth1.0(myapp, token=access_token, token_secret=access_secret)
  
  # API call
  shiny_tweets <- NULL
  if(init == FALSE)
    shiny_tweets <- httr::GET(paste("https://api.twitter.com/1.1/trends/place.json?id=",(input$select), sep=""), sig)
  else
    shiny_tweets <- httr::GET(paste("https://api.twitter.com/1.1/trends/place.json?id=","1", sep=""), sig)
  
  json_shiny <- content(shiny_tweets)
  json_shiny2 <- jsonlite::fromJSON(toJSON(json_shiny))
  trends <- (json_shiny2$trends[[1]])
  
  if(length(trends) > 0){
    # clean null tweet_volume
    for(row in 1:nrow(trends)){
      vol <- trends[row, "tweet_volume"]
      print(vol[[1]])
      if(!is.numeric(vol[[1]])){
        trends[row, "tweet_volume"] = -1e9
      }
    }
  }
  
  
  name_ <- as.vector(unlist(trends["name"]))
  volume_ <- as.vector(unlist(trends["tweet_volume"]))
  
  
  df <- data.frame(
    nameDF = name_,
    volumeDF = volume_
  )
  df <- df[order(volume_),]
  df <- tail(df, 10)
  
  print(df)
  
  output$table <- renderDataTable(df)
  
  output$trendsBars <- renderPlot(
    
    ggplot(df, aes(x=nameDF, y=volumeDF)) + geom_bar(stat="identity") + 
      labs(x="Trend", y="Volume") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    
    #  ggplot(tail(df, 5), aes(x="", y=volumeDF , fill=nameDF, size = 20))+
    #  geom_bar(width = 1, stat = "identity")+ 
    #  coord_polar("y", start=0)+ 
    #  scale_fill_brewer(palette="Blues")+  
    #  theme_minimal()+
    #  geom_text(aes(y = volumeDF/3 + c(0, cumsum(volumeDF)[-length(volumeDF)]), label = nameDF), size=5)
    
    )
}


opinionMining <- function(input, output){
  
  
  consumer_key <- "JlIb2YzmMxLkKeNRKNUPPwHcQ"
  consumer_secret <- "n7tfGKYjmDykj8iPwsnMXxfjHAX3cnDGLNpIIZ2sThePQ9144G"
  access_token <- "325277816-J8oo7uBcaGYHAtcr6t3d3GtbomM5EWCTumwXydZA"
  access_secret <- "GNgQemUkczg58TT571kw3lJhg2vdmnKYxd4sGrBHts85l"
  
  setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret, access_token = access_token, access_secret = access_secret)
  
  tweets <- searchTwitter(input$searchInput, n = input$sampleSize, lang = "en")
  tweets.df <-twListToDF(tweets)
  Tweets.text <- laply(tweets,function(t)t$getText())
  
  pos_list = scan("positive-words.txt", what="character", comment.char=";")
  neg_list <- scan("negative-words.txt", what="character", comment.char=";")
  
  output$tweetsTable <- renderDataTable(tweets.df)

  
  # data manipulation 
  scores <- laply(Tweets.text, function(tweet, pos_list, neg_list) {

    tweet <- gsub('https://','',tweet) # removes https://
    tweet <- gsub('http://','',tweet) # removes http://
    tweet <- gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters  #like emoticons 
    tweet <- gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet <- gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet <- gsub('\\d+', '', tweet) # removes numbers
    tweet <- str_replace_all(tweet,"[^[:graph:]]", " ") 
    tweet <- stringi::stri_trans_general(tweet, "latin-ascii")
    tweet <- iconv(tweet, 'UTF-8', 'ASCII')
    
    tweet <- tolower(tweet)
    word.list <- str_split(tweet, '\\s+')
    words <- unlist(word.list)
    
    pos_list.matches <- match(words, pos_list)
    neg_list.matches <- match(words, neg_list)
    
    pos_list.matches <- !is.na(pos_list.matches)
    neg_list.matches <- !is.na(neg_list.matches)
    
    #calculations
    score <- sum(pos_list.matches) - sum(neg_list.matches)

    return(score)
    
  }, pos_list, neg_list )
  
  
  print(Tweets.text)
  
  scores.df <- data.frame(score=scores, text=tweets.df)
  
  print(scores.df$score)

  output$opinionHistPlot <- renderPlot({
    
    x <- scores.df$score
    bins <- seq(-6, 5, by = 1)
    
    ggplot(data=scores.df, aes(scores.df$score)) + 
      geom_histogram(aes(x), 
                     breaks = bins, 
                     col = "red", 
                     fill = "green",
                     alpha = 0.1) + 
      labs(title="Opinion Analysis Histogram") +
      labs(x="Score", y="Frequency")    
    
  })
  
  
  output$opinionPiePlot <- renderPlot({
    pie(table(scores.df$score))
  })
  
  
}












# Define server logic required to draw a histogram
server <- function(input, output) {
  displayTrendsData(input, output, TRUE)
  
  observeEvent(input$button, {
    displayTrendsData(input, output, FALSE)
  })
  
  
  observeEvent(input$searchBtn, {
    
    opinionMining(input, output)
  })
  

    
}

# Run the application 
shinyApp(ui = ui, server = server)
