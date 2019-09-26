
library(dplyr)
library(ggplot2)
library(stringr)
#library(shinyWidgets)
library(plotly)
library("shinydashboard")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(tidytext)

file_list <- list.files("filmScripts/")

for (file in file_list){
  #print(file)
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- readxl::read_xlsx(paste0("filmScripts/",file))
    dataset$Movie = str_remove_all(file,".xlsx")
    names(dataset) = c("Character","Dialogue","Song","Movie")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-readxl::read_xlsx(paste0("filmScripts/",file))
    temp_dataset$Movie = str_remove_all(file,".xlsx")
    names(temp_dataset) = c("Character","Dialogue","Song","Movie")
    #print("past")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

dataset$Movie = factor(dataset$Movie)
dataset = dataset[is.na(dataset$Song),]

movieTitles = list('Solo Films' = list("Kabhi Khushi Kabhie Gham 2001","Sholay 1975","Shree 420 1955"), 
                   'Mission Impossible Series' = list("Mission Impossible 1996", "Mission Impossible 2 2000", "Mission Impossible 3 2006","Mission Impossible Rogue Nation 2015","Mission Impossible Ghost Protocol 2011"),
                   'Dirty Harry Series' = list("Dirty Harry 1 1971","Dirty Harry 2 Magnum Force 1973","Dirty Harry 3 The Enforcer 1976","Dirty Harry 4 Sudden Impact 1983","Dirty Harry 5 The Dead Pool 1988"),
                   'Bond films' = list("Dr No 1962","From Russia With Love 1963","Thunderball 1965","You Only Live Twice 1967","On Her Majesty's Secret Service 1969","Diamonds are Forever 1971","The Spy Who Loved Me 1977","For Your Eyes Only 1981","Octopussy 1983","A View To A Kill 1985","Tomorrow Never Dies 1997","The World Is Not Enough 1999","Die Another Day 2002","Casino Royale 2006")
)

GenerateWordCounts = function(dataframe_in){
  length_of_df = nrow(dataframe_in)
  word_counts = rep(NA,length_of_df)
  for(i in 1:length_of_df){
    numWords = length(unlist(strsplit(dataframe_in$Dialogue[i]," ")))
    word_counts[i] = numWords
  }
  return(word_counts)
}

dataset$'WordCount' = GenerateWordCounts(dataset)

dataset_aggregated = dataset[,c('Character','Movie')]

dataset_aggregated$WordCount = ave(dataset$WordCount,dataset_aggregated,FUN=sum)


dataset_aggregated = na.omit(dataset_aggregated[!duplicated(dataset_aggregated),])


generate_plotly = function(df){
  #size_of_df = nrow(df)
  percentages = df$WordCount / sum(df$WordCount) * 100
  plot = ggplot(df, aes(x =  reorder(Character, WordCount), y = percentages))+
    geom_bar(stat="identity",fill="steelblue", position = position_dodge(width = 0.8),width = 0.5)+
    coord_flip()+
    theme(text = element_text(size=20))+
    ggtitle("Words Spoken by each Character")+
    xlab("Character")+
    ylab("Percentage of Words Spoken")
  return(ggplotly(plot, height = 1000,width =850))
}

#this will be used for the word Cloud
Movies = unlist(movieTitles)
length_of_movie_list = length(Movies)
script_list = list()

for(i in 1:length_of_movie_list){
  script_list[Movies[i]] = paste(dataset[dataset$Movie==Movies[i],"Dialogue"], collapse = "")
}
generate_word_frequency_table = function(script){
  working_script = Corpus(VectorSource(script))
  #inspect(working_script)
  working_script <- tm_map(working_script, content_transformer(tolower))
  
  # Remove numbers
  working_script <- tm_map(working_script, removeNumbers)
  # Remove english common stopwords
  working_script <- tm_map(working_script, removeWords, stopwords("SMART"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  
  # Remove punctuations
  working_script <- tm_map(working_script, removePunctuation)
  # Eliminate extra white spaces
  working_script <- tm_map(working_script, stripWhitespace)
  # Text stemming
  # working_script <- tm_map(working_script, stemDocument)
  
  dtm <- TermDocumentMatrix(working_script)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  d$word = trimws(d$word)
  return(d)
}
generate_word_cloud = function(script){
  d = generate_word_frequency_table(script)
  set.seed(1234)
  wordcloud(words = d$word,scale = c(8, 0.2), freq = d$freq, min.freq = 1,
            max.words=50, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}

generate_word_freq_graph = function(script,n = 30){
  d = generate_word_frequency_table(script)
  colnames(d) = c("Word","Frequency")
  p = ggplot(d[1:n,], aes(x = reorder(Word,Frequency),y = Frequency))+
    geom_bar(stat="identity", fill="steelblue",position = position_dodge(width = 0.8),width = 0.5)+
    theme(text = element_text(size=20))+
    coord_flip()+
    xlab("Word")
  ggplotly(p)
}
 
generate_sentiment_graph= function(df,n=30){
  bing = get_sentiments("bing")
  word_freq_table = generate_word_frequency_table(df)
  new_df = inner_join(word_freq_table,bing)
  new_df[order(new_df$freq, decreasing = T),]
  
  ggplot(new_df[1:n,],aes(reorder(word,freq), freq, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Number of Times said",
         x = NULL) +
    theme(text = element_text(size=20))+
    coord_flip()
  
}




#packages used in the app
library(shiny)

#library(DT)



shinyApp(
  ui = fluidPage(
    titlePanel("Dialogue in Cinema"),
    sidebarPanel(
      uiOutput("var1_select")
      #actionButton(inputId = "update",                   
                   #label = "Render Graph"
      
    ),mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotlyOutput("plot")),
                  tabPanel("Word Cloud", plotOutput("wordCloud", height = 750, width = 750)),
                  tabPanel("Word Frequency Chart", plotlyOutput("wordFreq", height = 750, width = 700)),
                  tabPanel("Sentiment Analysis", plotOutput("sentiment", height = 750, width = 700))
      )
      
      
    )
    
  ),
  server = function(input, output) {
    
    output$var1_select <- renderUI({#add multiple = TRUE to include multiple movies
      selectInput("variable", "Movie",choices = movieTitles, selected = "Mission Impossible 1996")
      
    })
    
    #active_dataset = 
     # eventReactive(input$update, { # Event Reactive
     #   dataset_aggregated[dataset_aggregated$Movie %in% input$variable,]
     # })
    
    #output$data <- renderTable(
    #  active_dataset()
    #)
    
    output$plot <- renderPlotly(
      generate_plotly(dataset_aggregated[dataset_aggregated$Movie==input$variable,])
    )
    
    output$wordCloud = renderPlot(
      generate_word_cloud(script_list[[input$variable]])
    )
    
    output$wordFreq <- renderPlotly(
      generate_word_freq_graph(script_list[[input$variable]])
    )
    
    output$sentiment <- renderPlot(
      generate_sentiment_graph(script_list[[input$variable]])
    )
    
  },
  options = list(width = 1300,height = 700)
)





