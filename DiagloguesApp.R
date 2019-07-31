library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(shinyWidgets)
library(plotly)
library("shinydashboard")
#library(DT)

movieTitles = list('Solo Films' = list("Kabhi Khushi Kabhie Gham 2001","Sholay 1975","Shree 420 1955"), 
                   'Mission Impossible Series' = list("Mission Impossible 2 2000", "Mission Impossible 3 2006","Mission Impossible Rogue Nation 2015","Mission Impossible Ghost Protocol 2011"),
                   'Dirty Harry Series' = list("Dirty Harry 1 1971")
                )



createDataframe = function(pathTofile){
  df = readxl::read_xlsx(pathTofile)
  colnames(df) = c("Character","Dialogue","Song")
  df = df[is.na(df$Song),]
  characters = factor(df$Character)
  df$`WordCount` = GenerateWordCounts(df)
  df = aggregate(df$WordCount, by=list(characters_list=characters), FUN=sum)
  colnames(df) = c("Characters", "Word Count")
  return(as.data.frame(df))
}

GenerateWordCounts = function(dataframe_in){
  length_of_df = nrow(dataframe_in)
  word_counts = rep(NA,length_of_df)
  for(i in 1:length_of_df){
    numWords = length(unlist(strsplit(dataframe_in$Dialogue[i]," ")))
    word_counts[i] = numWords
  }
  return(word_counts)
}

library(ggplot2)
#install.packages("ggthemes") # Install 
library(ggthemes) # Load
#install.packages("plotly")
library(plotly)


generate_plotly = function(df){
  
  characters = factor(df$Character)
  df$`WordCount` = GenerateWordCounts(df)
  word_count_by_characters = aggregate(df$WordCount, by=list(characters_list=characters), FUN=sum)
  colnames(word_count_by_characters) = c("Characters", "WordCount")
  
  
  plot = ggplot(word_count_by_characters, aes(x =  Characters, y = WordCount))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p = ggplotly(plot)
  
  return(p)
}

shinyApp(
  ui = fluidPage(
    selectInput("variable", "Movie",choices = movieTitles
    ),
    tableOutput("data"),
    plotlyOutput("plot")
  ),
  server = function(input, output) {
    output$data <- renderTable(
      createDataframe(paste0("filmScripts/",str_replace_all(input$variable," ","_"),".xlsx"))
    )
    output$plot <- renderPlotly({
      generate_plotly(output$data)
    })
  }
)
