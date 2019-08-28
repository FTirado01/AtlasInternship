#packages used in the app
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(shinyWidgets)
library(plotly)
library("shinydashboard")
#library(DT)

file_list <- list.files("filmScripts/")

for (file in file_list){
  print(file)
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
    print("past")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

dataset$Movie = factor(dataset$Movie)
dataset = dataset[is.na(dataset$Song),]

movieTitles = list('Solo Films' = list("Kabhi Khushi Kabhie Gham 2001","Sholay 1975","Shree 420 1955"), 
                   'Mission Impossible Series' = list("Mission Impossible 1996", "Mission Impossible 2 2000", "Mission Impossible 3 2006","Mission Impossible Rogue Nation 2015","Mission Impossible Ghost Protocol 2011"),
                   'Dirty Harry Series' = list("Dirty Harry 1 1971","Dirty Harry 2 Magnum Force 1973","Dirty Harry 3 The Enforcer 1976","Dirty Harry 4 Sudden Impact 1983","Dirty Harry 5 The Dead Pool 1988"),
                   'Bond films' = list("From Russia With Love 1963","The World Is Not Enough 1999")
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


dataset_aggregated = dataset_aggregated[!duplicated(dataset_aggregated),]



library(ggplot2)
#install.packages("ggthemes") # Install 
library(ggthemes) # Load
#install.packages("plotly")
library(plotly)


generate_plotly = function(df){


  plot = ggplot(df, aes(x =  Character, y = WordCount))+geom_bar(aes(colour = Movie),stat="identity", position=position_dodge())+theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12))
  return(plot)
}

shinyApp(
  ui = fluidPage(
    titlePanel("Dialogue in Cinema"),
    sidebarPanel(
      uiOutput("var1_select"),
      actionButton(inputId = "update",                   
                   label = "Render Graph"
      )
    ),mainPanel(
      #tableOutput("data"),
      plotlyOutput("plot", width = "125%")
    )
    
  ),
  server = function(input, output) {
    
    output$var1_select <- renderUI({
      selectInput("variable", "Movie",choices = movieTitles, multiple = TRUE)
      
    })
    
    active_dataset = 
      eventReactive(input$update, { # Event Reactive
        dataset_aggregated[dataset_aggregated$Movie %in% input$variable,]
      })
    
    output$data <- renderTable(
      active_dataset()
    )
    
    output$plot <- renderPlotly(
      generate_plotly(active_dataset())
    )
    
  }
)
