#Last Updated on July 9

#Loading Libraries
library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

#Importing Data
data.all <- read_csv("DataTemp.csv")

#data.all <-read.csv("https://www.stat2games.sites.grinnell.edu/data/getdata.php") 
#data.all <- filter(data.all, Level > 0)
#data.all$Level <- as.factor(data.all$Level)
#data.all$WinLose <- as.factor(data.all$WinLose)
#data.all$GroupID <- as.character(data.all$GroupID)
#data.all$PlayerID <- as.character(data.all$PlayerID)

#data.all <- gather(data.all, key = type, value = Count, 10:15)
#data.all <- mutate(data.all, TreatType = str_sub(type, start= -1), Assgn = str_sub(type,1,nchar(type)-1))
#data.all$TreatType <- as.factor(data.all$TreatType)

#data.all <- arrange(data.all, Assgn)
#data.all <- select(data.all, -type)

#data.all <- spread(data.all, key = Assgn, value = Count)
#data.all <- mutate(data.all, PercentCured = Cure/Treat)

#For UI Inputs
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))

##UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("StatsVille Data Visualizations"),
  
  sidebarLayout(
      sidebarPanel(
      selectInput(inputId = "groupID",
                label = "Group ID:", 
                choices =  c("all", all_groups),
                multiple = TRUE,
                selectize = TRUE,
                selected = "all"),
      
      selectInput(inputId = "playerID",
                  label = "Player ID:",
                  choices =  c("all", all_players),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput("levels", "Level",
                  choices = c(1, 2, 3, 4, 5, 6, 7, 8),
                  multiple = FALSE),
      
      selectInput(inputId = "xvar",
                  label = "X Variable:",
                  #columns of the dataset
                  choices = unique(names(data.all)),
                  selected = unique(names(data.all))[6],
                  multiple = FALSE),
      
      selectInput(inputId = "yvar",
                  label = "Y Variable:",
                  #columns of the dataset
                  choices = unique(names(data.all)),
                  selected = unique(names(data.all))[15],
                  multiple = FALSE),
      
      #checkboxInput('meanvalue',"Display Mean",FALSE),
      
    
      
#      selectInput(inputId = "color",
#                  label = "Color by",
#                  choices = c("Game", "TreatType", "Level", "PlayerID", "Day", "WinLose"),
#                  selected = "TreatType",
#                  multiple = FALSE),
      
      selectInput(inputId = "facets",
                  label = "Facet by:",
                  choices = c("None", "TreatType", "Game", "Level", "PlayerID", "Day", "WinLose"),
                  selected = "None",
                  multiple = FALSE),
      
      downloadButton('downloadData', label = "StatsVille Data")
      
    ),
    
    #Outputs
    mainPanel(
      plotOutput("Plot")
    )
  )
)

##Server
server <- function(input, output,session) {
  
  #Dynamic Player ID Input
  observe({
    
    if ("all" %in% input$groupID) {gamedata <- data.all}
    else{gamedata <- filter(data.all, GroupID %in% input$groupID)}
    
    updateSelectInput(session, 
                      "playerID",
                      choices = c("all", sort(unique(gamedata$PlayerID))),
                      selected = "all")
  })
  
  
  #Dynamic Level Input
  observe({
    
    if ("all" %in% input$groupID){
      
      if("all" %in% input$playerID){
        gamedata <- data.all
        
      } else{
        gamedata <- filter(data.all, PlayerID %in% input$playerID)
      }
      
      
    } else{
      
      if("all" %in% input$playerID){
        gamedata <- filter(data.all, GroupID %in% input$groupID)
        
      } else{
        gamedata <- filter(data.all, GroupID %in% input$groupID, PlayerID %in% input$playerID)
      }
    }
    updateSelectInput(session, 
                      "levels",
                      choices = sort(unique(gamedata$Level)),
                      selected = sort(unique(gamedata$Level))[c(1)])
  })
  
  
  #Reactive Data
  plotDataR <- reactive({
    
    #Require
    req(input$groupID)
    
    if("all" %in% input$groupID){
      
      if("all" %in% input$playerID){
        data <- data.all %>% filter(Level == input$levels)
        
      } else{
        data <- data.all %>% filter(Level == input$levels, PlayerID %in% input$playerID)
      }
      
    } else{
      
      if("all" %in% input$playerID){
        data <- data.all %>% filter(Level == input$levels, GroupID %in% input$groupID)
        
      } else{
        data <- data.all %>% filter(Level == input$levels, GroupID %in% input$groupID, PlayerID %in% input$playerID)
      }
    }
    
    return(data)
    
  })
  
  
  
  #Creating Visualizations
  output$Plot <- renderPlot({
    
    #Require
    req(input$groupID)
    
    #Reactive Data
    plotData <- plotDataR()
    
    
    #General Plot
    myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color = "TreatType")) +
      geom_point() + 
      theme_bw() +
      labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by TreatType")) +
      theme(axis.text.x = element_text(size = 18),
            axis.title = element_text(size = 20), 
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 18), 
            legend.text = element_text(size = 16), 
            axis.text.y = element_text(size = 14)) 
    

    #If Facet is Selected
    if(input$facets != "None") {
      #myplot <- myplot + facet_wrap(input$facets)
      myplot <- myplot + facet_wrap(as.formula(paste("~", input$facets))) +
        labs(title = paste("Plot of", input$yvar,"by", input$xvar, "and Colored by TreatType", "and Faceted by", input$facets)) +
        theme(strip.text = element_text(size = 16)) 
    }
    
    return(myplot)
    
  })
  
  
  #Download Data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('StatsVilleData-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(plotDataR(), con)
    }
  )
  
  
#Closes Server   
}

#Running Shiny App
shinyApp(ui = ui, server = server)
