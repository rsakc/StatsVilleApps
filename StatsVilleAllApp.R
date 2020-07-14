#Last Updated on July 14 2020

#Loading Libraries
library(shiny)
library(readr)
library(ggplot2)
library(dplyr)

#Importing Data
data.all <-read.csv("https://www.stat2games.sites.grinnell.edu/data/statsville/getdata.php") 

#Filtering Data
data.all <- filter(data.all, Level > 0)

#To Lower
data.all$PlayerID <- tolower(data.all$PlayerID)
data.all$GroupID <- tolower(data.all$GroupID)

#Converting to Factor/Character
data.all$Level <- as.factor(data.all$Level)
data.all$WinLose <- as.factor(data.all$WinLose)
data.all$GroupID <- as.character(data.all$GroupID)
data.all$PlayerID <- as.character(data.all$PlayerID)

#Creating Percent Cured Columns
data.all <- mutate(data.all, PercentCureA = (CureA/TreatA)*100 , PercentCureB = (CureB/TreatB)*100)

#For UI Inputs
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))
ChoiceY <- c("Budget", "AvailToTreat", "TreatA", "TreatB", "CureA", "CureB", "PercentCureA", "PercentCureB", "CostA", "CostB", "SickCost")

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
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = 1),
      
       selectInput(inputId = "xvar",
                  label = "X Variable:",
                  #columns of the dataset
                  choices = c("PlayerID", "Day"),
                  selected = "Day",
                  multiple = FALSE),
      
      selectInput(inputId = "yvar",
                  label = "Y Variable:",
                  #columns of the dataset
                  choices = ChoiceY,
                  selected = "TreatA",
                  multiple = FALSE),
      
      selectInput(inputId = "color",
                  label = "Color by:",
                  choices = c("PlayerID", "Day","WinLose", "Level"),
                  selected = "WinLose",
                  multiple = FALSE),
      
      selectInput(inputId = "facets",
                  label = "Facet by:",
                  choices = c("None","PlayerID","Day","WinLose", "Level"),
                  selected = "None",
                  multiple = FALSE),
      
      checkboxInput("smoother", "Add a Model", FALSE),
      
      checkboxInput("summary", "Show Summary Statistics", FALSE),
    
      downloadButton('downloadData', label = "StatsVille Data"),
      
      a(h5("Instructor Details"),
        href="https://stat2labs.sites.grinnell.edu/statsville.html", 
        align="left", target = "_blank")
      
    ),
    
    #Outputs
    mainPanel(
      plotOutput("Plot"),
      tableOutput("SummaryTable")

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
          data <- data.all %>% filter(Level %in% input$levels)
        
        } else{
          data <- data.all %>% filter(Level %in% input$levels, PlayerID %in% input$playerID)
        }
        
      } else{
        
        if("all" %in% input$playerID){
          data <- data.all %>% filter(Level %in% input$levels, GroupID %in% input$groupID)
          
        } else{
          data <- data.all %>% filter(Level %in% input$levels, GroupID %in% input$groupID, PlayerID %in% input$playerID)
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
    
    #We need data
    if(nrow(plotData) > 0){
      
    #General Plot
    myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color)) + 
      geom_point() +
      theme_bw() +
      labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by", input$color)) +
      theme(axis.text.x = element_text(size = 18),
            axis.title = element_text(size = 20), 
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 18), 
            legend.text = element_text(size = 16), 
            axis.text.y = element_text(size = 14)) 
    
    
    #If Smoother is selected
    if(input$smoother == TRUE){
      #myplot <- myplot + stat_smooth(method = loess,  se = FALSE)
      myplot <- myplot + geom_smooth(method = lm, formula = y ~ splines::bs(x, 5), se = FALSE)
    } 
    
    #If Facet is Selected
    if(input$facets != "None"){
      #myplot <- myplot + facet_wrap(input$facets)
      myplot <- myplot + facet_wrap(as.formula(paste("~", input$facets))) +
        labs(title = paste("Plot of", input$yvar,"by", input$xvar, "and Colored by", input$color, "and Faceted by", input$facets)) +
        theme(strip.text = element_text(size = 16)) 
      
    }
    
    
    return(myplot)
    
    }
    
  })
    
    
    #Creating Summary Table
    output$SummaryTable <- renderTable({
      
      #Reactive Data
      plotData <- plotDataR()
      
      if(input$summary == TRUE){
        
        #If there is data
        if(nrow(plotData) != 0){
      
          #Y Variable
          yvar <- sym(input$yvar)
            
          table <- plotData %>% group_by_at(input$xvar) %>%
            summarize(N = n(), Mean = mean(!!yvar, na.rm = TRUE), SD = sd(!!yvar, na.rm = TRUE))
        
        
          return(table)
        
        }
      }
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