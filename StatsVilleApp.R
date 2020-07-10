#Last Updated on July 10
 
#Sample Size
#Scaling with Two Y Variables

#Loading Libraries
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)

#Importing Data
data.all <-read.csv("https://www.stat2games.sites.grinnell.edu/data/statsville/getdata.php") 

#Filtering Data
data.all <- filter(data.all, Game > 49)   ### Removing Games with no variability
data.all <- filter(data.all, Level == 1)  ### Restricting to Level = 1

#Converting to Factor/Character
data.all$Level <- as.factor(data.all$Level)
data.all$WinLose <- as.factor(data.all$WinLose)
data.all$GroupID <- as.character(data.all$GroupID)
data.all$PlayerID <- as.character(data.all$PlayerID)
#data.all <- mutate(data.all, WinLose = ifelse(WinLose == "1","Win","Lose"))

#Creating Percent Cured Columns
data.all <- mutate(data.all, PercentCureA = CureA/TreatA, PercentCureB = CureB/TreatB)


#For UI Inputs
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))
ChoiceA <- c("Budget", "AvailToTreat", "TreatA", "TreatB", "CureA", "CureB", "PercentCureA", "PercentCureB", "CostA", "CostB", "SickCost")
ChoiceB <- c("None", "Budget", "AvailToTreat", "TreatA", "TreatB", "CureA", "CureB", "PercentCureA", "PercentCureB", "CostA", "CostB", "SickCost")


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
      
      sliderInput(inputId = "day",
                 label = "Choose Days:",
                 step = 1,
                 ticks = FALSE,
                 min = min(data.all$Day),
                 max = max(data.all$Day),
                 value = c(min(data.all$Day), max(data.all$Day))),
      
       selectInput(inputId = "xvar",
                  label = "X Variable:",
                  choices = c("Day", "Sample Size"),
                  multiple = FALSE),
      
      selectInput(inputId = "yvar",
                  label=HTML('<p style="color:red; font-size: 11pt"> Y Variable (Red) </p>'),
                  #label = "Y Variable: (green)",####
                  choices = ChoiceA,
                  selected = "TreatA",
                  multiple = FALSE),
      
      selectInput(inputId = "yvar2",
                  label=HTML('<p style="color:blue; font-size: 11pt"> Second Y Variable (Blue): </p>'),
                  choices = ChoiceB,
                  selected = "None",
                  multiple = FALSE),
      
      selectInput(inputId = "facets",
                  label = "Facet by:",
                  choices = c("None", "Game", "PlayerID", "WinLose", "Day"),
                  selected = "None",
                  multiple = FALSE),
      
      selectInput(inputId = "tests",
                  label = "Statistical Tests:",
                  choices = c("None", "Chi-Sq Test", "Two Proportion Z-Test", "Two Sample T-Test"),
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
      uiOutput("header"),
      verbatimTextOutput("testsoutput"),
      uiOutput("ptext"),
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
  
  
  #Dynamic Y Variable 1 Input
  observe({
    
     #Require
     req(input$xvar)  
    
    if(input$xvar == "Day")
        {ChoiceA <- c("Budget", "AvailToTreat", "TreatA", "TreatB", "CureA", "CureB", "PercentCureA", "PercentCureB", "CostA", "CostB", "SickCost")}
    else{ChoiceA = c("Budget", "AvailToTreat", "TreatA", "CureA", "PercentCureA", "CostA", "SickCost")}
  
    updateSelectInput(session, 
                      "yvar",
                      choices = ChoiceA,
                      selected = "TreatA")
  })
  
  
  #Dynamic Y Variable 2 Input
  observe({
    
    #Require
    req(input$xvar)  
    
    if (input$xvar == "Day")
    {ChoiceB <- c("None", "Budget", "AvailToTreat", "TreatA", "TreatB", "CureA", "CureB", "PercentCureA", "PercentCureB", "CostA", "CostB", "SickCost")}
    else{ChoiceB = c("None", "TreatB", "CureB", "PercentCureB", "CostB")}
    
    updateSelectInput(session, 
                      "yvar2",
                      choices = ChoiceB,
                      selected = "None")
  })
  
  
  #Reactive Data
  plotDataR <- reactive({
    
      #Require
      req(input$groupID)
  
      if("all" %in% input$groupID){
        
        if("all" %in% input$playerID){
        data <- filter(data.all, Day >= input$day[1], Day <= input$day[2])
        
        } else{
          data <- filter(data.all, PlayerID %in% input$playerID, Day >= input$day[1], Day <= input$day[2])
        }
      
      } else{
        
          if("all" %in% input$playerID){
          data <- filter(data.all, GroupID %in% input$groupID, Day >= input$day[1], Day <= input$day[2])
        
        } else{
          data <- filter(data.all, GroupID %in% input$groupID, PlayerID %in% input$playerID, Day >= input$day[1], Day <= input$day[2])
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

    
    #If X Variable is Day
    if(input$xvar=="Day"){ 
    myplot <- ggplot(plotData) +
       geom_point(aes_string(x = input$xvar,y = input$yvar), color = "red") + 
       theme_bw() +
       labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar)) +
       theme(axis.text.x = element_text(size = 18),
            axis.title = element_text(size = 20), 
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 18), 
            legend.text = element_text(size = 16), 
            axis.text.y = element_text(size = 14)) 
   

      #If Y Variable 2 is selected 
      if (input$yvar2 != "None"){
        myplot <- myplot + geom_point(aes_string(x = input$xvar,y = input$yvar2), color = "blue") +
          labs(title = paste("Plot of", input$yvar, "and", input$yvar2, "by", input$xvar))
        } 
     
      #If Smoother is selected
      if (input$smoother == TRUE){
        #myplot <- myplot + stat_spline(aes_string(x = input$xvar,y = input$yvar), color = "red")
        #myplot <- myplot + geom_smooth(aes_string(x = input$xvar,y = input$yvar), method = lm, formula = y ~ splines::bs(x, 5), se = FALSE, color = "red")
        myplot <- myplot + geom_smooth(aes_string(x = input$xvar,y = input$yvar), method = loess, se = FALSE, color = "red")
      
        
        #Smoother + Y Variable 2 is selected
        if(input$yvar2 != "None"){
          #myplot <- myplot + stat_spline(aes_string(x = input$xvar,y = input$yvar2), color = "blue")
          myplot <- myplot + geom_smooth(aes_string(x = input$xvar,y = input$yvar2), method = loess, se = FALSE, color = "blue")
          }
        }
    
     }
      
    
    #If X Variable is Sample Size
     if(input$xvar=="Sample Size"){ 
        myplot <- ggplot(plotData) +
          geom_point(aes_string(x = "TreatA", y = input$yvar), color = "red") + 
          theme_bw() +
          labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar)) +
          theme(axis.text.x = element_text(size = 18),
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14)) 
        
    
        #If Y Variable 2 is Selected
        if (input$yvar2 != "None"){
          myplot <- myplot + geom_point(aes_string(x = "TreatB", y = input$yvar2), color = "blue") +
            labs(title = paste("Plot of", input$yvar, "and", input$yvar2, "by", input$xvar))
        } 
      
        #If Smoother is selected
        if(input$smoother == TRUE){
          #myplot <- myplot + stat_spline(aes_string(x = "TreatA" ,y = input$yvar), color = "red")
          myplot <- myplot + geom_smooth(aes_string(x = "TreatA",y = input$yvar), method = loess, se = FALSE, color = "red")
          
          #Smoother + Y Variable 2 is selected
          if(input$yvar2 != "None"){
            #myplot <- myplot + stat_spline(aes_string(x = "TreatB", y = input$yvar2), color = "blue")
              myplot <- myplot + geom_smooth(aes_string(x = "TreatB",y = input$yvar2), method = loess, se = FALSE, color = "blue")
          }
        }
      }  
      
    
    #If Facet Option is selected
    if(input$facets != "None") {
       #myplot <- myplot + facet_wrap(input$facets)
      
       #Y Variable 2 is Not Selected
       if(input$yvar2 == "None"){
       myplot <- myplot + facet_wrap(as.formula(paste("~", input$facets))) +
         labs(title = paste("Plot of", input$yvar,"by", input$xvar, "and Faceted by", input$facets)) +
         theme(strip.text = element_text(size = 16)) 
        
       #Y Variable 2 is Selected
       } else if(input$yvar2 != "None"){
         myplot <- myplot + facet_wrap(as.formula(paste("~", input$facets))) +
           labs(title = paste("Plot of", input$yvar, "and", input$yvar2,"by", input$xvar, "and Faceted by", input$facets)) +
           theme(strip.text = element_text(size = 16)) 
       }
    }
    
    return(myplot)
    
    })
    
    
    #Statistical Tests
    output$testsoutput <- renderPrint({
      
      #Reactive Data 
      plotData <- plotDataR()
      
      #We must have data
      if(nrow(plotData) > 0){
      
      
      ##Two Proportion Z Test
      if(input$tests == "Two Proportion Z-Test"){
        
        #Header
        output$header <- renderUI({
          h4("Observed Table:")
        })
        
        #Creating Vectors
        CureA <- sum(plotData$CureA)
        CureB <- sum(plotData$CureB)
        Cured <- c(CureA, CureB)
        Treated <- c(sum(plotData$TreatA), sum(plotData$TreatB))
        
        #Help Text for Proportions
        output$ptext <- renderUI(HTML(paste(
          em("prop 1: Treatment A, prop 2: Treatment B"))))
        
        prop.test(Cured, Treated, correct = FALSE)
        
    
      ##Chi Squared Test
      } else if(input$tests == "Chi-Sq Test"){
        
        #Header
        output$header <- renderUI({
          h4("Observed Table:")
        })
        
        #Creating Vectors
        CureA <- sum(plotData$CureA)
        NotCureA <- sum(plotData$TreatA) - sum(plotData$CureA)
        CureB <- sum(plotData$CureB)
        NotCureB <- sum(plotData$TreatB)- sum(plotData$CureB)
        
        Cured <- c(CureA, CureB)
        NotCured <- c(NotCureA, NotCureB)
        
        #Creating Table to Run Test
        table <- data.frame(Cured, NotCured)
        
        #Running Test
        chisq.test(table[, 1:2], correct = FALSE) 
        
        
      ##Two Sample T-Test
      } else if(input$tests == "Two Sample T-Test"){
        
        #Header
        output$header <- renderUI({
          h4("Observed Table:")
        })
        
        #Means
        CureA <- plotData$CureA
        CureB <- plotData$CureB
        
        #Test
        if(length(CureA) > 1 & length((CureB)) > 1){
          t.test(CureA, CureB)
        
        } else{
          "There are not enough observations to run the Two Sample T-Test."
        }
    
      }
    }
  })
    
    
    #Removing Header if tests input is none
    observeEvent(input$tests, {

      if(input$tests == "None"){
        output$header <- renderUI({
          ""})

      }
    })
    
    
    ##Creating Summary Table
    output$SummaryTable <- renderTable({
      
      #Reactive Data
      plotData <- plotDataR()
      
      if(input$summary == TRUE){
      
      #Creating Vectors
      CureA <- sum(plotData$CureA)
      NotCureA <- sum(plotData$TreatA) - sum(plotData$CureA)
      CureB <- sum(plotData$CureB)
      NotCureB <- sum(plotData$TreatB)- sum(plotData$CureB)
      
      Treatment <- c("Treatment A", "Treatment B")
      Treated <- c(sum(plotData$TreatA), sum(plotData$TreatB))
      Cured <- c(CureA, CureB)
      NotCured <- c(NotCureA, NotCureB)
      
      #Creating Table
      stable <- data.frame(Treatment, Treated, Cured, NotCured)
      
      return(stable)
      
      }
    })
    
    
   #Removing Proportion Help Text
    observeEvent(input$tests, {
      
      if(input$tests != "Two Proportion Z-Test"){
        output$ptext <- renderUI({""})
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