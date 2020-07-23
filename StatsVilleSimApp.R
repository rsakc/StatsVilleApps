#Last Updated on July 23 2020

#Loading Libraries
library(shinydashboard)
library(ggplot2)
library(shiny)
library(dplyr)


##UI
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(
                      title = "SIR Model Simulation",
                      titleWidth = 350),
                    
                    dashboardSidebar(
                      sliderInput(inputId = "trials",
                                  label = "Number of Trials",
                                  min = 1,
                                  max = 200,
                                  value = "20"),
                      
                      sliderInput(inputId = "population",
                                  label = "Number in School:",
                                  min = 100,
                                  max = 30000,
                                  step = 50,
                                  value = "300"),
                      
                      sliderInput(inputId = "initdis",
                                  label = "Proportion of Population Initially Diseased",
                                  min = 0,
                                  max = 1,
                                  value = "0.0833"),
                      
                      sliderInput(inputId = "percHealthyWithSymp",
                                  label = "Percent Healthy With Symptoms",
                                  min = 0,
                                  max = 0.1,
                                  value = "0"),
                      
                      sliderInput(inputId = "spread",
                                  label = "Spread Rate",
                                  min = 0,
                                  max = 0.1,
                                  value = "0.0035"),
                      
                      sliderInput(inputId = "aEff",
                                  label = "Effectiveness of A",
                                  min = 0,
                                  max = 1,
                                  value = "0.7"),
                      sliderInput(inputId = "bEff",
                                  label = "Effectiveness of B",
                                  min = 0,
                                  max = 1,
                                  value = "0.4"),
                      sliderInput(inputId = "percTreatA",
                                  label = "Percent Treated with A",
                                  min = 0,
                                  max = 1,
                                  value = "0.2"),
                      sliderInput(inputId = "percTreatB",
                                  label = "Percent Treated with B",
                                  min = 0,
                                  max = 1,
                                  value = "0.2"),
                      textInput(inputId = "trtAcost",
                                label = "Cost of Treatment A",
                                value = "25"),
                      textInput(inputId = "trtBcost",
                                label = "Cost of Treatment B",
                                value = "25"),
                      textInput(inputId = "sickcost",
                                label = "Sick Cost",
                                value = "50"),
                      
                      a(h5("Instructor Details"),
                        href="https://stat2labs.sites.grinnell.edu/statsville.html", 
                        align="left", target = "_blank")
                    ),
                    
                    dashboardBody(
                      plotOutput("plot"),
                      plotOutput("plotCost")
                      # box(
                      #   "The red line represents the number of people that have the disease.
                      #   The green line represents the number of people that are healthy.
                      #   The blue line repersents the number of people that have been cured.
                      #   The orange line represents the amount of money spent on treatment A.
                      #   The purple line represents the amount of money spent on treatment B.
                      #   The black line represents the amount of money spent on people being sick."
                      # ),
                      # box("This version implements randomness through the binomial distribution.")
                      )
                    )


##Server
server <- function(input, output) {
  
  #Setting Up
  mat <- matrix(nrow=30, ncol=26)
  sirDF <- as.data.frame(mat)
  finaloutput <- data.frame("trial", "Days", "Diseased", "NumberHealthy", "Cured", 
                            "Cured_by_A", "Cured_by_B", "TreatCost","CostA", "CostB",
                            "Sick.Cost", "Total.Cost_Acc")
  finaloutput <- finaloutput[-1,]
  
  
  #Reactive Object
  sirDF <- reactive({
    
    #Assigning Variables to Inputs
    population <- as.numeric(input$population)
    healthySymp <- as.numeric(input$percHealthyWithSymp)
    initDiseased <- round(as.numeric(input$initdis) * population)
    spreadRate <- as.numeric(input$spread)
    aEff <- as.numeric(input$aEff)
    bEff <- as.numeric(input$bEff)
    percTrtA <- as.numeric(input$percTreatA)
    percTrtB <- as.numeric(input$percTreatB)
    trtACost <- as.numeric(input$trtAcost)
    trtBCost <- as.numeric(input$trtBcost)
    sickcost <- as.numeric(input$sickcost)
    costA <- 0
    costB <- 0
    numtrials <- as.numeric(input$trials)
    colNamess <- c("trial", "Days", "Diseased", "NumberHealthy", "Cured", 
                   "Cured_by_A", "Cured_by_B", "TreatCost","CostA", "CostB",
                   "Sick.Cost", "Total.Cost_Acc")
    
    #Condition to run simulation
    if(percTrtA + percTrtB <= 1 && healthySymp + spreadRate <= 0.13) {
      
      # Calculations
      trial <- 1
      while(trial <= numtrials) {
        
        #Day 0
        mat[1,1] <- 0 #Day
        mat[1,2] <- population #Healthy
        mat[1,4] <- 0 #Diseased
        mat[1,15] <- 0 #Cumulative cured
        mat[1,17] <- 0 #Total cost
        mat[1,22] <- 0 #Cost of A
        mat[1,23] <- 0 #cost of B
        mat[1,24] <- 0 #sick Cost
        mat[1,25] <- 0 #Total Cost
        
        #Day 1
        mat[2,1] <- 1 #Day
        mat[2,4] <- initDiseased #Diseased
        mat[2,2] <- population - initDiseased #Healthy
        mat[2,3] <- round(mat[2,2] * healthySymp) #Symptoms
        mat[2,5] <- mat[2,3] + mat[2,4] #TotalSymp
        mat[2,6] <- mat[2,5] #Available to Treatment
        mat[2,7] <- round(percTrtA * mat[2,6]) # Number Treatment A
        mat[2,8] <- min(c(round(percTrtB * mat[2,6]), mat[2,6] - mat[2,7])) # Number Treatment B
        mat[2,9] <- mat[2,4] #Available to Treatment Diseased
        mat[2,10] <- round(percTrtA * mat[2,9]) #Disease Treatment A
        mat[2,11] <- min(c(round(percTrtB * mat[2,9]), mat[2,9] - mat[2,10]), na.rm = TRUE) #Disease Treatment B
        mat[2,12] <- rbinom(1, mat[2,10], aEff)  # Treatment A Cures
        mat[2,13] <- rbinom(1, mat[2,11], bEff) # Treatment B Cures
        mat[2,14] <- mat[2,12] + mat[2,13] # Sum Disease Cured
        mat[2,15] <- mat[2,14] # Cured
        mat[2,16] <- min(c(round(spreadRate * mat[2,2] * mat[2,4]), mat[2,2]), na.rm = TRUE) #Catch Disease
        mat[2,17] <- trtACost * mat[2,7] + trtBCost * mat[2,8] # Cost
        mat[2,18] <- mat[2,4] + mat[2,2] + mat[2,14] #Check
        mat[2,19] <-  trtACost * mat[2,7] #Cost A
        mat[2,20] <- trtBCost * mat[2,8] #Cost B
        mat[2,21] <- sickcost * mat[2,4] #Sick cost
        mat[2,22] <- mat[2,19] #Total cost on TrtA
        mat[2,23] <- mat[2,20] #Total cost on TrtB
        mat[2,24] <- mat[2,21] #Total cost on Sick People
        mat[2,25] <- mat[2,22] + mat[2,23] + mat[2,24] #Total Cost Cumulative 
        mat[2,26] <- mat[2,19] + mat[2,20] #Treat Cost Daily
        
        #Remaining Days
        for(i in 3:dim(mat)[1]) {
          mat[i,1] = i-1
          mat[i,4] = mat[i-1, 4] + mat[i-1, 16] - mat[i-1, 14] #Diseased + Catch Disease - Cured
          mat[i,2] = mat[i-1,2] - mat[i-1, 16]
          mat[i,3] = round(mat[i,2] * healthySymp)
          mat[i,5] = mat[i,3] + mat[i,4]
          mat[i,6] = mat[i,5]
          mat[i,7] = round(percTrtA * mat[i,6])
          mat[i,8] = min(c(round(percTrtB * mat[i,6]), mat[i,6] - mat[i,7]))
          mat[i,9] = mat[i,4]
          mat[i,10] = round(percTrtA * mat[i,9])
          mat[i,11] = min(c(round(percTrtB * mat[i,9]), mat[i,9] - mat[i,10]), na.rm = TRUE)
          mat[i,12] = rbinom(1, mat[i,10], aEff)
          mat[i,13] = rbinom(1, mat[i,11], bEff)
          mat[i,14] = mat[i,12] + mat[i,13]
          mat[i,15] = mat[i,14] + mat[i-1,15]
          mat[i,16] = min(c(round(spreadRate * mat[i,2] * mat[i,4]), mat[i,2]), na.rm = TRUE)
          mat[i,17] = (trtACost * mat[i,7]) + (trtBCost * mat[i,8])
          mat[i,18] = mat[i,4] + mat[i,2] + mat[i-1, 15]
          mat[i, 19] = trtACost * mat[i,7]
          mat[i, 20] = trtBCost * mat[i,8]
          mat[i, 21] = sickcost * mat[i,4]
          mat[i, 22] = mat[i-1,22] + mat[i,19]
          mat[i, 23] = mat[i-1,23] + mat[i,20]
          mat[i, 24] = mat[i-1,24] + mat[i,21]
          mat[i, 25] = mat[i,22] + mat[i,23] + mat[i,24]
          mat[i, 26] <- mat[i,19] + mat[i,20] #Treat Cost Daily
        }
        
        #Adding to Data Frame
        data <- as.data.frame(mat[,c(1,4,2,15,12,13,26,22,23,24,25)])
        data <- cbind("Trial" = trial, data)
        colnames(data) <- colNamess
        finaloutput <- rbind(finaloutput, data)
        trial <- trial + 1
        mat <- matrix(nrow=30, ncol=26)
      }
      
      return(finaloutput)
      
    } 
  })
  
  
  #Creating Visualization (1)
  output$plot <- renderPlot({
     
    #Assigning Variables to Inputs
    percTrtA <- as.numeric(input$percTreatA)
    percTrtB <- as.numeric(input$percTreatB)
    healthySymp <- as.numeric(input$percHealthyWithSymp)
    spreadRate <- as.numeric(input$spread)
    
    #Condition to create visual
    if(percTrtA + percTrtB <= 1 && healthySymp + spreadRate <= 0.13) {

    ggplot(sirDF(), aes(x = Days)) + 
      geom_line(aes(y = Diseased, group=trial, color= "deepskyblue"), alpha = .05) + 
      stat_summary(aes(y=Diseased, color ="deepskyblue4"),fun.y= mean, size=1, alpha =0.5, geom = "line")+
      geom_line(aes(y = NumberHealthy, group=trial,color= "olivedrab3"), alpha = .05) + 
      stat_summary(aes(y=NumberHealthy, color ="olivedrab4"), fun.y= mean, size=1, alpha =0.5, geom = "line")+
      geom_line(aes(y = Cured, group=trial, color="red"), alpha = .05) + 
      stat_summary(aes(y=Cured, color ="red4"), fun.y= mean, size=1, alpha =0.5, geom = "line")+
      ylab(label="People") + xlab("Days") +
      theme_bw() +
        theme(axis.text.x = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold"), 
              legend.title = element_text(size = 14, face = "italic"), 
              legend.text = element_text(size = 12, face = "italic"), 
              axis.text.y = element_text(size = 12)) +
      scale_color_manual(values = c("deepskyblue","deepskyblue4","olivedrab3","olivedrab4","red","red4"), 
                         labels = c("Diseased","Average Diseased","Healthy","Average Healthy","Cured","Average Cured")) +
      guides(color=guide_legend("Legend"))
    
      
    #Error Notifications 
    } else{
      
      if(percTrtA + percTrtB > 1){
        showNotification("Percent treated A and percent treated B can't be more than 1.")
      
      } else if(healthySymp + spreadRate > 0.13){
        showNotification("Percent healthy with symptoms and spread rate can't be greater than 0.13.")
      }
    }
  })

  
   #Creating Visualization (2)
   output$plotCost <- renderPlot({
     
     #Assigning Variables to Inputs
     percTrtA <- as.numeric(input$percTreatA)
     percTrtB <- as.numeric(input$percTreatB)
     healthySymp <- as.numeric(input$percHealthyWithSymp)
     spreadRate <- as.numeric(input$spread)
     
     #Condition to create visual
     if(percTrtA + percTrtB <= 1 && healthySymp + spreadRate <= 0.13) {
       
    ggplot(sirDF(), aes(x = Days)) +
      geom_line(aes(y = CostA,group=trial,color = "tomato1"), alpha=0.05) +
      stat_summary(aes(y=CostA, color ="tomato4"), fun.y= mean, size=1, alpha =0.5, geom = "line")+
      geom_line(aes(y = CostB,group=trial, color = "springgreen2"), alpha=0.05) +
      stat_summary(aes(y=CostB, color ="springgreen4"), fun.y= mean, size=1, alpha =0.5, geom = "line")+
      geom_line(aes(y = Sick.Cost,group=trial, color = "orange"), alpha=0.05) +
      stat_summary(aes(y=Sick.Cost, color ="orange3"), fun.y= mean, size=1, alpha =0.5, geom = "line")+
      geom_line(aes(y = Total.Cost_Acc,group=trial, color = "slateblue1"), alpha=0.05) +
      stat_summary(aes(y=Total.Cost_Acc, color ="slateblue3"), fun.y= mean, size=1, alpha =0.5, geom = "line")+
      ylab(label="Money") + xlab("Days") +
      theme_bw() + 
         theme(axis.text.x = element_text(size = 12),
               axis.title = element_text(size = 14, face = "bold"), 
               legend.title = element_text(size = 14, face = "italic"), 
               legend.text = element_text(size = 12, face = "italic"), 
               axis.text.y = element_text(size = 12)) +
       scale_color_manual(values = c("orange","orange3","slateblue1","slateblue3","springgreen2","springgreen4","tomato1","tomato4"), 
                          labels = c("Sick Cost","Average Sick Cost","Total Cost","Average Total Cost","CostB","Average CostB","CostA","Average CostA")) +
       guides(color=guide_legend("Legend"))
     
    }
  })

   
#Closes Server  
}

#Running Shiny App
shinyApp(ui = ui, server = server)