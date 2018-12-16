#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library("FactoMineR")
library("dplyr")
library("ggplot2")
library("data.table")

data <- read.csv("BDD_ACCIDENTO.csv", sep = ";")

#On récupère les personnes ayant eu des accidents
accidentés <- subset(data, data$ACCIDENT == 1)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$experienceplot <- renderPlot({
    
    #On retire la personne qui a une expérience peut plosible
    accidentés <- subset(data, data$EXPCYCLO < 98)
    
    
    # On créé une valeur de l'expérience totale pour chaque accidenté
    accidentés$expCalc =(accidentés$EXPCYCLO + accidentés$EXP125 + accidentés$EXPSUP125)/3
    
    #On a le nombre d'accident et l'experience pour chaque individu de façon nettoyée
    accidentésNettoyé <- select(accidentés, "NBACC1", "NBACC2", "NBACC3", "NBACC4", "NBACC5", "NBACC6", "NBACC7", "NBACC8", "NBACC9", "NBACC10", "NBACC11", "NBACC12", "NBACC13", "NBACC14", "expCalc" )

    # Mise en place des données en deux dimensions pour le plot
    expAndAccCategorie <- data.frame(typeAccident = as.factor(numeric()), experience = numeric())
    levels(expAndAccCategorie$typeAccident) <- c(1:14)
    
    for(i in 1:nrow(accidentésNettoyé)) {
      for(j in 1:14){
        if(accidentésNettoyé[i,j] > 0) {
          for(k in 1:accidentésNettoyé[i,j]) {
            #colonne 14 pour avoir l'experience des accidents
            value <- data.frame(typeAccident = as.factor(j), experience = accidentésNettoyé[i,15])
            expAndAccCategorie <- rbind(expAndAccCategorie, value)
          }
        }
      }
    }
    
    # draw the histogram with the specified number of bins
    #Plot dynamique pour shiny:
    if(!is.null(input$typeAccident)){
      ggplot(data = filter(.data = expAndAccCategorie, typeAccident == input$typeAccident), mapping = aes(x=experience, , fill=typeAccident , alpha=0.2)) + 
      geom_density() + 
      labs(title="", x="Experience")
    }
  })
  
})
