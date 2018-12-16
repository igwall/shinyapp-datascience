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
   
  output$experiencePlot <- renderPlot({
    
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
  
  # 1.2 - Les infrastructures
  output$infrastructurePlot <- renderPlot({
    
    accidentésInfrastructure <- select(accidentés, "CHOIXACC", "ROLEINFRA")
    
    # On enlève les accidents avec des données vides:
    accidentésInfrastructureApprofondi <- subset(accidentésInfrastructure, !is.na(ROLEINFRA))

    levels(accidentésInfrastructureApprofondi$ROLEINFRA) <- c(1,2)

    # On tris les id par type d'accident:
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(1:5), "ACC"] <- 1
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(6:10), "ACC"] <- 2
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(11:15), "ACC"] <- 3
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(16:20), "ACC"] <- 4
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(21:25), "ACC"] <- 5
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(26:30), "ACC"] <- 6
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(31:35), "ACC"] <- 7
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(36:40), "ACC"] <- 8
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(41:45), "ACC"] <- 9
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(46:50), "ACC"] <- 10
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(51:55), "ACC"] <- 11
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(56:60), "ACC"] <- 12
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(61:65), "ACC"] <- 13
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(66:70), "ACC"] <- 14
    accidentésInfrastructureApprofondi[accidentésInfrastructureApprofondi$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    levels(accidentésInfrastructureApprofondi$ACC) <- c(1:15)
    
    contingenceInfrastructure <- table(accidentésInfrastructureApprofondi$ACC, accidentésInfrastructureApprofondi$ROLEINFRA)
    df_contingence <- as.data.frame.matrix(contingenceInfrastructure)
    colnames(df_contingence) <- c("Oui","Non")
    
    chisq.test(contingenceInfrastructure)
    
    contingence_oui <- data.frame(type_acc = c(1:15), nb = df_contingence$Oui, RoleInfrastructure = "Oui")
    contingence_non <- data.frame(type_acc = c(1:15), nb = df_contingence$Non, RoleInfrastructure = "Non")
    contingence_final <- rbind(contingence_oui, contingence_non) 
    
    if(!is.null(input$typeAccident)){
    ggplot(data = subset(contingence_final, type_acc %in% input$typeAccident), aes(x=type_acc, y=nb)) + 
      geom_bar(stat= "identity", aes(fill = RoleInfrastructure)) + 
      scale_fill_manual(values=c("#004d7e", "#64be29")) + 
      labs(title="Role de l'infrastructure pour chaque type d'accident", x="Type d'accident", y="Nombre de réponse")
    }
  })
  
})
