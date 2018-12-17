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
  
  # 1.1 - Les humeurs
  #Neutre
  output$humeurNeutrePlot <- renderPlot({
    AccAndHumeur <- subset(select(accidentés, "CHOIXACC", "HUMEURNEUTRE",	"HUMEURGAI",	"HUMEURMECONTENT",	"HUMEURTRISTE",	"HUMEURCOLERE"),
                           !is.na(HUMEURNEUTRE) |
                             !is.na(HUMEURGAI) |
                             !is.na(HUMEURMECONTENT)|
                             !is.na(HUMEURTRISTE) |
                             !is.na(HUMEURCOLERE) 
    )
    
    AccAndHumeur <- subset(AccAndHumeur, AccAndHumeur$HUMEURCOLERE <= 5 & AccAndHumeur$HUMEURGAI <= 5 & AccAndHumeur$HUMEURMECONTENT <= 5 & AccAndHumeur$HUMEURTRISTE <= 5 & AccAndHumeur$HUMEURCOLERE <= 5)
    
    # On tris les id par type d'accident:
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(1:5), "ACC"] <- 1
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(6:10), "ACC"] <- 2
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(11:15), "ACC"] <- 3
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(16:20), "ACC"] <- 4
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(21:25), "ACC"] <- 5
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(26:30), "ACC"] <- 6
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(31:35), "ACC"] <- 7
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(36:40), "ACC"] <- 8
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(41:45), "ACC"] <- 9
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(46:50), "ACC"] <- 10
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(51:55), "ACC"] <- 11
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(56:60), "ACC"] <- 12
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(61:65), "ACC"] <- 13
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(66:70), "ACC"] <- 14
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    contingenceAccNeutre <- table(AccAndHumeur$ACC, AccAndHumeur$HUMEURNEUTRE)
    colnames(contingenceAccNeutre) <- c("Neutre_Ext","Neutre_Pro", "Neutre_Perso", "Neutre_Non", "Neutre_NSP")
    afcNeutre <- CA(contingenceAccNeutre)
  })
  
  #Gai
  output$humeurGaiPlot <- renderPLot({
    AccAndHumeur <- subset(select(accidentés, "CHOIXACC", "HUMEURNEUTRE",	"HUMEURGAI",	"HUMEURMECONTENT",	"HUMEURTRISTE",	"HUMEURCOLERE"),
                           !is.na(HUMEURNEUTRE) |
                             !is.na(HUMEURGAI) |
                             !is.na(HUMEURMECONTENT)|
                             !is.na(HUMEURTRISTE) |
                             !is.na(HUMEURCOLERE) 
    )
    
    AccAndHumeur <- subset(AccAndHumeur, AccAndHumeur$HUMEURCOLERE <= 5 & AccAndHumeur$HUMEURGAI <= 5 & AccAndHumeur$HUMEURMECONTENT <= 5 & AccAndHumeur$HUMEURTRISTE <= 5 & AccAndHumeur$HUMEURCOLERE <= 5)
    
    # On tris les id par type d'accident:
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(1:5), "ACC"] <- 1
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(6:10), "ACC"] <- 2
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(11:15), "ACC"] <- 3
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(16:20), "ACC"] <- 4
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(21:25), "ACC"] <- 5
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(26:30), "ACC"] <- 6
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(31:35), "ACC"] <- 7
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(36:40), "ACC"] <- 8
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(41:45), "ACC"] <- 9
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(46:50), "ACC"] <- 10
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(51:55), "ACC"] <- 11
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(56:60), "ACC"] <- 12
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(61:65), "ACC"] <- 13
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(66:70), "ACC"] <- 14
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    contingenceAccGai <- table(AccAndHumeur$ACC, AccAndHumeur$HUMEURGAI)# AccAndHumeur$HUMEURGAI, AccAndHumeur$HUMEURMECONTENT, AccAndHumeur$HUMEURTRISTE, AccAndHumeur$HUMEURCOLERE)
    colnames(contingenceAccGai) <- c("Gai_Ext","Gai_Pro", "Gai_Perso", "Gai_Non", "Gai_NSP")
    afcGai <- CA(contingenceAccGai)
  })
  
  #Triste
  output$humeurTristePlot <- renderPLot({
    AccAndHumeur <- subset(select(accidentés, "CHOIXACC", "HUMEURNEUTRE",	"HUMEURGAI",	"HUMEURMECONTENT",	"HUMEURTRISTE",	"HUMEURCOLERE"),
                           !is.na(HUMEURNEUTRE) |
                             !is.na(HUMEURGAI) |
                             !is.na(HUMEURMECONTENT)|
                             !is.na(HUMEURTRISTE) |
                             !is.na(HUMEURCOLERE) 
    )
    
    AccAndHumeur <- subset(AccAndHumeur, AccAndHumeur$HUMEURCOLERE <= 5 & AccAndHumeur$HUMEURGAI <= 5 & AccAndHumeur$HUMEURMECONTENT <= 5 & AccAndHumeur$HUMEURTRISTE <= 5 & AccAndHumeur$HUMEURCOLERE <= 5)
    
    # On tris les id par type d'accident:
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(1:5), "ACC"] <- 1
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(6:10), "ACC"] <- 2
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(11:15), "ACC"] <- 3
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(16:20), "ACC"] <- 4
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(21:25), "ACC"] <- 5
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(26:30), "ACC"] <- 6
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(31:35), "ACC"] <- 7
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(36:40), "ACC"] <- 8
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(41:45), "ACC"] <- 9
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(46:50), "ACC"] <- 10
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(51:55), "ACC"] <- 11
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(56:60), "ACC"] <- 12
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(61:65), "ACC"] <- 13
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(66:70), "ACC"] <- 14
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    contingenceAccTriste <- table(AccAndHumeur$ACC, AccAndHumeur$HUMEURTRISTE)# AccAndHumeur$HUMEURTRISTE, AccAndHumeur$HUMEURMECONTENT, AccAndHumeur$HUMEURTRISTE, AccAndHumeur$HUMEURCOLERE)
    colnames(contingenceAccTriste) <- c("Triste_Ext","Triste_Pro", "Triste_Perso", "Triste_Non", "Triste_NSP")
    afcTriste <- CA(contingenceAccTriste)
  })
  
  #Content
  output$humeurContentPlot <- renderPLot({
    AccAndHumeur <- subset(select(accidentés, "CHOIXACC", "HUMEURNEUTRE",	"HUMEURGAI",	"HUMEURMECONTENT",	"HUMEURTRISTE",	"HUMEURCOLERE"),
                           !is.na(HUMEURNEUTRE) |
                             !is.na(HUMEURGAI) |
                             !is.na(HUMEURMECONTENT)|
                             !is.na(HUMEURTRISTE) |
                             !is.na(HUMEURCOLERE) 
    )
    
    AccAndHumeur <- subset(AccAndHumeur, AccAndHumeur$HUMEURCOLERE <= 5 & AccAndHumeur$HUMEURGAI <= 5 & AccAndHumeur$HUMEURMECONTENT <= 5 & AccAndHumeur$HUMEURTRISTE <= 5 & AccAndHumeur$HUMEURCOLERE <= 5)
    
    # On tris les id par type d'accident:
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(1:5), "ACC"] <- 1
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(6:10), "ACC"] <- 2
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(11:15), "ACC"] <- 3
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(16:20), "ACC"] <- 4
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(21:25), "ACC"] <- 5
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(26:30), "ACC"] <- 6
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(31:35), "ACC"] <- 7
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(36:40), "ACC"] <- 8
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(41:45), "ACC"] <- 9
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(46:50), "ACC"] <- 10
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(51:55), "ACC"] <- 11
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(56:60), "ACC"] <- 12
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(61:65), "ACC"] <- 13
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(66:70), "ACC"] <- 14
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    contingenceAccContent <- table(AccAndHumeur$ACC, AccAndHumeur$HUMEURMECONTENT)# AccAndHumeur$HUMEURGAI, AccAndHumeur$HUMEURMECONTENT, AccAndHumeur$HUMEURTRISTE, AccAndHumeur$HUMEURCOLERE)
    colnames(contingenceAccContent) <- c("Mecontent_Ext","Mecontent_Pro", "Mecontent_Perso", "Mecontent_Non", "Mecontent_NSP")
    afcMecontent <- CA(contingenceAccContent)
  })
  
  
  #Colere
  output$humeurColerePlot <- renderPLot({
    AccAndHumeur <- subset(select(accidentés, "CHOIXACC", "HUMEURNEUTRE",	"HUMEURGAI",	"HUMEURMECONTENT",	"HUMEURTRISTE",	"HUMEURCOLERE"),
                           !is.na(HUMEURNEUTRE) |
                             !is.na(HUMEURGAI) |
                             !is.na(HUMEURMECONTENT)|
                             !is.na(HUMEURTRISTE) |
                             !is.na(HUMEURCOLERE) 
    )
    
    AccAndHumeur <- subset(AccAndHumeur, AccAndHumeur$HUMEURCOLERE <= 5 & AccAndHumeur$HUMEURGAI <= 5 & AccAndHumeur$HUMEURMECONTENT <= 5 & AccAndHumeur$HUMEURTRISTE <= 5 & AccAndHumeur$HUMEURCOLERE <= 5)
    
    # On tris les id par type d'accident:
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(1:5), "ACC"] <- 1
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(6:10), "ACC"] <- 2
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(11:15), "ACC"] <- 3
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(16:20), "ACC"] <- 4
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(21:25), "ACC"] <- 5
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(26:30), "ACC"] <- 6
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(31:35), "ACC"] <- 7
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(36:40), "ACC"] <- 8
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(41:45), "ACC"] <- 9
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(46:50), "ACC"] <- 10
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(51:55), "ACC"] <- 11
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(56:60), "ACC"] <- 12
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(61:65), "ACC"] <- 13
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(66:70), "ACC"] <- 14
    AccAndHumeur[AccAndHumeur$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    contingenceAccColere <- table(AccAndHumeur$ACC, AccAndHumeur$HUMEURCOLERE)# AccAndHumeur$HUMEURGAI, AccAndHumeur$HUMEURMECONTENT, AccAndHumeur$HUMEURTRISTE, AccAndHumeur$HUMEURCOLERE)
    colnames(contingenceAccColere) <- c("Colere_Ext","Colere_Pro", "Colere_Perso", "Colere_Non", "Colere_NSP")
    afcColere <- CA(contingenceAccColere)
  })
  
  #Usages AFC
  output$usagePlot <- renderPlot({
    accidentPropre <- select(subset(data, data$ACCIDENT == 1), "NBACC1", "NBACC2","NBACC3","NBACC4","NBACC5","NBACC6","NBACC7","NBACC8", "NBACC9","NBACC10","NBACC11","NBACC12","NBACC13","NBACC14","USAGE1")
    
    # Mise en place des données en deux dimensions pour le plot
    AccAndUsage <- data.frame(typeAccident = factor(integer()), usage= factor(integer()))
    
    levels(AccAndUsage$typeAccident) <- c(1:14)
    levels(AccAndUsage$usage) <- c(1:3)
    
    for(i in 1:nrow(accidentPropre)) {
      for(j in 1:14){
        if(accidentPropre[i,j] > 0) {
          for(k in 1:accidentPropre[i,j]) {
            #colonne 14 pour avoir l'experience des accidents
            value <- data.frame(typeAccident = as.factor(j), usage = accidentPropre[i,"USAGE1"])
            AccAndUsage <- rbind(AccAndUsage, value)
          }
        }
      }
    }
    
    tab <- table(AccAndUsage$typeAccident, AccAndUsage$usage)
    chisq.test(tab)
    AFCusage <- CA(tab)
    
  })
  
  #1.1 Alccol
  
  output$alcoolPlot <- renderPlot({
    accidentés <- subset(data, data$ACCIDENT == 1)
    
    # Pour savoir si un accident est aggravé on s'interesse au questionnaire approfondi. 
    # Pour savoir de quel accident il parle, on doit récupérer l'id de l'accident afin de le relier
    # à l'accident en question.
    
    # On sélectionne les bonnes colonnes et on enlève les accidents avec des données vides:
    AccAndAlcool <- subset(select(accidentés, "CHOIXACC", "ALCOOL"), !is.na(ALCOOL))
    
    # On tris les id par type d'accident:
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(1:5), "ACC"] <- 1
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(6:10), "ACC"] <- 2
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(11:15), "ACC"] <- 3
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(16:20), "ACC"] <- 4
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(21:25), "ACC"] <- 5
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(26:30), "ACC"] <- 6
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(31:35), "ACC"] <- 7
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(36:40), "ACC"] <- 8
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(41:45), "ACC"] <- 9
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(46:50), "ACC"] <- 10
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(51:55), "ACC"] <- 11
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(56:60), "ACC"] <- 12
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(61:65), "ACC"] <- 13
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(66:70), "ACC"] <- 14
    AccAndAlcool[AccAndAlcool$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    
    contingenceAcc <- table(AccAndAlcool$ACC, AccAndAlcool$ALCOOL)
    
    df_contingence <- as.data.frame.matrix(contingenceAcc)
    colnames(df_contingence) <- c("Oui","Non")
    
    plot_graph <- data.frame(type = c(1:15), nb = df_contingence$Oui, Alcool = "Oui")
    plot_graph2 <- data.frame(type = c(1:15), nb = df_contingence$Non, Alcool  = "Non")
    final_plot_graph <- rbind(plot_graph, plot_graph2) 
    
    if(!is.null(input$typeAccident)){
      ggplot(data= subset(final_plot_graph, type == input$typeAccident), aes(type, nb)) + 
      geom_bar(stat= "identity", aes(fill = Alcool)) + 
      scale_fill_manual(values=c("#004d7e", "#64be29"))
    }
  })
  
  #1.1 Stupéfiant
  
  output$stupefiantPlot <- renderPlot({
    accidentés <- subset(data, data$ACCIDENT == 1)
    
    # Pour savoir si un accident est aggravé on s'interesse au questionnaire approfondi. 
    # Pour savoir de quel accident il parle, on doit récupérer l'id de l'accident afin de le relier
    # à l'accident en question.
    
    # On sélectionne les bonnes colonnes et on enlève les accidents avec des données vides:
    AccAndStup <- subset(select(accidentés, "CHOIXACC", "STUP"), !is.na(STUP))
    
    # On tris les id par type d'accident:
    AccAndStup[AccAndStup$CHOIXACC %in% c(1:5), "ACC"] <- 1
    AccAndStup[AccAndStup$CHOIXACC %in% c(6:10), "ACC"] <- 2
    AccAndStup[AccAndStup$CHOIXACC %in% c(11:15), "ACC"] <- 3
    AccAndStup[AccAndStup$CHOIXACC %in% c(16:20), "ACC"] <- 4
    AccAndStup[AccAndStup$CHOIXACC %in% c(21:25), "ACC"] <- 5
    AccAndStup[AccAndStup$CHOIXACC %in% c(26:30), "ACC"] <- 6
    AccAndStup[AccAndStup$CHOIXACC %in% c(31:35), "ACC"] <- 7
    AccAndStup[AccAndStup$CHOIXACC %in% c(36:40), "ACC"] <- 8
    AccAndStup[AccAndStup$CHOIXACC %in% c(41:45), "ACC"] <- 9
    AccAndStup[AccAndStup$CHOIXACC %in% c(46:50), "ACC"] <- 10
    AccAndStup[AccAndStup$CHOIXACC %in% c(51:55), "ACC"] <- 11
    AccAndStup[AccAndStup$CHOIXACC %in% c(56:60), "ACC"] <- 12
    AccAndStup[AccAndStup$CHOIXACC %in% c(61:65), "ACC"] <- 13
    AccAndStup[AccAndStup$CHOIXACC %in% c(66:70), "ACC"] <- 14
    AccAndStup[AccAndStup$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    
    contingenceAcc <- table(AccAndStup$ACC, AccAndStup$STUP)
    
    df_contingence <- as.data.frame.matrix(contingenceAcc)
    colnames(df_contingence) <- c("Oui","Non")
    
    plot_graph <- data.frame(type = c(1:15), nb = df_contingence$Oui, Stup = "Oui")
    plot_graph2 <- data.frame(type = c(1:15), nb = df_contingence$Non, Stup  = "Non")
    final_plot_graph <- rbind(plot_graph, plot_graph2) 
    
    if(!is.null(input$typeAccident)){
      ggplot(data = subset(final_plot_graph, type == input$typeAccident), aes(type, nb)) + 
      geom_bar(stat= "identity", aes(fill = Stup)) + 
      scale_fill_manual(values=c("#004d7e", "#64be29"))
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
  
  # 1.2 Meteo
  output$meteoPlot <- renderPLot({
    accidentés <- subset(data, data$ACCIDENT == 1)
    
    # Pour savoir si un accident est aggravé on s'interesse au questionnaire approfondi. 
    # Pour savoir de quel accident il parle, on doit récupérer l'id de l'accident afin de le relier
    # à l'accident en question.
    
    # On sélectionne les bonnes colonnes et on enlève les accidents avec des données vides:
    AccEtTemps <- subset(select(accidentés, "CHOIXACC", "TEMPS"), !is.na(TEMPS))
    
    # On tris les id par type d'accident:
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(1:5), "ACC"] <- 1
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(6:10), "ACC"] <- 2
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(11:15), "ACC"] <- 3
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(16:20), "ACC"] <- 4
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(21:25), "ACC"] <- 5
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(26:30), "ACC"] <- 6
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(31:35), "ACC"] <- 7
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(36:40), "ACC"] <- 8
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(41:45), "ACC"] <- 9
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(46:50), "ACC"] <- 10
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(51:55), "ACC"] <- 11
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(56:60), "ACC"] <- 12
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(61:65), "ACC"] <- 13
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(66:70), "ACC"] <- 14
    AccEtTemps[AccEtTemps$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    
    contingenceAcc <- table(AccEtTemps$ACC, AccEtTemps$TEMPS)
    
    df_contingence <- as.data.frame.matrix(contingenceAcc)
    colnames(df_contingence) <- c("Sec","Pluie","Intemperies")
    
    afc_temps <- CA(df_contingence)
  })
  
  #1.2 Traffic
  ouput$trafficPlot <- renderPlot({
    
    accidentés <- subset(data, data$ACCIDENT == 1)
    
    # Pour savoir si un accident est aggravé on s'interesse au questionnaire approfondi. 
    # Pour savoir de quel accident il parle, on doit récupérer l'id de l'accident afin de le relier
    # à l'accident en question.
    
    
    # On sélectionne les bonnes colonnes et on enlève les accidents avec des données vides:
    AccEtTrafic <- subset(select(accidentés, "CHOIXACC", "TRAFIC"), !is.na(TRAFIC))
    AccEtTrafic <- subset(select(AccEtTrafic, "CHOIXACC", "TRAFIC"), AccEtTrafic$TRAFIC %in% c(1:4))
    
    
    # On tris les id par type d'accident:
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(1:5), "ACC"] <- 1
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(6:10), "ACC"] <- 2
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(11:15), "ACC"] <- 3
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(16:20), "ACC"] <- 4
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(21:25), "ACC"] <- 5
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(26:30), "ACC"] <- 6
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(31:35), "ACC"] <- 7
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(36:40), "ACC"] <- 8
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(41:45), "ACC"] <- 9
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(46:50), "ACC"] <- 10
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(51:55), "ACC"] <- 11
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(56:60), "ACC"] <- 12
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(61:65), "ACC"] <- 13
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(66:70), "ACC"] <- 14
    AccEtTrafic[AccEtTrafic$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    AccEtTrafic$TRAFIC = droplevels(AccEtTrafic$TRAFIC)
    
    contingenceAcc <- table(AccEtTrafic$ACC, AccEtTrafic$TRAFIC)
    
    df_contingence <- as.data.frame.matrix(contingenceAcc)
    colnames(df_contingence) <- c("TresFluide","Fluide","Dense", "Bloque")
    
    afc_trafic <- CA(df_contingence)
  })
  
  #1.2 Etat de la chaussée
  output$etatChausseePlot <- renderPlot({
    accidentés <- subset(data, data$ACCIDENT == 1)
    
    # Pour savoir si un accident est aggravé on s'interesse au questionnaire approfondi. 
    # Pour savoir de quel accident il parle, on doit récupérer l'id de l'accident afin de le relier
    # à l'accident en question.
    
    # On sélectionne les bonnes colonnes et on enlève les accidents avec des données vides:
    AccAndJournee <- subset(select(accidentés, "CHOIXACC", "CHAUSSEE1", "CHAUSSEE2", "CHAUSSEE3", "CHAUSSEE4"), !is.na(CHAUSSEE1))
    levels(AccAndJournee$CHAUSSEE1) <- c(1:7)
    levels(AccAndJournee$CHAUSSEE2) <- c(1:7)
    levels(AccAndJournee$CHAUSSEE3) <- c(1:7)
    levels(AccAndJournee$CHAUSSEE4) <- c(1:7)
    
    # On tris les id par type d'accident:
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(1:5), "ACC"] <- 1
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(6:10), "ACC"] <- 2
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(11:15), "ACC"] <- 3
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(16:20), "ACC"] <- 4
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(21:25), "ACC"] <- 5
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(26:30), "ACC"] <- 6
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(31:35), "ACC"] <- 7
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(36:40), "ACC"] <- 8
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(41:45), "ACC"] <- 9
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(46:50), "ACC"] <- 10
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(51:55), "ACC"] <- 11
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(56:60), "ACC"] <- 12
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(61:65), "ACC"] <- 13
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(66:70), "ACC"] <- 14
    AccAndJournee[AccAndJournee$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    c1 <- select(subset(AccAndJournee, AccAndJournee$CHAUSSEE1 %in% c(1:7)), "ACC", "CHAUSSEE1")
    colnames(c1) <- c("typeAcc", "chaussee")
    c2 <- select(subset(AccAndJournee, AccAndJournee$CHAUSSEE2 %in% c(1:7)), "ACC", "CHAUSSEE2")
    colnames(c2) <- c("typeAcc", "chaussee")
    c3 <- select(subset(AccAndJournee, AccAndJournee$CHAUSSEE3 %in% c(1:7)), "ACC", "CHAUSSEE3")
    colnames(c3) <- c("typeAcc", "chaussee")
    c4 <- select(subset(AccAndJournee, AccAndJournee$CHAUSSEE4 %in% c(1:7)), "ACC", "CHAUSSEE4")
    colnames(c4) <- c("typeAcc", "chaussee")
    
    c <- rbind(c1,c2)
    c <- rbind(c,c3)
    c <- rbind(c,c4)
    
    c$chaussee = droplevels(c$chaussee)
    
    contingenceAcc <- table(c$typeAcc, c$chaussee)
    df_contingence <- as.data.frame.matrix(contingenceAcc)
    colnames(df_contingence) <- c("seche","humide","neige","gravier","boue","huile","deformee")
    
    afc_journee <- CA(df_contingence)
  })
  
  #1.2 Type de routes
  output$typeRoute <- renderPlot({
    accidentés <- subset(data, data$ACCIDENT == 1)
    
    # Pour savoir si un accident est aggravé on s'interesse au questionnaire approfondi. 
    # Pour savoir de quel accident il parle, on doit récupérer l'id de l'accident afin de le relier
    # à l'accident en question.
    
    
    # On sélectionne les bonnes colonnes et on enlève les accidents avec des données vides:
    AccEtRoute <- subset(select(accidentés, "CHOIXACC", "TYPEROUTE"), !is.na(TYPEROUTE))
    AccEtRoute <- subset(select(AccEtRoute, "CHOIXACC", "TYPEROUTE"), AccEtRoute$TYPEROUTE %in% c(1:6))
    
    
    # On tris les id par type d'accident:
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(1:5), "ACC"] <- 1
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(6:10), "ACC"] <- 2
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(11:15), "ACC"] <- 3
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(16:20), "ACC"] <- 4
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(21:25), "ACC"] <- 5
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(26:30), "ACC"] <- 6
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(31:35), "ACC"] <- 7
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(36:40), "ACC"] <- 8
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(41:45), "ACC"] <- 9
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(46:50), "ACC"] <- 10
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(51:55), "ACC"] <- 11
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(56:60), "ACC"] <- 12
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(61:65), "ACC"] <- 13
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(66:70), "ACC"] <- 14
    AccEtRoute[AccEtRoute$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    AccEtRoute$TYPEROUTE = droplevels(AccEtRoute$TYPEROUTE)
    
    contingenceAcc <- table(AccEtRoute$ACC, AccEtRoute$TYPEROUTE)
    
    df_contingence <- as.data.frame.matrix(contingenceAcc)
    colnames(df_contingence) <- c("Ville","Route","Autoroute", "Peripherique", "TerrainPrive", "Chemin")
    
    afc_trafic <- CA(df_contingence)
  })
  
  # 1.3
  output$presenceTiersPlot <- renderPlot({
    #On récupère les personnes ayant eu des accidents
    accidentés <- subset(data, data$ACCIDENT == 1)
    
    # Pour savoir si un accident est aggravé on s'interesse au questionnaire approfondi. 
    # Pour savoir de quel accident il parle, on doit récupérer l'id de l'accident afin de le relier
    # à l'accident en question.
    
    # Acc1: id 1 -> 5 , Acc2: id 6 -> 10, etc...
    accidentésAccidentTiers <- select(accidentés, "CHOIXACC", "AUTREUSAGER")
  
    # On enlève les accidents avec des données vides:
    accidentésTiers <- subset(accidentésAccidentTiers, !is.na(AUTREUSAGER))
    
    # On tris les id par type d'accident:
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(1:5), "ACC"]   <- 1
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(6:10), "ACC"]  <- 2
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(11:15), "ACC"] <- 3
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(16:20), "ACC"] <- 4
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(21:25), "ACC"] <- 5
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(26:30), "ACC"] <- 6
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(31:35), "ACC"] <- 7
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(36:40), "ACC"] <- 8
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(41:45), "ACC"] <- 9
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(46:50), "ACC"] <- 10
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(51:55), "ACC"] <- 11
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(56:60), "ACC"] <- 12
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(61:65), "ACC"] <- 13
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(66:70), "ACC"] <- 14
    accidentésTiers[accidentésTiers$CHOIXACC %in% c(71:75), "ACC"] <- 15
    
    # str(accidentésTiers$ACC)
    
    contingenceAcc <- table(accidentésTiers$ACC, accidentésTiers$AUTREUSAGER)
    df_contingence <- as.data.frame.matrix(contingenceAcc)
    colnames(df_contingence) <- c("Non","Oui")
    
    plot_graph <- data.frame(type = c(1:15), nb = df_contingence$Oui, Présence_Tiers = "Oui")
    plot_graph2 <- data.frame(type = c(1:15), nb = df_contingence$Non, Présence_Tiers = "Non")
    final_plot_graph <- rbind(plot_graph, plot_graph2) 
    
    if(!is.null(input$typeAccident)){
      ggplot(data = subset(final_plot_graph, type %in% input$typeAccident), aes(type, nb)) + 
      geom_bar(stat= "identity", aes(fill = Présence_Tiers)) + 
      scale_fill_manual(values=c("#004d7e", "#64be29")) + 
      labs(title="Présence d'un tiers en fonction du type d'accident", x="Type d'accident", y="Nombre de réponse")
    }
  })

  output$aggravationPlot <- renderPlot({
    
    #On récupère les personnes ayant eu des accidents
    accidentés <- subset(data, data$ACCIDENT == 1)
    
    # Pour savoir si un accident est aggravé on s'interesse au questionnaire approfondi. 
    # Pour savoir de quel accident il parle, on doit récupérer l'id de l'accident afin de le relier
    # à l'accident en question.
    
    # Acc1: id 1 -> 5 , Acc2: id 6 -> 10, etc...
    accidentésIdAccidentTrié <- select(accidentés, "CHOIXACC","AUTREUSAGER", "AGGRAVATION")
    
    # On enlève les questionnaires ou la personne n'a pas répondue à aggravation:
    accidentésIdAccidentTrié <- subset(accidentésIdAccidentTrié, AGGRAVATION != "")
    accidentésIdAccidentTrié <- subset(accidentésIdAccidentTrié, AGGRAVATION != "0")
    accidentésIdAccidentTrié <- subset(accidentésIdAccidentTrié, AUTREUSAGER = 1)
    #On nettoie également les facteurs
    accidentésIdAccidentTrié$AGGRAVATION = droplevels(accidentésIdAccidentTrié$AGGRAVATION)
    
    #Nettoyage:
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(1:5), "ACC"] <- "ACC1"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(6:10), "ACC"] <- "ACC2"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(11:15), "ACC"] <- "ACC3"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(16:20), "ACC"] <- "ACC4"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(21:25), "ACC"] <- "ACC5"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(26:30), "ACC"] <- "ACC6"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(31:35), "ACC"] <- "ACC7"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(36:40), "ACC"] <- "ACC8"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(41:45), "ACC"] <- "ACC9"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(46:50), "ACC"] <- "ACC10"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(51:55), "ACC"] <- "ACC11"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(56:60), "ACC"] <- "ACC12"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(61:65), "ACC"] <- "ACC13"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(66:70), "ACC"] <- "ACC14"
    accidentésIdAccidentTrié[accidentésIdAccidentTrié$CHOIXACC %in% c(71:75), "ACC"] <- "ACCAUTRE"
    
    #On créé les différents niveaux de valeurs
    levels(accidentésIdAccidentTrié$ACC) <- c("ACC1", "ACC2","ACC3","ACC4","ACC5","ACC6","ACC7","ACC8","ACC9","ACC10","ACC11","ACC12","ACC13","ACC14","ACCAUTRE")
    
    
    # On va récupérer les accidents ou les gens estiment que la réaction d'un tiers à aggravée la situation, etc... :
    #accidentAggravé <- subset(accidentésIdAccidentTrié, AGGRAVATION == "1")
    # accidentPasAggravé <- subset(accidentésIdAccidentTrié, AGGRAVATION == "2")
    # accidentNSPAggravé <- subset(accidentésIdAccidentTrié, AGGRAVATION == "NSP")
    
    #Construction des données pour faire l'AFC (tableau de contingences): 
    afc_raws <- table(accidentésIdAccidentTrié$ACC, accidentésIdAccidentTrié$AGGRAV)
    
    # On a deux valriables qualitatives: l'accident et la réponse à aggravation. 
    # On va donc faire une AFC:
    res_afc <- CA(afc_raws)
    
  })
  
  # 2.1
  output$representationJeunesPlot <- renderPlot({
    
    dateNaissance <- as.Date(data$DATE_NAISS, format="%d%b%Y")
    anneesNaissance <- as.numeric(format(dateNaissance, "%Y"))
    age <- 2018 - anneesNaissance
    
    accidentEtAge <- select(subset(data, data$ACCIDENT == 1), "NBACC1", "NBACC2","NBACC3","NBACC4","NBACC5","NBACC6","NBACC7","NBACC8", "NBACC9","NBACC10","NBACC11","NBACC12","NBACC13","NBACC14","DATE_NAISS")
    accidentEtAge$age <- 2018 - as.numeric(format(as.Date(accidentEtAge$DATE_NAISS, format="%d%b%Y"), format="%Y")) 
    
    accidentPropre <- subset(accidentEtAge, accidentEtAge$age < 100)
    accidentPropre$tranche <- accidentPropre$age
    levels(accidentPropre$tranche) <- c(levels(accidentPropre$tranche), "vingtaine", "trentaine", "quarantaine", "cinquantaine", "soixantaine", "70aine", "80aine", "90aine")
    accidentPropre[accidentPropre$tranche %in% c(20:29), "tranche"] <- "vingtaine"
    accidentPropre[accidentPropre$tranche %in% c(30:39), "tranche"] <- "trentaine"
    accidentPropre[accidentPropre$tranche %in% c(40:49), "tranche"] <- "quarantaine"
    accidentPropre[accidentPropre$tranche %in% c(50:59), "tranche"] <- "cinquantaine"
    accidentPropre[accidentPropre$tranche %in% c(60:69), "tranche"] <- "70aine"
    accidentPropre[accidentPropre$tranche %in% c(70:79), "tranche"] <- "80aine"
    accidentPropre[accidentPropre$tranche %in% c(80:89), "tranche"] <- "90aine"
    
    
    accidentPropre$DATE_NAISS <- NULL 
    
    maFonction <- function(final, testEncore){
      test <- 0 
      for(i in 1:7){
        total <- 0 
        for(j in 1:ncol(testEncore)){ 
          total <- total + as.numeric(colnames(testEncore)[j]) * testEncore[i,j]
        }
        test[i] <- total
      } 
      test <- as.table(test)
      rownames(test) <- rownames(testEncore)
      final <- rbind(final,test)
      rownames(final) <- NULL
      return(final)
    }
    
    final <- 0
    final <- as.table(final)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC1)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC2)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC3)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC4)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC5)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC6)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC7)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC8)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC9)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC10)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC11)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC12)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC13)
    final <- maFonction(final, totalAccidentX)
    totalAccidentX <- table(accidentPropre$tranche, accidentPropre$NBACC14)
    final <- maFonction(final, totalAccidentX)
    final <- final[-1,]
    q
    finalProportion <- final 
    for(i in 1:nrow(final)){
      finalProportion[i,] <- prop.table(final[i,])
    }
    
    finalProportionAllType <- final[1,]
    for(i in 1:ncol(final)){
      finalProportionAllType[i] <- sum(final[,i])
    }
    finalProportionAllType <- prop.table(finalProportionAllType)
    
    dat <- data.frame(
      type = c(1:14),
      proportion = finalProportion[,"vingtaine"]
    )
    if(!is.null(input$typeAccident)){
      ggplot(data= subset(dat, type %in% input$typeAccident), aes(x=type, y=proportion)) +
      geom_bar(stat="identity", fill = "#004d7e") + 
      geom_hline(aes(yintercept = finalProportionAllType["vingtaine"]), color="#64be29", linetype="dashed") + 
      theme_light()
    }
  })
  
  # 2.2
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

})
