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
  
  output$presenceTiersPlot <- renderPlot({
    #On récupère les personnes ayant eu des accidents
    accidentés <- subset(data, data$ACCIDENT == 1)
    
    # Pour savoir si un accident est aggravé on s'interesse au questionnaire approfondi. 
    # Pour savoir de quel accident il parle, on doit récupérer l'id de l'accident afin de le relier
    # à l'accident en question.
    
    # Acc1: id 1 -> 5 , Acc2: id 6 -> 10, etc...
    accidentésAccidentTiers <- select(accidentés, "CHOIXACC", "AUTREUSAGER")
    str(accidentésAccidentTiers$AUTREUSAGER)
    
    
    # On enlève les accidents avec des données vides:
    accidentésTiers <- subset(accidentésAccidentTiers, !is.na(AUTREUSAGER))
    str(accidentésTiers$AUTREUSAGER)
    
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
    plot(contingenceAcc)
    df_contingence <- as.data.frame.matrix(contingenceAcc)
    colnames(df_contingence) <- c("Non","Oui")
    
    plot_graph <- data.frame(type = c(1:15), nb = df_contingence$Oui, Présence_Tiers = "Oui")
    plot_graph2 <- data.frame(type = c(1:15), nb = df_contingence$Non, Présence_Tiers = "Non")
    final_plot_graph <- rbind(plot_graph, plot_graph2) 
    
    ggplot(data = subset(final_plot_graph, ACC %in% input$typeAccident), aes(type, nb)) + 
    geom_bar(stat= "identity", aes(fill = Présence_Tiers)) + 
    scale_fill_manual(values=c("#004d7e", "#64be29")) + 
    labs(title="Présence d'un tiers en fonction du type d'accident", x="Type d'accident", y="Nombre de réponse")
  })
  
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
    
    ggplot(data= subset(dat, type %in% input$typeAccident), aes(x=type, y=proportion)) +
      geom_bar(stat="identity", fill = "#004d7e") + 
      geom_hline(aes(yintercept = finalProportionAllType["vingtaine"]), color="#64be29", linetype="dashed") + 
      theme_light()
    
    
  })
  
})
