#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(""),
  navbarPage("Étude des différences entre les accidents",
             tabPanel("Général"),
             navbarMenu("Sections",
                        tabPanel("Etat du conducteur"),
                        tabPanel("Habitude de conduite"),
                        tabPanel("Causes exterieures"),
                        tabPanel("Causes dues à un tiers")
             ),
             tabPanel("Âges"),
             tabPanel("Expériences",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("typeAccident", "Types d'accidents:",
                                             c("Accident 1"  =  1,
                                               "Accident 2"  =  2,
                                               "Accident 3"  =  3,
                                               "Accident 4"  =  4,
                                               "Accident 5"  =  5,
                                               "Accident 6"  =  6,
                                               "Accident 7"  =  7,
                                               "Accident 8"  =  8,
                                               "Accident 9"  =  9,
                                               "Accident 10" = 10,
                                               "Accident 11" = 11,
                                               "Accident 12" = 12,
                                               "Accident 13" = 13,
                                               "Accident 14" = 14))
                        ),
                        mainPanel(
                          plotOutput("experienceplot")
                        
                      ),
             
             
             
             
             
             
             
             tabPanel("Usages")
  ),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("typeAccident", "Types d'accidents:",
                         c("Accident 1"  =  1,
                           "Accident 2"  =  2,
                           "Accident 3"  =  3,
                           "Accident 4"  =  4,
                           "Accident 5"  =  5,
                           "Accident 6"  =  6,
                           "Accident 7"  =  7,
                           "Accident 8"  =  8,
                           "Accident 9"  =  9,
                           "Accident 10" = 10,
                           "Accident 11" = 11,
                           "Accident 12" = 12,
                           "Accident 13" = 13,
                           "Accident 14" = 14))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
    )
  )
))
