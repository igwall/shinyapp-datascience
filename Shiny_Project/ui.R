#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


dashboardPage(skin = "blue",
  dashboardHeader(title = "Riders Analytics"),
  dashboardSidebar(
    menuItem("Général", tabName = "general", icon = icon("arrow-right")),
    menuItem("Sections", tabName = "sections", icon = icon("arrow-right"),
             menuSubItem("Etat du conducteur", tabName = "etat", icon = icon("arrow-right")),
             menuSubItem("Habitude de conduite", tabName = "habit", icon = icon("arrow-right")),
             menuSubItem("Causes exterieures", tabName = "ext", icon = icon("arrow-right")),
             menuSubItem("Causes dues à un tiers", tabName = "tiers", icon = icon("arrow-right"))),
    menuItem("Age", tabName = "age", icon = icon("arrow-right")),
    menuItem("Experience", tabName = "expe", icon = icon("arrow-right")),
    menuItem("Usage", tabName = "usage", icon = icon("arrow-right")),
    checkboxGroupInput("typeAccident", "Types d'accidents:", c("Accident 1"  =  1,
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
                                                               "Accident 14" = 14))),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Types accident",
                  checkboxGroupInput("typeAccident", "Types d'accidents:", c("Accident 1"  =  1,
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
                                                                             "Accident 14" = 14)
                                     )
                  ),
                
                box(title = "Répartition de l'expérience", plotOutput("experienceplot"))
              )
        ),
              
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)