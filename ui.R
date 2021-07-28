#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("info")),
    menuItem("Data", tabName = "data", icon = icon("table")),
    menuItem("Data Exploration", tabName = "dataExploration", icon = icon("chart-bar")),
    menuItem("Modeling", tabName = "modeling", icon = icon("chart-line"),
             menuSubItem("Modeling Info", tabName = "info"),
             menuSubItem("Model Fitting", tabName = "fitting"),
             menuSubItem("Prediction", tabName = "prediction")
             )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("about", h2("About")
            ),
    tabItem("data", h2("Data"),
            fluidRow(
              box(
                width = 2,
                checkboxGroupInput("tableVariables", "Variables to include in table",
                                 choices = names(heartData),
                                 inline = FALSE,
                                 selected = names(heartData)
                                 )
                ),
              box(
                width = 10,
                DTOutput("fullTable")#,
                # style = "overflow-x: scroll"
                )
              ),
            fluidRow(
              downloadButton("dlTable", "Download data set with selected variables"))
            ),
    tabItem("dataExploration", h2("Data Exploration")
            ),
    tabItem("info", h2("Modeling Info")
            ),
    tabItem("fitting", h2("Model Fitting")
            ),
    tabItem("prediction", h2("Prediction")
            )
  )
)

dashboardPage(
  dashboardHeader(title = "Heart Disease Data"),
  sidebar,
  # dashboardSidebar(
  #     sidebarMenu(
  #         menuItem("About", tabName = "about", icon = icon("info")),
  #         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  #         menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
  #         menuItem("ChartsTest", icon = icon("bar-chart-o"), startExpanded = TRUE,
  #                  menuSubItem("Sub-item 1", tabName = "subitem1"),
  #                  menuSubItem("Sub-item 2", tabName = "subitem2")
  #         )
  #     ),
  #     textOutput("res")
  # ),
  body
)
