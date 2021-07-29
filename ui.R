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
                uiOutput("varChoiceTable")
                ),
              box(
                width = 10,
                DTOutput("fullTable")
                )
              ),
            fluidRow(
              downloadButton("dlTable", "Download data set with selected variables"))
    ),
    tabItem("dataExploration", h2("Data Exploration"),
            fluidRow(
              column(width = 4,
                box(
                  width = NULL,
                  uiOutput("plotVariable1choices"),
                  uiOutput("plotVariable2choices")
                ),
                box(
                  width = NULL,
                  uiOutput("singleVarChart"),
                  uiOutput("chart2vars"),
                  uiOutput("corr")
                )
              ),
              column(width = 8,
                box(
                  width = NULL,
                  uiOutput("plotSingleVar"),
                  uiOutput("plotSingleVar2"),
                  uiOutput("plot2vars")
                )
              )
            )
    ),
    tabItem("info", h2("Modeling Info")
    ),
    tabItem("fitting", h2("Model Fitting"),
            fluidRow(
              box(width = 5,
                sliderInput("trainingProp", "What proportion of data to use as training set?",
                          min = 0.1, max = 0.9, value = 0.7)
              ),
              box(width = 2,
                actionButton("fit", "Fit Models")
              ),
              box(width = 5,
                tableOutput("comparison")
              )
            ),
            fluidRow(
              box(
                uiOutput("varChoiceModel1"),
                uiOutput("intChoiceModel1"),
                uiOutput("autoChoiceModel1")
              ),
              box(
                verbatimTextOutput("model1Text")
              )
            ),
            fluidRow(
              box(
                uiOutput("varChoiceModel2")
              ),
              box(
                verbatimTextOutput("model2Text")
              )
            ),
            fluidRow(
              box(
                uiOutput("varChoiceModel3")
              ),
              box(
                verbatimTextOutput("model3Text")
              )
            )
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
