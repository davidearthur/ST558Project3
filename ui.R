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
    tabItem("about", h2("About"),
              "This app allows the user to explore and analyze a data set from 1988 related to diagnosis of heart disease at a location of the Cleveland Clinic.  The data set includes 14 measurements taken on 303 patients at 8 categorical predictors, 5 continuous predictors, and a categorical response.  The response variable (num) is ordinal with 5 levels, representing the absence of heart disease (num = 0), or various degrees of heart disease (num = 1, 2, 3, 4).  For the purposes of this app, a binary response variable was created (disease), representing simply the presence or absence of heart disease (disease = 'yes'/'no').  For more information or to download the data set, see ",
              a(href="https://archive.ics.uci.edu/ml/datasets/heart+disease", target="_blank", "UCI Machine Learning"),
              br(),
              br(),
              "The Data page allows the user to view the data set in a table, with the options of choosing which variables to include, sorting by any variable, and searching.  The user can also download the data set with their chosen variables to a .csv file.  The Data Exploration page allows the user to view various summaries and plots of the data, again with a choice of variables and an option to download plots.  The plots are interactive, allowing zoom, pan, and other features.",
              br(),
              br(),
              "The Model Fit page is subdivided into 3 pages.  Modeling Info describes 3 types of statistical models that can be applied to the heart disease data set.  Model Fitting allows the user to fit the 3 models, choosing which variables to include for each.  Prediction allows the user to input their own values for each of the variables, and outputs the predicted response from each of the models.",
            br(),
            br(),
            "Image from ", a(href = "https://www.unc.edu/posts/2020/11/03/data-driven-intervention-reduces-cardiovascular-risk-across-our-state/", target = "_blank", "article on data-driven approach to heart health care"), "(unrelated to data set used in this app):",
            imageOutput("graphic")
    ),
    tabItem("data", h2("Data"),
            fluidRow(
              box(title = "Description of variables",
                  width = 12,
                  tags$li("disease (response): yes/no, diagnosis of heart disease (> 50% diameter narrowing in any major vessel)"),
                  br(),
                  tags$li("age: in years"),
                  br(),
                  tags$li("sex: yes/no"),
                  br(),
                 tags$li("cp: chest pain type (1 = typical angina, 2 = atypical angina, 3 = non-anginal pain, 4 = asymptomatic)"),
                  br(),
                  tags$li("trestbps: resting blood pressure (in mm Hg on admission to the hospital)"),
                  br(),
                  tags$li("chol: serum cholesterol in mg/dl"),
                  br(),
                  tags$li("fbs: (fasting blood sugar > 120 mg/dl)  (1 = true; 0 = false)"),
                  br(),
                  tags$li("restecg: resting electrocardiographic results (0 = normal, 1 = having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV), 2 = showing probable or definite left ventricular hypertrophy by Estes' criteria)"),
                  br(),
                  tags$li("thalach: maximum heart rate achieved"),
                  br(),
                  tags$li("exang: exercise induced angina (1 = yes, 0 = no)"),
                  br(),
                  tags$li("oldpeak: ST depression induced by exercise relative to rest"),
                  br(),
                  tags$li("slope: the slope of the peak exercise ST segment (1 = upsloping, 2 = flat, 3 = downsloping)"),
                  br(),
                  tags$li("ca: number of major vessels (0-3) colored by flourosopy"),
                  br(),
                  tags$li("thal: (3 = normal, 6 = fixed defect, 7 = reversable defect)"),
                  br(),
                  tags$li("num (alternate response): angiographic disease status")
              )
            ),
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
              downloadButton("dlTable", "Download data set with selected variables")
            )
    ),
    tabItem("dataExploration", h2("Data Exploration"),
            fluidRow(
                box(
                  width = 4,
                  uiOutput("plotVariable1choices"),
                  uiOutput("plotVariable1filter"),
                  checkboxInput("numVars", "Include a 2nd variable?"),
                  uiOutput("plotVariable2choices"),
                  uiOutput("plotVariable2filter"),
                  uiOutput("nullTest")
                ),
                box(
                  width = 8,
                  uiOutput("singleVarChart"),
                  uiOutput("chart2vars"),
                  uiOutput("corr")
                )
            ),
            fluidRow(
                box(
                  uiOutput("plotSingleVar"),
                  uiOutput("plotSingleVar2"),
                  uiOutput("plot2vars")
                )
            )
    ),
    tabItem("info", h2("Modeling Info"),
            fluidRow(
              box(title = "Logistic Regression Models",
                  uiOutput("glmDesc")
              ),
              box(title = "Classification Tree Models",
                  uiOutput("treeDesc")
              )
            ),
            fluidRow(
              box(title = "Random Forest Models",
                  width = 12,
                  uiOutput("rfDesc")
              )
            )
    ),
    tabItem("fitting", h2("Model Fitting"),
            fluidRow(
              column(width = 6,
                box(width = NULL,
                    sliderInput("trainingProp", "What proportion of data to use as training set?",
                                min = 0.1, max = 0.9, value = 0.7
                    ),
                    sliderInput("numFolds", "How many folds to use in cross validation?",
                                min = 2, max = 15, value = 10,
                                step = 1, ticks = TRUE
                    ),
                    actionButton("fit", "Fit Models")
                )
              ),
              column(width = 6,
                box(title = "Comparison of performance on training set",
                    width = NULL,
                    tableOutput("comparison")
                ),
                box(title = "Comparison of mis-classification rates on test set",
                    width = NULL,
                    tableOutput("performanceComp")
                )
              )
            ),
            fluidRow(
              column(width = 4,
                box(width = NULL,
                  uiOutput("varChoiceModel1")
                ),
                box(title = "GLM fit on training set",
                    width = NULL,
                    verbatimTextOutput("model1Text")
                )
              ),
              column(width = 8,
                box(title = "GLM Summary",
                    width = NULL,
                    collapsible = TRUE,
                    verbatimTextOutput("model1Summary")
                )
              )
            ),
            fluidRow(
              box(width = 4,
                uiOutput("varChoiceModel2")
              ),
              box(title = "Classification Tree fit on training set",
                  width = 8,
                  verbatimTextOutput("model2Text")
              )
            ),
            fluidRow(
              box(width = 4,
                uiOutput("varChoiceModel3")
              ),
              box(title = "Random Forest fit on training set",
                  width = 8,
                  verbatimTextOutput("model3Text")
              )
            ),
            fluidRow(
              box(title = "GLM performance on test set",
                verbatimTextOutput("performance1")
              ),
              box(title = "Classification Tree performance on test set",
                verbatimTextOutput("performance2")
              )
            ),
            fluidRow(
              box(title = "Random Forest performance on test set",
                verbatimTextOutput("performance3")
              )
            )
    ),
    tabItem("prediction", h2("Prediction"),
            fluidRow(
              box(title = "Categorical variables",
                uiOutput("factorsInput")
              ),
              box(title = "Continuous variables",
                uiOutput("contInput")
              ),
              box("Predictions of fitted models based on selected values of variables",
                tableOutput("predictions")
              )
            )
    )
  )
)

dashboardPage(
  dashboardHeader(title = "Heart Disease Data"),
  sidebar,
  body
)
