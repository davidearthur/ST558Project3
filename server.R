#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# 
library(shinydashboard)
library(DT)
library(MASS)
library(tidyverse)
library(GGally)
library(plotly)
library(ggmosaic)
library(knitr)
library(xtable)
library(caret)

select <- dplyr::select
# read in data
heartData <- read_csv("processed.cleveland.data",
                      col_names = c("age", "sex", "cp", "trestbps", "chol", "fbs",
                                    "restecg", "thalach", "exang", "oldpeak",
                                    "slope", "ca", "thal", "num"
                                    ),
                      na = "?"
)

heartData <- heartData %>%
  mutate(across(c(sex, cp, fbs, restecg, exang, slope, thal), as_factor)) %>%
  mutate(across(c(ca, num), ordered)) %>%
  mutate(disease = factor(num != 0)) %>%
  mutate(disease = fct_recode(disease, Yes = "TRUE", No = "FALSE")) %>%
  mutate(sex = fct_recode(sex, Male = "1", Female = "0")) %>%
  select(disease, everything()) %>%
  na.omit()

function(input, output, session) {

  # About page graphic
  output$graphic <- renderImage(list(src = "hearthealth.jpeg", height = "250px"), deleteFile = FALSE)
  # get variables to include in data table
  output$varChoiceTable <- renderUI({
    checkboxGroupInput("tableVariables", "Variables to include in table",
                       choices = names(heartData),
                       inline = FALSE,
                       selected = names(heartData)
    )
  })
  filterTableData <- reactive({
    filteredTableData <- heartData %>% select(input$tableVariables)
  })
  # create data table and download button
  output$fullTable <- renderDT({
    datatable(filterTableData(),
              options = list(scrollX = TRUE))
  })
  output$dlTable <- downloadHandler(
    filename = "HeartData.csv",
    content = function(con) {
      write.csv(filterTableData(), con)
    }
  )
  # get 1st variable to summarize and plot
  output$plotVariable1choices <- renderUI({
    selectInput("plotVariable1", "First variable to summarize",
                choices = names(heartData),
                selected = "disease"
    )
  })
  # get filter values variable 1
  output$plotVariable1filter <- renderUI({
    req(input$plotVariable1)
    var1 <- (heartData %>%
               select(input$plotVariable1))[[1]]
    if(is.factor(var1)){
      checkboxGroupInput("var1FactorFilterValues", "Filter by levels of 1st Variable:",
                         choices = levels(var1),
                         selected = levels(var1)
                           )
    } else {
      sliderInput("var1ContFilterValues", "Filter by value of 1st variable:", min = min(var1), max = max(var1), value = c(min(var1), max(var1)))
    }
  })
  # get 2nd variable to summarize and plot
  output$plotVariable2choices <- renderUI({
    req(input$numVars)
    if(input$numVars == TRUE){
      selectInput("plotVariable2", "Second variable to summarize (optional)",
                  choices = names(heartData %>% select(-input$plotVariable1))
      )
    }
  })
  # get filter values variable 2
  output$plotVariable2filter <- renderUI({
    req(input$plotVariable2)
    var2 <- (heartData %>%
               select(input$plotVariable2))[[1]]
    if(is.factor(var2)){
      checkboxGroupInput("var2FactorFilterValues", "Filter by levels of 2nd Variable:",
                         choices = levels(var2),
                         selected = levels(var2)
      )
    } else {
      sliderInput("var2ContFilterValues", "Filter by value of 2nd variable:", min = min(var2), max = max(var2), value = c(min(var2), max(var2)))
    }
  })
  # get variables and filter values for plots and summaries
  heartDataPlot <- reactive({
    req(input$plotVariable1)
    var1 <- (heartData %>%
               select(input$plotVariable1))[[1]]
    if(is.factor(var1)){
      filtered <- (heartData %>% filter(!!sym(input$plotVariable1) %in% input$var1FactorFilterValues))
    } else {
      filtered <- (heartData %>% 
                     filter(!!sym(input$plotVariable1) >=  input$var1ContFilterValues[1]) %>%
                     filter(!!sym(input$plotVariable1) <=  input$var1ContFilterValues[2]))
    }
    if(!is.null(input$plotVariable2)){
      var2 <- (heartData %>%
                 select(input$plotVariable2))[[1]]
      if(is.factor(var2)){
        filtered <- (filtered %>% filter(!!sym(input$plotVariable2) %in% input$var2FactorFilterValues))
      } else {
        filtered <- (filtered %>% 
                       filter(!!sym(input$plotVariable2) >=  input$var2ContFilterValues[1]) %>%
                       filter(!!sym(input$plotVariable2) <=  input$var2ContFilterValues[2]))
      }
    }
    return(filtered)
  })
  # choose types of summaries and plots based on variables selected
  output$plotSingleVar <- renderUI({
    if(input$numVars == FALSE){
      if(is.factor((heartData %>% select(input$plotVariable1))[[1]])){
        plotlyOutput("singleVarBar")
      } else {
        selectInput("singleVarChoice", "Which type of plot?",
                  choices = c("Histogram", "Boxplot"),
                  selected = "Histogram"
        )
      }
    } 
  })
  output$plotSingleVar2 <- renderUI({
    req(input$singleVarChoice)
    if(input$numVars == FALSE & 
       is.numeric((heartData %>% select(input$plotVariable1))[[1]])){
      if(input$singleVarChoice == "Histogram"){
        plotlyOutput("singleVarHist")
      } else {
        if(input$singleVarChoice == "Boxplot"){
          plotlyOutput("singleVarBox")
        }
      }
    }
  })
  output$singleVarChart <- renderUI({
    if(input$numVars == FALSE){
      tableOutput("singleVarSummary")
    }
  })
  output$chart2vars <- renderUI({
    req(input$numVars, input$plotVariable2)
    if(input$numVars == TRUE){
      if(is.factor((heartData %>% select(input$plotVariable1))[[1]])){
        if(is.factor((heartData %>% select(input$plotVariable2))[[1]])){
          tagList(
            textOutput("factorFactorSumTitle"),
            tableOutput("factorFactorSummary"),
            verbatimTextOutput("factorFactorSummary2")
          )
        } else {
          if(is.numeric((heartData %>% select(input$plotVariable2))[[1]])){
            tagList(
              textOutput("factorContSumTitle"),
              tableOutput("factorContSummary")
            )
          }
        }
      } else {
        if(is.numeric((heartData %>% select(input$plotVariable1))[[1]])){
          if(is.factor((heartData %>% select(input$plotVariable2))[[1]])){
            tagList(
              textOutput("contFactorSumTitle"),
              tableOutput("contFactorSummary")
            )
            } else {
            if(is.numeric((heartData %>% select(input$plotVariable2))[[1]])){
              tableOutput("contContSummary")
            }
          }
        }
      }
    }
  })
  output$corr <- renderUI({
    req(input$plotVariable2)
    if(is.numeric((heartData %>% select(input$plotVariable1))[[1]])){
      if(is.numeric((heartData %>% select(input$plotVariable2))[[1]])){
        textOutput("corrText")
      }
    }
  })
  output$plot2vars <- renderUI({
    req(input$numVars, input$plotVariable2)
    if(input$numVars == TRUE){
      if(is.factor((heartData %>% select(input$plotVariable1))[[1]])){
        if(is.factor((heartData %>% select(input$plotVariable2))[[1]])){
          plotlyOutput("factorFactorPlot")
        } else {
          if(is.numeric((heartData %>% select(input$plotVariable2))[[1]])){
            plotlyOutput("factorContPlot")
          }
        }
      } else {
        if(is.numeric((heartData %>% select(input$plotVariable1))[[1]])){
          if(is.factor((heartData %>% select(input$plotVariable2))[[1]])){
            plotlyOutput("contFactorPlot")
          } else {
            if(is.numeric((heartData %>% select(input$plotVariable2))[[1]])){
              plotlyOutput("contContPlot")
            }
          }
        }
      }
    }
  })
  
  # Descriptions of models
  output$glmDesc <- renderUI({
    withMathJax(
      helpText("Logistic regression is a type of Generalized Linear Model (GLM) used to model a binary response variable, such as yes/no,  male/female, success/failure, or 0/1.  Predictor variables can be categorical or continuous.  The parameters of the model are linear, and multiplying parameters by variables equals the log-odds (logit) of success:
      $$log(\\frac{\\theta}{1-\\theta}) = \\beta_0 + \\beta_1X_1 + … + \\beta_pX_p$$ where \\(\\theta = P(success)\\) and \\(p = number of predictors\\).",
      br(),
      "Probability of success can be calculated using the ilogit function:  $$\\theta = \\frac{e^{( \\beta_0 + \\beta_1X_1 + … + \\beta_pX_p)}}{1 + e^{( \\beta_0 + \\beta_1X_1 + … + \\beta_pX_p)}}$$  For classification purposes, a cutoff for \\(p\\) can chosen (e.g. 0.5), so that \\(p>cutoff\\) will be classified as a success, and \\(p<cutoff\\) will be classified as a failure.  Variations of logistic regression can also be applied to response variables with more than 2 levels.",
      br(),
      br(),
      "Logistic regression has the benefit of relatively straightforward interpretation and efficient computation, but has the drawback of requiring certain assumptions regarding the variance and distribution of errors in the model."
      )
    )
    
  })
  output$treeDesc <- renderUI({
    helpText(
      "Classification trees make predictions by dividing the predictor space into a number of regions, and determining which region the predictor values of the new observation fall into by applying a series of splits, each based on the value of a single predictor.  The response for the new observation is then predicted to be the predominant class observed in the region.  First a large tree is grown, with the goal of maximizing prediction accuracy, resulting in a tree with many regions, each containing a small number of observations.  But this complex tree will generally be overfit, with low bias and high variance, so it gets pruned back to an optimal size, determined by cross validation, that will have higher bias but lower variance, and ideally perform better when predicting on new data.",
      br(),
      br(),
      "Classification trees have several benefits.  Their output is easy to interpret, and predictors don’t need to be scaled.  Also, they don’t require any statistical assumptions, and they inherently select variables and account for interactions between variables.  Drawbacks include the need to prune the tree, the fact that small changes in data can drastically change output, and their greedy algorithm (splits are made one a time, without considering potential splits further down the tree)."
    )
  })
  output$rfDesc <- renderUI({
    helpText(
      "Random forest models are an improvement on bagged tree models, which improve on basic decision trees by using the bootstrap to take many samples from the training data set and producing an unpruned tree from each sample, then averaging the predictions of those trees to get the bagged tree model.  The averaging of hundreds of high-variance trees results in a much lower variance model.  The random forest model is a further improvement on the bagged tree model.  It works by decorrelating the trees that are generated and averaged together.  In a bagged tree model, many of the trees can end up being similar, with the main splits dominated by the strongest predictor(s).  The correlation between these trees means that averaging them results in a smaller reduction in variance than desired.  To remedy this, random forest models consider only a random subset of predictors for each split, resulting in less correlation between trees, and lower variance in the final model.  The number of predictors considered for each split is a tuning parameter, whose value can be chosen using cross validation.",
      br(),
      br(),
      "Random forest models share some benefits with classification trees--they require no statistical assumptions, and they automatically select variables and account for interactions between variables.  However, they improve on the predictive ability of classification trees, but at the expense of interpretability."
    )
  })
  # get variables to include in GLM model
  output$varChoiceModel1 <- renderUI({
    checkboxGroupInput("modelVariables1", "Variables to include in GLM",
                       choices = names(heartData %>% select(-c(disease, num))),
                       inline = FALSE,
                       selected = names(heartData %>% select(-c(disease, num)))
    )
  })
  # get variables to include in Tree model
  output$varChoiceModel2 <- renderUI({
    checkboxGroupInput("modelVariables2", "Variables to include in Classification Tree Model",
                       choices = names(heartData %>% select(-c(disease, num))),
                       inline = FALSE,
                       selected = names(heartData %>% select(-c(disease, num)))
    )
  })
  # get variables to include in random forest model
  output$varChoiceModel3 <- renderUI({
    checkboxGroupInput("modelVariables3", "Variables to include in Random Forest Model",
                       choices = names(heartData %>% select(-c(disease, num))),
                       inline = FALSE,
                       selected = names(heartData %>% select(-c(disease, num)))
    )
  })
  # get levels of factor variables for prediction
  output$factorsInput <- renderUI({
    tagList(
        selectInput("sex", "sex", choices = c(levels(heartData$sex))),
        selectInput("cp", "cp", choices = c(levels(heartData$cp))),
        selectInput("fbs", "fbs", choices = c(levels(heartData$fbs))),
        selectInput("restecg", "restecg", choices = c(levels(heartData$restecg))),
        selectInput("exang", "exang", choices = c(levels(heartData$exang))),
        selectInput("slope", "slope", choices = c(levels(heartData$slope))),
        selectInput("ca", "ca", choices = c(levels(heartData$ca))),
        selectInput("thal", "thal", choices = c(levels(heartData$thal)))
    )
  })
  # get values of continuous variables for prediction
  output$contInput <- renderUI({
    tagList(
        numericInput("age", "age", value = mean(heartData$age, min = 0, max = 150)),
        numericInput("trestbps", "trestbps", value = mean(heartData$trestbps), min = 0, max = 2 * max(heartData$trestbps)),
        numericInput("chol", "chol", value = mean(heartData$chol), min = 0, max = 2 * max(heartData$chol)),
        numericInput("thalach", "thalach", value = mean(heartData$thalach), min = 0, max = 2 * max(heartData$thalach)),
        numericInput("oldpeak", "oldpeak", value = mean(heartData$oldpeak), min = 0, max = 2 * max(heartData$oldpeak))
    )
  })
  # create summary tables
  output$singleVarSummary <- renderTable({
    req(input$plotVariable1)
    if(is.factor((heartDataPlot() %>% select(input$plotVariable1))[[1]])){
      count(heartDataPlot(), !!sym(input$plotVariable1))
    } else {
      if(is.numeric((heartDataPlot() %>% select(input$plotVariable1))[[1]])){
        xtable(summary(heartDataPlot() %>% select(input$plotVariable1)))
      }
    }
  })
  output$singleVarBar <- renderPlotly({
    req(input$plotVariable1)
    figBar <- heartDataPlot() %>% count(!!sym(input$plotVariable1)) %>%
      plot_ly(x = as.formula(paste0("~", input$plotVariable1)), y = ~n, type = "bar") 
    figBar
  })
  output$singleVarBox <- renderPlotly({
    req(input$plotVariable1)
    figBox <- heartDataPlot() %>%
      plot_ly(y = as.formula(paste0("~", input$plotVariable1)), type = "box")
    figBox
  })
  output$singleVarHist <- renderPlotly({
    req(input$plotVariable1)
    figHist <- heartDataPlot() %>% 
      plot_ly(x = as.formula(paste0("~", input$plotVariable1)), type = "histogram")
    figHist
  })
  output$factorFactorSummary <- renderTable({
    req(input$numVars, input$plotVariable2)
    xtable(table((heartDataPlot() %>% select(input$plotVariable1))[[1]], (heartDataPlot() %>% select(input$plotVariable2))[[1]]))
  },
    rownames = TRUE
  )
  output$factorFactorSummary2 <- renderPrint({
    req(input$numVars, input$plotVariable2)
    summary(table((heartDataPlot() %>% select(input$plotVariable1))[[1]], (heartDataPlot() %>% select(input$plotVariable2))[[1]]))
  }
  )
  output$factorFactorSumTitle <- renderText({
    req(input$plotVariable2)
    paste0(input$plotVariable1, " by ", input$plotVariable2, " contingency table")
  })
  output$factorContSummary <- renderTable({
    req(input$numVars, input$plotVariable2)
    heartDataPlot() %>% select(input$plotVariable1, !!sym(input$plotVariable2)) %>%
      group_by(!!sym(input$plotVariable1)) %>%
      summarize("Min" = min(!!sym(input$plotVariable2)), "1st Qu." = quantile(!!sym(input$plotVariable2))["25%"],  "Median" = median(!!sym(input$plotVariable2)), "Mean" = mean(!!sym(input$plotVariable2)), "3rd Qu." = quantile(!!sym(input$plotVariable2))["75%"], "Max" = max(!!sym(input$plotVariable2))) %>%
      xtable
  })
  output$factorContSumTitle <- renderText({
    req(input$plotVariable2)
    paste0("Summary of ", input$plotVariable2, " grouped by levels of ", input$plotVariable1)
  })
  output$contFactorSummary <- renderTable({
    req(input$numVars, input$plotVariable2)
    heartDataPlot() %>% select(!!sym(input$plotVariable2), !!sym(input$plotVariable1)) %>%
      group_by(!!sym(input$plotVariable2)) %>%
      summarize("Min" = min(!!sym(input$plotVariable1)), "1st Qu." = quantile(!!sym(input$plotVariable1))["25%"],  "Median" = median(!!sym(input$plotVariable1)), "Mean" = mean(!!sym(input$plotVariable1)), "3rd Qu." = quantile(!!sym(input$plotVariable1))["75%"], "Max" = max(!!sym(input$plotVariable1))) %>%
      xtable 
  })
  output$contFactorSumTitle <- renderText({
    req(input$plotVariable2)
    paste0("Summary of ", input$plotVariable1, " grouped by levels of ", input$plotVariable2)
  })
  output$contContSummary <- renderTable({
    req(input$numVars, input$plotVariable2)
    heartDataPlot() %>% select(!!sym(input$plotVariable1), !!sym(input$plotVariable2)) %>% summary %>% xtable
  })
  output$corrText <- renderText({
    req(input$numVars, input$plotVariable2)
    if(is.numeric((heartDataPlot() %>% select(input$plotVariable2))[[1]]) & is.numeric((heartDataPlot() %>% select(input$plotVariable1))[[1]])){
      paste0("Correlation between ", input$plotVariable1, " and ", input$plotVariable2, " = ", round(cor(heartDataPlot() %>% select(!!sym(input$plotVariable1)), heartDataPlot() %>% select(!!sym(input$plotVariable2))), 3))
    }
  })
  # create plots
  output$factorFactorPlot <- renderPlotly({
    req(input$numVars, input$plotVariable2)
    p <- ggplot(data = count(heartDataPlot(), !!sym(input$plotVariable2), !!sym(input$plotVariable1))) + geom_mosaic(aes(weight = n, x = product(!!sym(input$plotVariable2)), fill = !!sym(input$plotVariable1))) + labs(x = input$plotVariable2, y = input$plotVariable1)
    ggplotly(p)
  })
  output$factorContPlot <- renderPlotly({
    req(input$numVars, input$plotVariable2)
    lev <- levels((heartDataPlot() %>% select(input$plotVariable1))[[1]])
    figHist <- plot_ly(alpha = 0.6)
    for(i in lev){
      figHist <- figHist %>% add_histogram(x = (heartDataPlot() %>% filter(!!sym(input$plotVariable1) == i) %>% select(input$plotVariable2))[[1]], name = i)
    }
    figHist <- figHist %>% layout(barmode = "overlay",
                                  xaxis = list(title = input$plotVariable2),
                                  legend = list(title = list(text = input$plotVariable1)))
    figHist
  })
  output$contFactorPlot <- renderPlotly({
    req(input$numVars, input$plotVariable2)
    lev <- levels((heartDataPlot() %>% select(input$plotVariable2))[[1]])
    figHist <- plot_ly(alpha = 0.6)
    for(i in lev){
      figHist <- figHist %>% add_histogram(x = (heartDataPlot() %>% filter(!!sym(input$plotVariable2) == i) %>% select(input$plotVariable1))[[1]], name = i)
    }
    figHist <- figHist %>% layout(barmode = "overlay",
                                  xaxis = list(title = input$plotVariable1),
                                  legend = list(title = list(text = input$plotVariable2)))
    figHist
  })
  output$contContPlot <- renderPlotly({
    req(input$numVars, input$plotVariable2)
    figScatter <- heartDataPlot() %>% plot_ly(x = as.formula(paste0("~", input$plotVariable1)),
                  y = as.formula(paste0("~", input$plotVariable2)),
                 type = "scatter",
                 mode = "markers")
    figScatter
  })
  
  ## Modeling page
  # store user inputs when action button pressed
  data <- eventReactive(input$fit, {
    list(prop = input$trainingProp, vars1 = input$modelVariables1, vars2 = input$modelVariables2, vars3 = input$modelVariables3, folds = input$numFolds
    )
  })
  # fit models
  fitModels <- reactive({
    #Create a progress object and make sure it closes when we exit the reactive
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Fitting model", value = 0)
    info <- data()
    # partition data set
    trainIndex <- createDataPartition(heartData$disease, p = info$prop[[1]], list = FALSE)
    heartTrain <- heartData[trainIndex, ]
    heartTest <- heartData[-trainIndex, ]
    # fit GLM model
    predictors1 <- paste(info$vars1, collapse = "+")
    formula1 <- as.formula(paste0("disease", "~", predictors1))
    progress$inc(1/4, detail = paste("GLM"))
    model1Fit <- train(formula1, data = heartTrain,
                      method = "glm",
                      family = "binomial",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = info$folds)
    )
    # fit tree model
    predictors2 <- paste(info$vars2, collapse = "+")
    formula2 <- as.formula(paste0("disease", "~", predictors2))
    progress$inc(2/4, detail = paste("Tree"))
    model2Fit <- train(formula2, data = heartTrain,
                       method = "rpart",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "repeatedcv", number = info$folds, repeats = 3)
    )
    # fit random forest model
    predictors3 <- paste(info$vars3, collapse = "+")
    formula3 <- as.formula(paste0("disease", "~", predictors3))
    progress$inc(3/4, detail = paste("Random Forest"))
    model3Fit <- train(formula3, data = heartTrain,
                       method = "rf",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "repeatedcv", number = info$folds, repeats = 3)
    )
    progress$inc(4/4, detail = paste("Done"))
    list(model1Fit, model2Fit, model3Fit, heartTest)
  })
  # create summaries of model fits
  output$model1Text <- renderPrint({
    model1Fit <- fitModels()[[1]]
    model1Fit
  })
  output$model1Summary <- renderPrint({
    model1Fit <- fitModels()[[1]]
    summary(model1Fit)
  })
  output$model2Text <- renderPrint({
    model2Fit <- fitModels()[[2]]
    model2Fit
  })
  output$model3Text <- renderPrint({
    model3Fit <- fitModels()[[3]]
    model3Fit
  })
  output$comparison <- renderTable({
    model1Fit <- fitModels()[[1]]
    model2Fit <- fitModels()[[2]]
    model3Fit <- fitModels()[[3]]
    results <- c("GLM" = model1Fit[["results"]][["Accuracy"]],
                 "Tree" = model2Fit[["results"]]["Accuracy"][which.max(model2Fit[["results"]][["Accuracy"]]), ],
                 "Random Forest" = model3Fit[["results"]]["Accuracy"][which.max(model3Fit[["results"]][["Accuracy"]]), ]
    )
    data.frame("Accuracy" = results)
  },
  rownames = TRUE
  )
  # get performance on test set
  performance <- reactive({
    model1Fit <- fitModels()[[1]]
    model2Fit <- fitModels()[[2]]
    model3Fit <- fitModels()[[3]]
    heartTest <- fitModels()[[4]]
    pred1 <- predict(model1Fit, heartTest)
    conf1 <- confusionMatrix(pred1, heartTest$disease)
    pred2 <- predict(model2Fit, heartTest)
    conf2 <- confusionMatrix(pred2, heartTest$disease)
    pred3 <- predict(model3Fit, heartTest)
    conf3 <- confusionMatrix(pred3, heartTest$disease)
    list(conf1, conf2, conf3)
  })
  # create summaries of model performance on test set
  output$performance1 <- renderPrint({
    performance()[[1]]
  })
  output$performance2 <- renderPrint({
    performance()[[2]]
  })
  output$performance3 <- renderPrint({
    performance()[[3]]
  })
  output$performanceComp <- renderTable({
    results <- c("GLM" = 1 - performance()[[1]][["overall"]][["Accuracy"]],
                 "Tree" = 1 - performance()[[2]][["overall"]][["Accuracy"]],
                 "Random Forest" = 1 - performance()[[3]][["overall"]][["Accuracy"]]
    )
    data.frame("Mis-classification rate" = results)
  },
  rownames = TRUE
  )
  # get predictions on user input data
  output$predictions <- renderTable({
    model1Fit <- fitModels()[[1]]
    model2Fit <- fitModels()[[2]]
    model3Fit <- fitModels()[[3]]
    newdf <- data.frame(age = input$age, sex = input$sex, cp = input$cp, trestbps = input$trestbps, chol = input$chol, fbs = input$fbs, restecg = input$restecg, thalach = input$thalach, exang = input$exang, oldpeak = input$oldpeak, slope = input$slope, ca = input$ca, thal = input$thal)
    results <- c("GLM" = predict(model1Fit, newdf),
                 "Tree" = predict(model2Fit, newdf),
                 "Random Forest" = predict(model3Fit, newdf))
    data.frame("Prediction" = results)
  },
  rownames = TRUE
  )
}

# Other things I tried but didn't include in final version (inc ROC, AUC, etc.), may add in future

# binMod <- glm(num ~ . - disease, family = binomial, heartData)
# summary(binMod)
# df <- data.frame(binMod$fitted.values, heartData$num)
# tabBin <- xtabs(~ (binMod$fitted.values > .4) + (heartData$num != 0))
# tabBin
# (tabBin[1,1] + tabBin[2,2])/sum(tabBin)
# library(ROCR)
# predBin <- prediction(binMod$fitted.values, (heartData$num != 0))
# perfBin <- performance(predBin, "acc")
# plot(perfBin)
# optimize(perfBin, interval = c(.2, .8), maximum = TRUE)
# max(perfBin)
# perfBin
# slotNames(perfBin)
# perfBin @ x.name
# 
# aucBin <- performance(predBin, "auc")
# aucBin @ y.values[[1]]
# perfBin @ y.values[[1]]


# binMod2 <- glm(disease ~ . - num, family = binomial, heartData)
# summary(binMod2)
# df <- data.frame(binMod2$fitted.values, heartData$num)
# tabBin <- xtabs(~ (binMod2$fitted.values > .4) + (heartData$num != 0))
# tabBin
# (tabBin[1,1] + tabBin[2,2])/sum(tabBin)
# library(ROCR)
# predBin <- prediction(binMod2$fitted.values, (heartData$num != 0))
# perfBin <- performance(predBin, "acc")
# plot(perfBin)
# optimize(perfBin, interval = c(.2, .8), maximum = TRUE)
# max(perfBin)
# perfBin
# slotNames(perfBin)
# perfBin @ x.name
# 
# aucBin <- performance(predBin, "auc")
# aucBin @ y.values[[1]]
# perfBin @ y.values[[1]]
# 
# orderedLogMod <- polr(num ~ ., heartData)
# orderedLogMod
# 
# pred <- predict(orderedLogMod, heartData, "class")
# df2 <- data.frame(pred, heartData$num)
# df2
# tab <- xtabs(~ (pred == 0) + (heartData$num == 0))
# (tab[1,1] + tab[2,2])/sum(tab)