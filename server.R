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
# 
# # Define server logic required to draw a histogram



function(input, output, session) {
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
  # var1 <- reactive({
  #   var1 <- (heartData %>% select(input$plotVariable1))[[1]]
  # })
  # var2 <- reactive({
  #   var2 <- (heartData %>% select(input$plotVariable1))[[2]]
  # })
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
  output$plotVariable1choices <- renderUI({
    # vars <- names(heartData %>% select(-input$plotVariable1))
    selectInput("plotVariable1", "First variable to summarize",
                choices = names(heartData),
                selected = "disease"
    )
  })
  output$plotVariable2choices <- renderUI({
    vars <- names(heartData %>% select(-input$plotVariable1))
    selectInput("plotVariable2", "Second variable to summarize (optional)",
                choices = c("none", names(heartData %>% select(-input$plotVariable1))),
                selected = "none"
    )
  })

  output$plotSingleVar <- renderUI({
    if(input$plotVariable2 == 'none'){
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
    if(input$plotVariable2 == 'none' & 
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
    if(input$plotVariable2 == 'none'){
      tableOutput("singleVarSummary")
    }
  })
  
  output$chart2vars <- renderUI({
    if(input$plotVariable2 != "none"){
      if(is.factor((heartData %>% select(input$plotVariable1))[[1]])){
        if(is.factor((heartData %>% select(input$plotVariable2))[[1]])){
          tableOutput("factorFactorSummary")
        } else {
          if(is.numeric((heartData %>% select(input$plotVariable2))[[1]])){
            tableOutput("factorContSummary")
          }
        }
      } else {
        if(is.numeric((heartData %>% select(input$plotVariable1))[[1]])){
          if(is.factor((heartData %>% select(input$plotVariable2))[[1]])){
            tableOutput("contFactorSummary")
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
    if(is.numeric((heartData %>% select(input$plotVariable1))[[1]])){
      if(is.numeric((heartData %>% select(input$plotVariable2))[[1]])){
        textOutput("corrText")
      }
    }
  })
  
  output$plot2vars <- renderUI({
    if(input$plotVariable2 != "none"){
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
  
  output$varChoiceModel1 <- renderUI({
    checkboxGroupInput("modelVariables1", "Variables to include in GLM",
                       choices = names(heartData %>% select(-c(disease, num))),
                       inline = FALSE,
                       selected = names(heartData %>% select(-c(disease, num)))
    )
  })
  output$intChoiceModel1 <- renderUI({
    checkboxInput("interaction", "Include interaction terms?"
    )
  })
  output$autoChoiceModel1 <- renderUI({
    checkboxInput("selection", "Perform automatic variable selection based on AIC (among variables selected above)?"
    )
  })
  
  output$varChoiceModel2 <- renderUI({
    checkboxGroupInput("modelVariables2", "Variables to include in Classification Tree Model",
                       choices = names(heartData %>% select(-c(disease, num))),
                       inline = FALSE,
                       selected = names(heartData %>% select(-c(disease, num)))
    )
  })
  
  output$varChoiceModel3 <- renderUI({
    checkboxGroupInput("modelVariables3", "Variables to include in Random Forest Model",
                       choices = names(heartData %>% select(-c(disease, num))),
                       inline = FALSE,
                       selected = names(heartData %>% select(-c(disease, num)))
    )
  })
  
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
  output$contInput <- renderUI({
    tagList(
        numericInput("age", "age", value = mean(heartData$age, min = 0, max = 150)),
        numericInput("trestbps", "trestbps", value = mean(heartData$trestbps), min = 0, max = 2 * max(heartData$trestbps)),
        numericInput("chol", "chol", value = mean(heartData$chol), min = 0, max = 2 * max(heartData$chol)),
        numericInput("thalach", "thalach", value = mean(heartData$thalach), min = 0, max = 2 * max(heartData$thalach)),
        numericInput("oldpeak", "oldpeak", value = mean(heartData$oldpeak), min = 0, max = 2 * max(heartData$oldpeak))
    )
  })

  
  output$factorFactorSummary <- renderTable({
    # xtable(xtabs(as.formula(paste0("~", input$plotVariable1, "+", input$plotVariable2)), data = heartData))
    xtable(table((heartData %>% select(input$plotVariable1))[[1]], (heartData %>% select(input$plotVariable2))[[1]]))
  },
    rownames = TRUE
    # count(heartData, !!sym(input$plotVariable1), !!sym(input$plotVariable2)) %>% xtable
  )
  output$factorContSummary <- renderTable({
    heartData %>% select(input$plotVariable1, !!sym(input$plotVariable2)) %>%
      group_by(!!sym(input$plotVariable1)) %>%
      summarize("Min" = min(!!sym(input$plotVariable2)), "1st Qu." = quantile(!!sym(input$plotVariable2))["25%"],  "Median" = median(!!sym(input$plotVariable2)), "Mean" = mean(!!sym(input$plotVariable2)), "3rd Qu." = quantile(!!sym(input$plotVariable2))["75%"], "Max" = max(!!sym(input$plotVariable2))) %>%
      xtable 
  })
  output$contFactorSummary <- renderTable({
    heartData %>% select(!!sym(input$plotVariable2), !!sym(input$plotVariable1)) %>%
      group_by(!!sym(input$plotVariable2)) %>%
      summarize("Min" = min(!!sym(input$plotVariable1)), "1st Qu." = quantile(!!sym(input$plotVariable1))["25%"],  "Median" = median(!!sym(input$plotVariable1)), "Mean" = mean(!!sym(input$plotVariable1)), "3rd Qu." = quantile(!!sym(input$plotVariable1))["75%"], "Max" = max(!!sym(input$plotVariable1))) %>%
      xtable 
  })
  output$contContSummary <- renderTable({
    heartData %>% select(!!sym(input$plotVariable1), !!sym(input$plotVariable2)) %>% summary %>% xtable
    # heartData2 %>% select(!!sym(input$plotVariable1), !!sym(input$plotVariable2)) %>% cor
  })
  output$corrText <- renderText({
    paste0("Correlation between ", input$plotVariable1, " and ", input$plotVariable2, " = ", round(cor(heartData %>% select(!!sym(input$plotVariable1)), heartData %>% select(!!sym(input$plotVariable2))), 3))
  })
  
  output$factorFactorPlot <- renderPlotly({
    p <- ggplot(data = count(heartData, !!sym(input$plotVariable2), !!sym(input$plotVariable1))) + geom_mosaic(aes(weight = n, x = product(!!sym(input$plotVariable2)), fill = !!sym(input$plotVariable1))) + labs(x = input$plotVariable2, y = input$plotVariable1)
    ggplotly(p)
  })
  output$factorContPlot <- renderPlotly({
    lev <- levels((heartData %>% select(input$plotVariable1))[[1]])
    figHist <- plot_ly(alpha = 0.6)
    for(i in lev){
      figHist <- figHist %>% add_histogram(x = (heartData %>% filter(!!sym(input$plotVariable1) == i) %>% select(input$plotVariable2))[[1]], name = i)
    }
    figHist <- figHist %>% layout(barmode = "overlay",
                                  xaxis = list(title = input$plotVariable2),
                                  legend = list(title = list(text = input$plotVariable1)))
    figHist
  })
  output$contFactorPlot <- renderPlotly({
    lev <- levels((heartData %>% select(input$plotVariable2))[[1]])
    figHist <- plot_ly(alpha = 0.6)
    for(i in lev){
      figHist <- figHist %>% add_histogram(x = (heartData %>% filter(!!sym(input$plotVariable2) == i) %>% select(input$plotVariable1))[[1]], name = i)
    }
    figHist <- figHist %>% layout(barmode = "overlay",
                                  xaxis = list(title = input$plotVariable1),
                                  legend = list(title = list(text = input$plotVariable2)))
    figHist
  })
  output$contContPlot <- renderPlotly({
    figScatter <- heartData %>% plot_ly(x = as.formula(paste0("~", input$plotVariable1)),
                  y = as.formula(paste0("~", input$plotVariable2)),
                 type = "scatter",
                 mode = "markers")
    figScatter
  })
  
  
  output$singleVarSummary <- renderTable({
    if(is.factor((heartData %>% select(input$plotVariable1))[[1]])){
      count(heartData, !!sym(input$plotVariable1))
    } else {
      if(is.numeric((heartData %>% select(input$plotVariable1))[[1]])){
        xtable(summary(heartData %>% select(input$plotVariable1)))
      }
    }
  })
  output$singleVarBar <- renderPlotly({
    figBar <- heartData %>% count(!!sym(input$plotVariable1)) %>%
      plot_ly(x = as.formula(paste0("~", input$plotVariable1)), y = ~n, type = "bar") 
    figBar
  })
  output$singleVarBox <- renderPlotly({
    figBox <- heartData %>%
      plot_ly(y = as.formula(paste0("~", input$plotVariable1)), type = "box")
    figBox
  })
  output$singleVarHist <- renderPlotly({
    figHist <- heartData %>% 
      plot_ly(x = as.formula(paste0("~", input$plotVariable1)), type = "histogram")
    figHist
  })
  
  data <- eventReactive(input$fit, {
    list(prop = input$trainingProp, vars1 = input$modelVariables1, vars2 = input$modelVariables2, vars3 = input$modelVariables3, int = input$interaction, auto = input$selection)
  })
  
  fitModels <- reactive({
    #Create a progress object and make sure it closes when we exit the reactive
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Fitting model", value = 0)
    info <- data()
    trainIndex <- createDataPartition(heartData$disease, p = info$prop[[1]], list = FALSE)
    heartTrain <- heartData[trainIndex, ]
    heartTest <- heartData[-trainIndex, ]
    predictors1 <- paste(info$vars1, collapse = "+")
    formula1 <- as.formula(paste0("disease", "~", predictors1))
    progress$inc(1/4, detail = paste("GLM"))
    model1Fit <- train(formula1, data = heartTrain,
                      method = "glm",
                      family = "binomial",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = 10)
    )
    predictors2 <- paste(info$vars2, collapse = "+")
    formula2 <- as.formula(paste0("disease", "~", predictors2))
    progress$inc(2/4, detail = paste("Tree"))
    model2Fit <- train(formula2, data = heartTrain,
                       method = "rpart",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
    )
    predictors3 <- paste(info$vars3, collapse = "+")
    formula3 <- as.formula(paste0("disease", "~", predictors3))
    progress$inc(3/4, detail = paste("Random Forest"))
    model3Fit <- train(formula3, data = heartTrain,
                       method = "rf",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
    )
    progress$inc(4/4, detail = paste("Done"))
    list(model1Fit, model2Fit, model3Fit, heartTest)
  })
  
  
  
  output$model1Text <- renderPrint({
    model1Fit <- fitModels()[[1]]
    model1Fit
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

# paste("Tree model accuracy = ", fitModels[[2]][["results"]]["Accuracy"])

# listTest <- list(logFit10, logFit4)
# str(listTest[[2]])
# str(logFit4)

# logFit10[["results"]]["Accuracy"][[1]]
# str(logFit10$results)
# lapply(list(logFit10, logFit4), [["results"]])
  # test <- paste(c("hi", "bye"), collapse = "+")
  # as.formula(paste0("y", "~", test))

# 
# options(contrasts = c("contr.treatment", "contr.poly"))
# house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
# house.plr
# summary(house.plr, digits = 3)
# 
# ## slightly worse fit from
# summary(update(house.plr, method = "probit", Hess = TRUE), digits = 3)
# ## although it is not really appropriate, can fit
# summary(update(house.plr, method = "loglog", Hess = TRUE), digits = 3)
# summary(update(house.plr, method = "cloglog", Hess = TRUE), digits = 3)
# 
# predict(house.plr, housing, type = "p")
# addterm(house.plr, ~.^2, test = "Chisq")
# house.plr2 <- stepAIC(house.plr, ~.^2)
# house.plr2$anova
# anova(house.plr, house.plr2)
# 
# house.plr <- update(house.plr, Hess=TRUE)
# pr <- profile(house.plr)
# confint(pr)
# plot(pr)
# pairs(pr)
