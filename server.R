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
  select(disease, everything()) %>%
  na.omit()

# data.frame(summary(heartData2 %>% select(age)))
# xtable::xtable(summary(heartData2 %>% select(age)))
# class(data.frame(count(heartData2, disease)))
# ?stripchart
# stripchart(heartData[,2], method = "jitter")
# heartData %>%  ggplot() + geom_histogram(aes(x = disease, fill = disease, alpha = 0.5))

# fig3d <- plot_ly(x = heartData$trestbps, y = heartData$age, z = heartData$num)
# fig3d
# 
# figBox <- plot_ly(heartData, y = ~trestbps, color = ~disease, type = "box")
# figBox
# 
# x1 <- heartData2 %>% filter(cp == "1") %>%
#   select(age)
# x2 <- heartData2 %>% filter(cp == "2") %>%
#   select(age)
# 
# lev <- levels(heartData2$cp)
# figHist <- plot_ly(alpha = 0.6)
# for(i in lev){
#   figHist <- figHist %>% add_histogram(x = (heartData2 %>% filter(cp == i) %>%
#     select(age))[[1]])
# }
# figHist <- figHist %>% layout(barmode = "dodge")
# figHist
# 
# add_histogram(x = x1[[1]]) %>%
#   add_histogram(x = x2[[1]]) %>%
#   layout(barmode = "overlay")
# figHist
# summary(x1)
# 
# figBar <- heartData %>% count(cp, disease) %>%
#   plot_ly(x = ~cp, y = ~n, color = ~disease) %>%
#   add_bars()
# figBar


# figBar <- heartData %>% count(!!sym("cp")) %>%
#   plot_ly(x = ~aes_string("cp"), y = ~n, type = "bar") %>%
#   layout(barmode = "stack")
# figBar


# 
# tab <- xtabs(~ cp + disease, data = heartData)
# tab
# dotchart(tab)
# mosaicplot(tab, shade = TRUE)
# 
# 

# 
# heartData %>% select(where(is.factor)) %>% names
# heartData %>% select(where(is.numeric)) %>% names
# 
# checkboxGroupInput()

# ggplot(heartData, aes(x = slope, fill = num)) + geom_bar()
# # 
# # ggpairs(heartData)
# # 
# # str(heartData)
# # 
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
  # output$barSingle <- renderPlotly({
  #   figBar <- heartData %>% count(!!sym(input$plotVariable1)) %>%
  #     plot_ly(x = ~aes_string(input$plotVariable1), y = ~n) %>%
  #     add_bars()
  #   figBar
  # })
  # output$test <- renderText({
  #   paste0("plotVariable1", input$plotVariable1, input$plotVariable2, heartData %>% count(!!sym(input$plotVariable1)), heartData %>% select(input$plotVariable1) %>% mean)
  # })
  
  
  # output$singleVar <- renderUI({
  #   if(input$plotVariable2 == "none"){
  #     selectInput("singleVarChoice", "Which type of plot?",
  #                 choices = c("Histogram", "Boxplot"),
  #                 selected = "Histogram"
  #     )
  #     }
  # })

  
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
}

  
  
#     output$distPlot <- renderPlot({
# 
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
# 
#     })
# 

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
