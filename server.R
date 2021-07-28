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

filter

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
  select(disease, everything())
  na.omit()

fig3d <- plot_ly(x = heartData$trestbps, y = heartData$age, z = heartData$num)
fig3d

figBox <- plot_ly(heartData, y = ~trestbps, color = ~disease, type = "box")
figBox

figBar <- heartData %>% count(cp, disease) %>%
  plot_ly(x = ~cp, y = ~n, color = ~disease) %>%
  add_bars()
figBar

ggplot(heartData, aes(x = slope, fill = num)) + geom_bar()
# 
# ggpairs(heartData)
# 
# str(heartData)
# 
binMod <- glm(num ~ . - disease, family = binomial, heartData)
summary(binMod)
df <- data.frame(binMod$fitted.values, heartData$num)
tabBin <- xtabs(~ (binMod$fitted.values > .4) + (heartData$num != 0))
tabBin
(tabBin[1,1] + tabBin[2,2])/sum(tabBin)
library(ROCR)
predBin <- prediction(binMod$fitted.values, (heartData$num != 0))
perfBin <- performance(predBin, "acc")
plot(perfBin)
optimize(perfBin, interval = c(.2, .8), maximum = TRUE)
max(perfBin)
perfBin
slotNames(perfBin)
perfBin @ x.name

aucBin <- performance(predBin, "auc")
aucBin @ y.values[[1]]
perfBin @ y.values[[1]]


binMod2 <- glm(disease ~ . - num, family = binomial, heartData)
summary(binMod2)
df <- data.frame(binMod2$fitted.values, heartData$num)
tabBin <- xtabs(~ (binMod2$fitted.values > .4) + (heartData$num != 0))
tabBin
(tabBin[1,1] + tabBin[2,2])/sum(tabBin)
library(ROCR)
predBin <- prediction(binMod2$fitted.values, (heartData$num != 0))
perfBin <- performance(predBin, "acc")
plot(perfBin)
optimize(perfBin, interval = c(.2, .8), maximum = TRUE)
max(perfBin)
perfBin
slotNames(perfBin)
perfBin @ x.name

aucBin <- performance(predBin, "auc")
aucBin @ y.values[[1]]
perfBin @ y.values[[1]]
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
  filterTableData <- reactive({
    filteredTableData <- heartData %>% select(input$tableVariables)
  })
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
# 
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
}
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
