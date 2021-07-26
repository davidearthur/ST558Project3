#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(MASS)
library(tidyverse)
select <- dplyr::select

heartData <- read_csv("processed.cleveland.data",
                      col_names = c("age", "sex", "cp", "trestbps", "chol", "fbs",
                                    "restecg", "thalach", "exang", "oldpeak",
                                    "slope", "ca", "thal", "num"
                                    ),
                      na = "?"
)
    
heartData <- heartData %>% 
    mutate(across(c(sex, cp, fbs, restecg, exang, thal), as_factor)) %>%
    mutate(across(c(slope, ca, num), ordered)) %>%
    na.omit()


str(heartData)

binMod <- glm(num ~ ., family = binomial, heartData)
summary(binMod)
df <- data.frame(binMod$fitted.values, heartData$num)


orderedLogMod <- polr(num ~ ., heartData)
orderedLogMod

pred <- predict(orderedLogMod, heartData, "class")
df2 <- data.frame(pred, heartData$num)
df2
tab <- xtabs(~ (pred == 0) + (heartData$num == 0))
(tab[1,1] + tab[2,2])/sum(tab)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})

options(contrasts = c("contr.treatment", "contr.poly"))
house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
house.plr
summary(house.plr, digits = 3)

## slightly worse fit from
summary(update(house.plr, method = "probit", Hess = TRUE), digits = 3)
## although it is not really appropriate, can fit
summary(update(house.plr, method = "loglog", Hess = TRUE), digits = 3)
summary(update(house.plr, method = "cloglog", Hess = TRUE), digits = 3)

predict(house.plr, housing, type = "p")
addterm(house.plr, ~.^2, test = "Chisq")
house.plr2 <- stepAIC(house.plr, ~.^2)
house.plr2$anova
anova(house.plr, house.plr2)

house.plr <- update(house.plr, Hess=TRUE)
pr <- profile(house.plr)
confint(pr)
plot(pr)
pairs(pr)
