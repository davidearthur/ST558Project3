# ST558Project3
## Shiny App
### Description and Purpose of App
This `shiny` app allows the user to explore and analyze a data set from 1988 related to diagnosis of heart disease at a location of the Cleveland Clinic.  There are pages for exploring the data, fitting models to the data, and using the models to make predictions on new data.

### Packages Required
The following packages are required to run this app: 

*  shinydashboard
*  DT
*  MASS
*  tidyverse
*  GGally
*  plotly
*  ggmosaic
*  knitr
*  xtable
*  caret

### Code to install all necessary packages:
`install.packages("shinydashboard", "DT", "MASS", "tidyverse", "GGally", "plotly", "ggmosaic", "knitr", "xtable", "caret")`

### Code to run app from gitHub:
`shiny::runGitHub("ST558Project3", "davidearthur", ref = "main")`