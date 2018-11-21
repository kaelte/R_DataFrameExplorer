rm(list=ls())
### The Data frame explorer needs a data frame called inputDf.
### The file sampleData.R provides such a data frame
source(local=TRUE,file="sampleData.R")
source(local=TRUE,file="DataFrameExplorer.R")
ui <- uiFunction
server <- serverFunction
shinyApp(ui,server)
