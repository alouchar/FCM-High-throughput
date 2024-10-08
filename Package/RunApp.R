rm(list = ls(all=TRUE))
#_____________________________________________________________ Required packages
packages = c("shinydashboard","shiny","markdown","DT","CytoDx","shinyFiles","flowCore","shinycssloaders","dplyr","ggplot2","ggExtra",
             "V8","cluster","gdata","shinyalert","shinyjs","shinyWidgets","plotly")

##To run once, not everytime
for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
  }
  require(p, character.only = T)
  BiocManager::install("CytoDx",force = T)
}

for(p in packages){
  require(p,character.only = T)
}

rm(p,packages)

options(scipen = 999)
options(shiny.maxRequestSize=30*1024^2)
shinyOptions(warn.stacktrace = FALSE)

setwd("C:/Users/ArnaudL/Desktop/High throughput bioinfos/PhytoCytoTraits")
#_____________________________________________________________ Source R text

#___________________________________ Source R functions
source("./functions/functions.R")
source("./functions/var_coeff.R")
source("./functions/kmeans.R")
source("./functions/PartAroundMed.R")
source("./functions/DBSCAN.R")

#___________________________________ Shiny Server and UI
source("./package/server.R")
source("./package/ui.R")

#___________________________________ RunApp
shinyApp(ui, server)
