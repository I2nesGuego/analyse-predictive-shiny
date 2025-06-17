# global.R

# Librairies
library(shiny)
library(shinydashboard)
library(FactoMineR)
library(e1071)
library(randomForest)
library(neuralnet)
library(MASS)
library(caTools)
library(ggplot2)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(shinyWidgets)
library(plotly)
library(shinythemes)
library(promises)
library(future)
library(DT)
library(openxlsx)
library(tidyr)

# %||% opérateur : retourne x si non NULL, sinon y
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

# Paramètres globaux
plan(multisession)
options(shiny.sanitize.errors = TRUE)
options(shiny.maxRequestSize = 350 * 1024^2)

#Chargement des fichiers utilitaires dans helpers/
source("helpers/data_utils.R")
source("helpers/model_utils.R")
source("helpers/plotting.R")


