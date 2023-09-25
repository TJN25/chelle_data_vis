# packages
library("shiny")
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(png)
library(shinyjs)
library(shinyalert)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library("data.table")
library(reshape2)
library(RColorBrewer)


# modules
source("background-colours.R")

# functions

source("utils.R")
source("utils/dataInputClass.R")
source("utils/plotClass.R")

# global variables
options(dplyr.summarise.inform = FALSE)

load("tmp/mainData.Rda")
load("data/zeroBlasts.Rda")
load("tmp/metaData.Rda")
