# packages
library(googledrive)
library("shiny")
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(png)
library(shinyjs)
library(ggplot2)
library(shinyalert)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(gganimate)
library(rdrop2)
# modules
source("background-colours.R")

# functions

source("utils.R")
source("utils/dataInputClass.R")
source("plots.R")

# global variables

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)
options(dplyr.summarise.inform = FALSE)

drive_download(as_id("1LyYpiXYSkXKnEyGCRjbJy0_x2iNKLchY"), path = "tmp/mainData.Rda", overwrite = T)
drive_download(as_id("16DEmrdQyj0hjUU83Iof4B1A79CAXx-YT"), path = "tmp/metaData.Rda", overwrite = T)

load("tmp/mainData.Rda")
load("data/zeroBlasts.Rda")
load("tmp/metaData.Rda")











