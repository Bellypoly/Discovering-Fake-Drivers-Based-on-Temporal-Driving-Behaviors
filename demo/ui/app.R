getwd()
setwd("/Users/suwaphit/Desktop/R/Discovering-Fake-Drivers-Based-on-Temporal-Driving-Behaviors/demo/ui")
org_df <- read.csv(file = "../../dataset/Driving_Data_KIA_SOUL.csv", header = T, stringsAsFactors = T)

library(shiny)
library(argonR)
library(argonDash)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)

# template
source("sidebar.R")
source("header.R")
source("footer.R")

# elements
source("cards/cards_tab.R")
source("tabs/tabsets_tab.R")
source("alerts/alerts_tab.R")
source("medias/medias_tab.R")
source("items/items_tab.R")
source("css_effects/effects_tab.R")
source("sections/sections_tab.R")
source("tables/tables_tab.R")

source("../server.R")

# App
shiny::shinyApp(
  ui = argonDashPage(
    tags$head(
      tags$style(HTML("
          .navbar-brand-img{
            max-height:9.5rem !important;
          }
          .shiny-options-group{
            font-size:14px;
          }
      "))
    ),
    title = "RDFD: Discovering Fake Drivers Based on Temporal Driving Behaviors",
    author = "Suwaphit Buabuthr",
    description = "Discovering Fake Drivers Based on Temporal Driving Behaviors",
    sidebar = argonSidebar,
    header = argonHeader,
    body = argonDashBody(
      argonTabItems(
        data_tab,
        cards_tab
        # tabsets_tab,
        # alerts_tab,
        # images_tab,
        # items_tab,
        # effects_tab,
        # sections_tab
      )
    ),
    footer = argonFooter
  ),
  server = s
)
