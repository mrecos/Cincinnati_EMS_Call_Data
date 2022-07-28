## Step 1
library(shiny)

library(shinydashboard)
library(leaflet)
library(shinyBS)
library(sf)
library(tidyverse)
library(DataRobotColors)

## Step 2
ui = dashboardPage(
  dashboardHeader(title = ""),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(menuItem(
                     "",
                     tabName = "",
                     icon = icon("table")
                   ))
  ),
  dashboardBody(
    tags$head(),
    fluidRow(column(width = 12))
  )
)

# step 3
server = function(input, output, session) { }

shinyApp(ui, server)

# step 4
dat  <- read_csv("./DATA/Cinc_OD_pnts_ACS_KNN_PREDICT.csv") 
net  <- read_sf("./DATA/cin_final_net.geojson")
fd   <- st_read("https://opendata.arcgis.com/datasets/a6f043d181f94e37a274975a3718b7af_16.geojson") %>%
  filter(BND_NAME == "CINCINNATI") %>%
  mutate(buffer = 1000)
preds <- read_csv("./DATA/result-623bb75a4c8eb0a58a23a47f.csv")
preds_net <- net %>% 
  mutate(id = seq(1,n())) %>% 
  left_join(preds %>% 
              select(id, countopioid_PREDICTION, starts_with("EXPLANATION")),
            by = "id")

# step 5
pred <- reactiveValues(pred_price = NULL,
                       pred_exp   = NULL)

map = createLeafletMap(session, 'map')

# step 6
DR_colors <- as.character(DataRobotColors::DataRobot_get_palettes()$DR_Diverging)
pal_DR <- colorFactor(
  palette = DR_colors,
  domain = preds_net$countopioid_PREDICTION
)

# step 7
session$onFlushed(once = TRUE, function() {
  
  text <- apply(preds_net, 1, function(.x){
    paste("<b>Predicted Count:</b><h2>",
          ceiling(.x$countopioid_PREDICTION),"</h2>",
          .x$EXPLANATION_1_FEATURE_NAME,": <b>", 
          get_color(.x$EXPLANATION_1_QUALITATIVE_STRENGTH), 
          .x$EXPLANATION_1_QUALITATIVE_STRENGTH,"</font></b><br>",
          .x$EXPLANATION_2_FEATURE_NAME,": <b>", 
          get_color(.x$EXPLANATION_2_QUALITATIVE_STRENGTH),
          .x$EXPLANATION_2_QUALITATIVE_STRENGTH,"</font></b><br>",
          .x$EXPLANATION_3_FEATURE_NAME,": <b>", 
          get_color(.x$EXPLANATION_3_QUALITATIVE_STRENGTH),
          .x$EXPLANATION_3_QUALITATIVE_STRENGTH,"</font></b><br>")
  })
  
# step 8
  output$map <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = preds_net, 
                  fillColor = ~pal_DR(countopioid_PREDICTION),
                  color = NA,
                  label = lapply(text, htmltools::HTML),
                  fillOpacity = 0.70, group = "Prediction") %>%
      addCircles(data = fd, weight = 2, 
                 fillColor = NA, color = "green",
                 fillOpacity = 0, radius = ~buffer,
                 popup = ~COMPANY,
                 group = "Fire Station") %>% 
      setView(lat = 39.13923,lng = -84.52602,zoom = 12) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      addLayersControl(
        overlayGroups = c("Prediction", "Fire Station"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright") %>% 
      hideGroup("Fire Station")
  })
})





