library(shiny)
library(shinydashboard)
library(leaflet)
library(httr)
library(jsonlite)
library(shinyBS)
library(scales)
library(sf)
library(tidyverse)
library(DataRobotColors)

dat <- read_csv("./DATA/Cinc_OD_pnts_ACS_KNN_PREDICT.csv") 
net  <- read_sf("./DATA/cin_final_net.geojson")
# cinBoundary <- 
#   st_read("https://opendata.arcgis.com/datasets/ed78f4754b044ac5815d0a9efe9bb336_1.geojson") %>%
#   st_transform('ESRI:102258') 
fd <- st_read("https://opendata.arcgis.com/datasets/a6f043d181f94e37a274975a3718b7af_16.geojson") %>%
  filter(BND_NAME == "CINCINNATI") %>%
  mutate(buffer = 1000)
  # st_transform('ESRI:102258') %>% 
  # st_buffer(1000) %>% 
  # st_transform("epsg:4326")
preds <- read_csv("./DATA/result-623bb75a4c8eb0a58a23a47f.csv")
preds_net <- net %>% 
  mutate(id = seq(1,n())) %>% 
  left_join(preds %>% 
              select(id, countopioid_PREDICTION, starts_with("EXPLANATION")),
            by = "id")

get_color <- function(sign){
  if(sign == "+++"){
    return("<font color = '#ff803f'>")
  } else if(sign == "++"){
    return("<font color = '#ffb38c'>")
  } else if(sign == "+"){
    return("<font color = '#ffe5d8'>")
  } else if(sign == "---"){
    return("<font color = '#61abe9'>")
  } else if(sign == "--"){
    return("<font color = '#a0ccf1'>")
  } else if(sign == "-"){
    return("<font color = '#dfeefa'>")
  }
}
####

ui = dashboardPage(
  dashboardHeader(title = "Cincinnati Dashboard"),
  dashboardSidebar(collapsed = TRUE,
    sidebarMenu(menuItem(
      "Opioid Count Predictions",
      tabName = "Dashboard",
      icon = icon("table")
    ))#,
    
  ),
  dashboardBody(
    tags$script(HTML("addClass(‘sidebar-mini’);")),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    fluidRow(column(width = 12,
                    leafletOutput('map', height = "90vh")))
  )
)
server = function(input, output, session) {
  
  pred <- reactiveValues(pred_price = NULL,
                         pred_exp   = NULL)
  
  map = createLeafletMap(session, 'map')
  
  DR_colors <- as.character(DataRobotColors::DataRobot_get_palettes()$DR_Diverging)
  pal_DR <- colorFactor(
    palette = DR_colors,
    domain = preds_net$countopioid_PREDICTION
  )

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
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addPolygons(data = preds_net, fillColor = ~pal_DR(countopioid_PREDICTION),color = NA,
                         label = lapply(text, htmltools::HTML),
                    fillOpacity = 0.70, group = "Prediction") %>%
        # addPolygons(data = fd, fillColor = NA, color = "green",
        #             fillOpacity = 0, group = "Fire Station") %>% 
        #             
        addCircles(data = fd, weight = 2, fillColor = NA, color = "green",
                   fillOpacity = 0, radius = ~buffer, popup = ~COMPANY,
                   group = "Fire Station") %>% 
        setView(lat = 39.13923,lng = -84.52602,zoom = 12) %>%
        addProviderTiles("CartoDB.Positron",group = "CartoDB.Positron") %>%
        addProviderTiles("OpenStreetMap",group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery") %>%
        addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter") %>%
        addLayersControl(
          baseGroups = c("CartoDB.Positron", "OpenStreetMap",
                         "Esri.WorldImagery","CartoDB.DarkMatter"),
          overlayGroups = c("Prediction", "Fire Station"),
          options = layersControlOptions(collapsed = FALSE),
          position = "topright") %>% 
        hideGroup("Fire Station")
    })
  })
  
  #   #print(click)
  #   # get_color <- function(sign){
  #   #   if(grepl("\\+",sign)){
  #   #     return("<font color = 'red'>")
  #   #   } else if(grepl("-",sign)){
  #   #     return("<font color = 'blue'>")
  #   #   }
  #   # }

  
}

shinyApp(ui, server)


