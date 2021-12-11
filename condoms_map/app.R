library(shiny)
library(leaflet)
library(tidyverse)
library(rvest)
library(httr)

condom_data = 
    GET("https://data.cityofnewyork.us/resource/4kpn-sezh.csv") %>% 
    content("parsed")
cleaned_data = 
    condom_data %>% 
    select(facility_pk, facilityname, partnertype, partner_type_detailed, 
           address, borough, zipcode, latitude, longitude, phone, condoms_male,
           fc2_female_insertive_condoms, lubricant) %>% 
    rename(condoms_female = fc2_female_insertive_condoms) %>% 
    rename(partner_subtype = partner_type_detailed) %>% 
    mutate(
        condoms_male = as.factor(condoms_male), 
        condoms_male = fct_recode(condoms_male, "0" = "FALSE", "1" = "TRUE"),
        condoms_female = as.factor(condoms_female),
        condoms_female = fct_recode(condoms_female, "0" = "FALSE", "1" = "TRUE"),
        lubricant = as.factor(lubricant),
        lubricant = fct_recode(lubricant, "0" = "FALSE", "1" = "TRUE"),
        borough = as.factor(borough)
    )

cleaned_data$partnertype[cleaned_data$partnertype == "Community Based Organization/Non-Profit"] =
    "Community-Based Organization/Non-Profit"

cleaned_data_plot =
    cleaned_data %>% 
    filter(longitude != 0) %>% #removed 0 long and lat
    filter(latitude != 0) %>%
    mutate(condoms_male = fct_recode(condoms_male, "No" = "0", "Yes" = "1")) %>%
    mutate(condoms_female = fct_recode(condoms_female, "No" = "0", "Yes" = "1")) %>%
    mutate(lubricant = fct_recode(lubricant, "No" = "0", "Yes" = "1"))

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
    titlePanel("Where Can I Find Safe Sex Products in NYC?"),
    mainPanel(
        h4("Instructions"),
        h6("To use the map, select the checkboxes corresponding to the type of safer sex product you are looking for. If you aren’t interested in a particular product or products, select both “Yes” and “No.” For example, if you are specifically looking for locations that carry female condoms and lubricant, select “Yes” for both female condoms and lubricant, and select both “Yes” and “No” for male condoms. Click on the marker clusters and markers to explore the various locations and their corresponding information.")),
    tags$head(tags$style(
        type = "text/css",
        "#controlPanel {background-color: rgba(255,255,255,0.8);}",
        ".leaflet-top.leaflet-right .leaflet-control {margin-right: 210px;}")),
    leafletOutput(outputId = "mymap", width = "100%"),
    sidebarPanel(top = 10, right = 10, width = 210,
                 checkboxGroupInput("condoms_men", "Male Condoms?", 
                                    choices = unique(cleaned_data_plot$condoms_male)),
                 checkboxGroupInput("condoms_fem", "Female Condoms?", 
                                    choices = unique(cleaned_data_plot$condoms_female)),
                 checkboxGroupInput("lube", "Lubricant?", 
                                    choices = unique(cleaned_data_plot$lubricant))))

server <- function(input, output, session) {
    output$mymap <- renderLeaflet({
        req(input$condoms_men)
        cleaned_data_plot = cleaned_data_plot %>%
            dplyr::filter(condoms_male %in% input$condoms_men)
        req(input$condoms_fem)
        cleaned_data_plot = cleaned_data_plot %>%
            dplyr::filter(condoms_female %in% input$condoms_fem)
        req(input$lube)
        cleaned_data_plot = cleaned_data_plot %>%
            dplyr::filter(lubricant %in% input$lube)
        leaflet(cleaned_data_plot) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addMarkers(clusterOptions = markerClusterOptions(),
                       lng = cleaned_data_plot$longitude, lat = cleaned_data_plot$latitude,
                       popup = paste("Facility Name:", cleaned_data_plot$facilityname, "<br>",
                                     "Partner Type:", cleaned_data_plot$partnertype, "<br>",
                                     "Partner Subtype:", cleaned_data_plot$partner_subtype, "<br>",
                                     "Address:", cleaned_data_plot$address, "<br>",
                                     "Borough:", cleaned_data_plot$borough, "<br>",
                                     "Phone:", cleaned_data_plot$phone, "<br>",
                                     "Male Condoms:", cleaned_data_plot$condoms_male, "<br>",
                                     "Female Condoms:", cleaned_data_plot$condoms_female, "<br>",
                                     "Lubricant:", cleaned_data_plot$lubricant)) %>%
            setView(lat = 40.7, lng = -74, zoom = 10)
    })
    }

shinyApp(ui = ui, server = server)
