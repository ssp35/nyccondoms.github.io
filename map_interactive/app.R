library(shiny)
library(leaflet)
library(shinydashboard)
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
    tags$head(tags$style(
        type = "text/css",
        "#controlPanel {background-color: rgba(255,255,255,0.8);}",
        ".leaflet-top.leaflet-right .leaflet-control {
      margin-right: 210px;
    }"
    )),
    leafletOutput(outputId = "mymap", width = "100%"),
    sidebarPanel(top = 10, right = 10, width = 210,
                 checkboxGroupInput("condoms_men", "Male Condoms?", choices = unique(cleaned_data_plot$condoms_male)),
                 checkboxGroupInput("condoms_fem", "Female Condoms?", choices = unique(cleaned_data_plot$condoms_female)),
                 checkboxGroupInput("lube", "Lubricant?", choices = unique(cleaned_data_plot$lubricant))),
    mainPanel(
        p("When we first took a look at the data, we were excited about the possibility of utilizing the latitude and longitude variables to create a map of where free safe sex products are located in NYC. Our goal was to create an interactive tool that would allow users to explore locations by borough and type of safe sex product."),
        p("We started off by trying to plot all the products as points in leaflet, but the number of points was overwhelming, and as a result unreadable. To mitigate this issue, we decided to cluster the markers on the map, so that the number of locations in a particular area was still represented, but visually, the map was more readable."),
        p("When looking at all of the marker clusters, we noticed a cluster located in Africa. Since our dataset is specific to NYC, we decided to investigate this. After examining the specific rows, we found out that this was due to longitude and latitude having a value of 0. These were likely missing values, so we filtered values that were “0” for the purpose of the interactive map."),
        p("To make this map as user-friendly as possible, we decided to make the values of  the variables facilityname, partnertype, partner_subtype, address, borough, phone, condoms_male, condoms_female, and lubricant, display when the user clicks on a specific marker. This way the interactive map can be utilized to display all the relevant information needed for a user to find a location that dispenses the free safe sex product(s) they are looking for. In our data cleaning, we had recoded the variables condoms_male, condoms_female, and lubricant to factors with values “0”, and “1.” As a result, “0” and “1” were displaying on the marker. Since this is not user-friendly, for the plot, we recoded the factor values to “No” and “Yes.”"),
        p("While the leaflet map displayed all of they key information we were looking to represent, our goal was to create a leaflet map in a shiny app, so that the user would be able to filter the map by type of safe sex product. To do this we explored various widget types, experimenting with select boxes and radio buttons, before eventually deciding on the checkbox group widget, because it allowed the user to select various combinations of safe sex products most easily."),
        p("We then spent time working on the aesthetics of app. We added a title to the top, and we moved the checkbox panel to the bottom of the map. "))
    )

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

shinyApp(ui, server)
