#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(Cairo)
library(ggiraph)

mpoBoundary <- st_read("data/AggregationAreas.gpkg", "MPO_Boundary")
commTypes_byMuni <- st_read("data/AggregationAreas.gpkg", "CommunityTypes")
commTypes<- commTypes_byMuni %>% 
  group_by(communityType) %>% 
  summarize(geometry = st_union(geom))

higherEd <- st_read("data/DestinationData.gpkg", "higherEd_PT")
healthcare <- st_read("data/DestinationData.gpkg", "healthcare_PT")
# TODO: spell essential correctly and re-save destination data.
essentialPlace_poly <- st_read("data/DestinationData.gpkg", "essentailPlace_Final_POLY")
essentialPlace_pt <- st_read("data/DestinationData.gpkg", "essentailPlace_Final_PT")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # ranges2 <- reactiveValues(x = NULL, y = NULL)
    output$map <- renderGirafe({

        overview_map <- ggplot()+
          geom_sf(data = mpoBoundary, color = "black", size = 1.4, fill = "white")+
          geom_sf_interactive(data = commTypes, aes(fill = communityType, 
                                                    tooltip= communityType))+
          theme_void()+ 
        theme(legend.position="none")
        girafe(ggobj = overview_map)

    })
    output$mapZoom <- renderGirafe({
      detail_map <- ggplot()+
        geom_sf(data = mpoBoundary, color = "black", size = 1.4, fill = "white")+
        geom_sf_interactive(data = commTypes, aes(fill = communityType, tooltip = communityType))+
        geom_sf(data = higherEd, aes(size = enrollment), color= 'light gray', fill = 'transparent', alpha = .6)+
        geom_sf(data = essentialPlace_poly, fill= "white", alpha =.8)+
        geom_sf(data = healthcare, aes(shape = type), size= 2, color= 'brown', alpha= .7)+
        # coord_sf(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)+
        theme_void()+
        #theme(legend.title = element_text( size=10), legend.text=element_text(size=10))
        theme(legend.position="none")
        
      detail_map <- girafe(ggobj= detail_map)
      # detail_map <- girafe_options(detail_map,opts_zoom(min = .7, max = 10))
      detail_map
    })
    # observe({
    #   brush <- input$map_zoom
    #   if (!is.null(brush)) {
    #     ranges2$x <- c(brush$xmin, brush$xmax)
    #     ranges2$y <- c(brush$ymin, brush$ymax)
    #     
    #   } else {
    #     ranges2$x <- NULL
    #     ranges2$y <- NULL
    #   }
    # })

})
