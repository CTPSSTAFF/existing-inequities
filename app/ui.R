library(ggiraph)
library(shiny)
library(shinyWidgets)
library(reactable)

shinyUI(fluidPage(

    # Application title
    titlePanel("Existing Inequities, Boston Region MPO"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel("", width = 3,
            pickerInput("dest", label = h4('Destinations'),
                               choices= list("Healthcare opportunities, Non-emergency"=1,
                                             "Healthcare opportunities, Emergency"=2,
                                             "Jobs"=3,
                                             "Higher Education"=4,
                                             "Essential Places"=5,
                                             "Open Space"=6,
                                             "Open Space, Conservation"= 7, 
                                             "Open Space, Paths"= 8),
                               selected = 1),
            pickerInput("modes", label = h4("Modes"),
                               choices = list("Walk"=1,
                                              "Bike" =2,
                                              "Transit (Bus and RT only)"= 3,
                                              "Transit (All modes)"= 4,
                                              "Drive"= 5
                                              ),
                        multiple= T,
                               selected = c(1,2,4,5)),
            pickerInput("time", label = h4("Travel Times"),
                        choices = list("15 minutes"= 1,
                                       "30 minutes"= 2,
                                       "45 minutes"= 3,
                                       "60 minutes"= 4),
                        #multiple = T,
                        #selected= c(1,2,3,4)),
                        selected = c(3)),
            # pickerInput("demo", label= h4("Demographic Overlay"),
            #             choices= list("None"= 0,
            #                           "Minority Status (Adults)"= 1,
            #                           "Minority Status" = 2,
            #                           "Income Status (Adults)" = 3,
            #                           "Income Status" = 4,
            #                           "Vehicle Availability" = 5),
            #             selected = c(0)),
            pickerInput('aggArea', label= h4("Aggregation Areas"),
                               choices = list(
                                              "Developing Suburbs: Maturing New England Towns"= 1,
                                              "Inner Core: Streetcar Suburbs"= 2,
                                              "Developing Suburbs: Country Suburbs"= 3,
                                              "Maturing Suburbs:Established Suburbs and Cape Cod Towns"=4,
                                              "Maturing Suburbs: Mature Suburban Towns"= 5,
                                              "Inner Core: Metro Core Communities" = 6,
                                              "Regional Urban Centers: Sub-Regional Urban Centers" = 7,
                                              "Boston Region MPO"= 8),
                              # multiple= T,
                               selected = 8
                               )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel(strong("Access Plots"),
                               fluidRow(
                                 plotOutput("access_plots", height = 800),
                                 h4("Weighted Avg Access Opportunities for Boston Region MPO and Aggregation Area"),
                                 reactableOutput("avgs"),
                                 # column(6,
                                 #        h4("Weighted Avg Access Opportunities for Boston Region MPO"),
                                 #        reactableOutput("avgs_mpo")
                                 #        ),
                                 # column(6, 
                                 #        h4("Weighted Avg Access Opportunities for Aggregation Area"),
                                 #        reactableOutput("avgs_agg")),
                                 #girafeOutput("mapZoom", height = 800)
                               )
                               ),
                      # tabPanel(strong("Compare Access"),
                      #          ),
                      # tabPanel(strong("Travel Costs"),
                      #          ),
                      tabPanel(strong("About Project"),
                               )
                      ),
          
          
          # fluidRow(
          #   column(width = 4,
          #          girafeOutput("map", height = 300,
          #              # brush = brushOpts(
          #              #   id= "map_zoom",
          #              #   resetOnNew = F
          #              # )
          #              )
          #   ), 
          #   column(width = 8,
          #          h4(" "),
          #          p("Select area of map to investigate below."),
          #          p('Placeholder for descriptive and summary text.'))
          #   ),
         
        )
    )
))
