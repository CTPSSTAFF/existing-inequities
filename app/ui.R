library(ggiraph)
library(shiny)
library(reactable)

shinyUI(fluidPage(

    # Application title
    titlePanel("Existing Inequities, Boston Region MPO"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel("", width = 3,
            radioButtons("dest", label = h3('Destinations'),
                               choices= list("Healthcare opportunities, Non-emergency"=1,
                                             "Healthcare opportunities, Emergency"=2,
                                             "Jobs"=3,
                                             "Higher Education"=4,
                                             "Essential Places"=5,
                                             "Open Space"=6,
                                             "Open Space, Conservation"= 7, 
                                             "Open Space, Paths"= 8),
                               selected = 1),
            checkboxGroupInput("modes", label = h3("Modes"),
                               choices = list("Walk"=1,
                                              "Bike" =2,
                                              "Transit, Bus and RT only"= 3,
                                              "Transit, All modes"= 4,
                                              "Drive"= 5
                                              ),
                               selected = c(1,2,3,4,5)),
            radioButtons('aggArea', label= h3("Aggregation Areas"),
                               choices = list(
                                              "Developing Suburbs: Maturing New England Towns"= 1,
                                              "Inner Core: Streetcar Suburbs"= 2,
                                              "Developing Suburbs: Country Suburbs"= 3,
                                              "Maturing Suburbs:Established Suburbs and Cape Cod Towns"=4,
                                              "Maturing Suburbs: Mature Suburban Towns"= 5,
                                              "Inner Core: Metro Core Communities" = 6,
                                              "Regional Urban Centers: Sub-Regional Urban Centers" = 7,
                                              "Boston Region MPO"= 8),
                               selected = 8
                               )
            # radioButtons("demo", label = h3("Demographic Populations"),
            #                    choices= list("Low-Income, total population"=1,
            #                                  "Low-Income, adults"=2,
            #                                  "Minority, total population"=3,
            #                                  "Minoirty, adults"=4,
            #                                  "Zero-Vehicle Households"=5,
            #                                  "Clear Demographics"= 6),
            #              selected=6
            #                    ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
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
          #          h3(" "),
          #          p("Select area of map to investigate below."),
          #          p('Placeholder for descriptive and summary text.'))
          #   ),
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
        )
    )
))
