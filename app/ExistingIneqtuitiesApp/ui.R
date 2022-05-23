#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Existing Inequities, Boston Region MPO"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel("sidebar", width = 3,
            checkboxGroupInput("dest", label = h3('Destinations'),
                               choices= list("Healthcare opportunities, Non-emergency"=1,
                                             "Healthcare opportunities, Emergency"=2,
                                             "Jobs"=3,
                                             "Higher Education"=4,
                                             "Essential Places"=5,
                                             "Open Space"=6),
                               selected = 1),
            radioButtons("demo", label = h3("Demographic Populations"),
                               choices= list("Low-Income, total population"=1,
                                             "Low-Income, adults"=2,
                                             "Minority, total population"=3,
                                             "Minoirty, adults"=4,
                                             "Zero-Vehicle Households"=5,
                                             "Clear Demographics"= 6),
                         selected=6
                               ),
            radioButtons('aggArea', label= h3("Aggregation Areas"),
                               choices = list("Boston Region MPO"= 1,
                                              "MAPC Community Types"= 2),
                               selected = 1
                               )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            column(width = 4,
                   girafeOutput("map", height = 300,
                       # brush = brushOpts(
                       #   id= "map_zoom",
                       #   resetOnNew = F
                       # )
                       )
            ), 
            column(width = 8,
                   h3(" "),
                   p("Select area of map to investigate below."),
                   p('Placeholder for descriptive and summary text.'))
            ),
          fluidRow(
            girafeOutput("mapZoom", height = 800)
            )
        )
    )
))
