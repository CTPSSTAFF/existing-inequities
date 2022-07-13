
library(shiny)
library(shinyWidgets)
library(reactable)

aggAreaIcons<- c(
  'ct1.png', 'ct2.png','ct3.png','ct4.png','ct5.png','ct6.png', 'ct7.png','ct8.png')
aggAreaNames <- c("Developing Suburbs: Maturing New England Towns","Inner Core: Streetcar Suburbs",
                   "Developing Suburbs: Country Suburbs", "Maturing Suburbs: Established Suburbs and Cape Cod Towns",
                   "Maturing Suburbs: Mature Suburban Towns" , "Inner Core: Metro Core Communities",
                   "Regional Urban Centers: Sub-Regional Urban Centers", "MPO")
  

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
            pickerInput("demo", label= h4("Demographic Overlay"),
                        choices= list("None"= 0,
                                      #"Total Population" = 1,
                                      #"Total Population (Adults)" = 2,
                                      "Minority Status (Adults)"= 3,
                                      "Minority Status" = 4,
                                      "Income Status (Adults)" = 5,
                                      "Income Status" = 6,
                                      "Vehicle Availability" = 7),
                        # multiple = T,
                        # options =  list(
                        #   "max-options" = 2,
                        #   "max-options-text" = "Limit two demographic groups"
                        # ),
                        selected = c(0)),
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
                        choicesOpt = list(
                          # TODO: https://stackoverflow.com/questions/20391156/multiple-lines-of-text-next-to-image-css-html
                          content = mapply(aggAreaNames, 
                                           aggAreaIcons, 
                                           FUN = function(area, icon){
                          HTML(paste(tags$img(src= icon, alt = area, width = 75, height = 75), area))}, 
                          SIMPLIFY = F, 
                          USE.NAMES = F
                          )),
                        selected = 8)
                             
                    
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel(strong("Access Plots"),
                               fluidRow(
                                 plotOutput("access_plots", height = 900),
                                 reactableOutput("avgs"),
                               )
                               ),
                      tabPanel(strong("Compare Access"),
                               h4("Weighted Avg Access Opportunities for Boston Region MPO and Aggregation Area"),
                               reactableOutput("access_all"),
                              ),
                      tabPanel(strong("Travel Costs"),
                               h4(),
                               p("From the Center for Neighborhood Technology"),
                               plotOutput("index_plot", height = 500),
                               ),
                      tabPanel(strong("About Project"),
                               p("The purpose of this study is to develop a baseline assessment of existing transportation
                               inequities in the Boston region. While the equity policies applied by the MPO and other
                               transportation agencies often take the status quo as a given and attempt to prevent making
                               inequities worse through future investments, this study will take a historical perspective
                               and attempt to identify existing inequities that have been caused by past decisions and
                               identify opportunities for the MPO to reduce the divergent outcomes between population groups."),
                               p("See methodology notes", a("here", href= "https://github.com/CTPSSTAFF/existing-inequities", target="_blank" ), ".")
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
