library(shiny)
library(shinyWidgets)
library(reactable)
library(readr)
library(leaflet)
library(ggiraph)

# Prep agg area names and icon files. note, order must match the choices list
aggAreaIcons<- c('ct8.png','ct3.png','ct1.png','ct6.png','ct2.png','ct4.png','ct5.png','ct7.png')
aggAreaNames <- c(
  "Boston Region MPO", "Developing Suburbs: Country Suburbs","Developing Suburbs: Maturing New England Towns",
  "Inner Core: Metro Core Communities","Inner Core: Streetcar Suburbs",
  "Maturing Suburbs: Established Suburbs and Cape Cod Towns","Maturing Suburbs: Mature Suburban Towns" ,
  "Regional Urban Centers: Sub-Regional Urban Centers"
  )
index_vars <- read_rds("data/hta_index_vars.rds")

shinyUI(fluidPage(
  tags$head(
    tags$style(HTML(
      "
        .leaflet-container {
    background-color: white;
        }
  text {
  font-family: sans-serif;
  }
      "
    ))
  ),

    # Application title
    titlePanel("Identifying Existing Destination Access Inequities in the Boston Region "),
    tabsetPanel(type = "tabs",
                tabPanel(strong("Access Plots"),
                         column(12,  
                                br(),
                                p(strong("Instructions:")," This application answers the question: ",
                                  em(strong("how does access differ between demographic groups and modes for a given destination and travel time?")), 
                                  " Use the options in the sidebar to change the maps below. Each map shows the number destinations people living 
                                 within the selected aggregation area. Users can select the MPO region or a smaller aggregation area, as well as 
                                 filter the results by demographic group. See the “About the Project” tab for more information about the map filters."),
                                p(strong("How to Interpret the Maps:"), "The application is designed for users to compare access to a selected destination 
                                   for different modes, demographic groups, and travel time thresholds. Only one aggregation area can be selected at a time. 
                                   Areas in darker purple indicate that people living in that location have greater access to the selected destination than 
                                   areas in lighter purple."),
                                br()),
                         column(3, 
                                pickerInput("dest", label =  p(h4(strong('Destinations')),
                                                               h5('Select a destination from the dropdown menu:')
                                                               ),
                                                               choices= list(
                                                                 "Essential places"=5,
                                                                 "Healthcare opportunities, emergency"=2,
                                                                 "Healthcare opportunities, non-emergency"=1,
                                                                 "Higher education"=4,
                                                                 "Jobs"=3,
                                                                 "Open Space, all parks"=6,
                                                                 "Open Space, large parks"= 7,
                                                                 "Open Space, paths"= 8),
                                                               selected = 5),
                                pickerInput(inputId = "mode_time", label = p(h4(strong("Travel times and modes")),
                                                                   h5('Select one or more travel combinations from the dropdown menu:')),
                                            choices =  c("15 minute Bike"= 1," 15 minute Walk"= 2),#choice_list,
                                            multiple  =  T, selected = c(1,2)),
                                                   pickerInput("demo", label= p(h4(strong('Equity Population Filter')),
                                                                                h5("Select a demographic group from the dropdown menu:")
                                                                                ),
                                                               choices= list("None"= 0,
                                                                             #"Total Population" = 1,
                                                                             #"Total Population (Adults)" = 2,
                                                                             #"Minority Status (Adults)"= 3,
                                                                             "Minority population" = 4,
                                                                             #"Income Status (Adults)" = 5,
                                                                             "Low-income population" = 6,
                                                                             "Zero-vehicle households" = 7),
                                                               selected = c(0)),
                                pickerInput('aggArea', label=p(h4(strong('Aggregation Areas')),
                                                               h5("Select an aggregation area from the dropdown menu:")
                                                               ),
                                            choices = list(
                                              "Boston Region MPO"= 8,
                                              "Developing Suburbs: Country Suburbs"= 3,
                                              "Developing Suburbs: Maturing New England Towns"= 1, 
                                              "Inner Core: Metro Core Communities" = 6,
                                              "Inner Core: Streetcar Suburbs"= 2,
                                              "Maturing Suburbs:Established Suburbs and Cape Cod Towns"=4,
                                              "Maturing Suburbs: Mature Suburban Towns"= 5,
                                              "Regional Urban Centers: Sub-Regional Urban Centers" = 7
                                                                 ),
                                                               choicesOpt = list(
                                                                 content = mapply(aggAreaNames, 
                                                                                  aggAreaIcons, 
                                                                                  FUN = function(area, icon){
                                                                                    HTML(paste(tags$img(src= icon, alt = area, width = 85, height = 85), area))}, 
                                                                                  SIMPLIFY = F, 
                                                                                  USE.NAMES = F
                                                                 )),
                                                               selected = 8)),
                         column(9,
                               fluidRow(
                                 girafeOutput("access_plots", height = 1000),
                               ))
                               ),
                      tabPanel(strong("Compare Access"),
                               column(12, 
                                      br(),
                              
                               p(strong("Instructions:"), "Navigate the table below using the arrows to display entries for different destinations and aggregation areas."),
                               p(strong("How to Interpret the Results:"), " This table should be used to compare access for selected destinations and travel 
                                 time thresholds between equity populations and their non-equity counterparts between different aggregation areas and the MPO region. 
                                 This table contains meaningful ratios. Ratios for all combinations of modes, destinations, and travel times analyzed can be downloaded ", 
                                 a("here", href="https://github.com/CTPSSTAFF/existing-inequities/blob/main/output/access_all_comp.csv", target="_blank" ), "."),
                               tags$div(
                                 tags$ul(
                                   tags$li(strong("Average Number of Opportunities Accessible Per Person:"), " results are provided for each equity population"),
                                   tags$li(HTML("<b>Ratio:</b> the ratio of the number of destinations accessible to the equity population to the total number of 
                                   destinations accessible to their respective non-equity Population. The ratio is available for the MPO area and the 
                                   selected aggregation area. Ratios where the equity population has less access are <span style= 'color:red'> shown in red</span>."))
                                 )),
                               br()),
                               h4(strong("Weighted Average of the Number of Accessible Opportunities for the Boston Region MPO and Aggegration Areas")),
                               reactableOutput("access_all"),
                              ),
                      tabPanel(strong("Travel Costs"),
                               column(12,  
                                      br(),
                                      p("This tab shows two different ways to analyze transportation costs in the region. The first map shows several variables related to 
                                        transportation and housing costs for Census tracts in the Boston region. These are based on housing cost data from the Census Bureau 
                                        and the modeled transportation costs from the ", a("Center for Neighborhood Technology", href = "https://cnt.org/"), ". The second map shows the monetized value of travel 
                                        time (VTT) for trips to select destinations in the Boston Region MPO. VTT takes into account the perceived cost of a trip (as opposed to the 
                                        objective clock time of that trip) based on the opportunity cost of time spent on the trip."),
                                    
                                      br()),
                               fluidRow(
                                 column(1,),
                                 column(4, 
                                    h4(strong('Housing and Transportation Costs in the Boston Region')),
                                    p(strong("Instructions:")," This map answers the question: ",
                                      em(strong("how do household transportation and/or housing costs differ across the MPO region?"))),
                                    p("Use the drop-down menu to select the variable to display on the map, and hover over a Census tract to view housing and transportation cost data. 
                                      See the 'About Project' tab for data sources. (A “regional typical household” is the average for all households in the Boston MPO region.)"),
                                     selectInput("index_var", label= "Select Variable:", 
                                                 choices = index_vars, 
                                                 selected = "ht_ami",
                                                 width = '100%'),
                                      ),
                                column(7,
                                      leafletOutput("index_map", height = 700),
                                      )),
                               br(),
                               fluidRow(
                                 column(1,),
                                 column(4, 
                                               h4(strong('Value of Travel Time for Selected Trips')),
                                               p(strong("Instructions:")," This map answers the question: ",
                                                 em(strong("how does the value of travel time differ between transportation modes for a sample origin-destination pair?")), 
                                                 "Select a destination below. Hover over each trip to see the difference in travel costs between driving and public transit."),
                                        p("Origins were found by finding the center of every census tract in the Boston Region MPO. 
                                          Destinations were selected from the essential places. 
                                          Cost deltas can only be calculated where there is both a drive cost and a transit cost. There is no cost delta where there is no transit path available."),
                                        selectInput("delta_dest", label =  "Select destination:",
                                                    choices = list("Downtown Boston" = 1,
                                                                   "Framingham" = 2,
                                                                   "Longwood Medical Area" = 3,
                                                                   "Lynn" = 4, 
                                                                   "Quincy" = 5),
                                                    selected = 3),
                                                      
                                                    ),
                                        column(7,
                                               leafletOutput("delta_map", height = 700),
                                               ))
                                        ),
                      tabPanel(strong("About Project"),
                               column(2),
                               column(8,
                                br(), 
                                h4(strong("Introduction")),
                               p("In 2022, the Boston Region Metropolitan Planning Organization (MPO) conducted a study called Identifying Transportation Inequities 
                                 in the Boston Region.  The study sought to quantify existing transportation inequities among environmental justice (EJ)—minority and 
                                 low-income—populations and zero-vehicle households in the Boston region using several destination access metrics. In addition, the study 
                                 explored two methodologies for analyzing transportation costs. This application shows the results of that study. Detailed methodology 
                                 can be found at the study’s GitHub page,", 
                                 a("here.", href= "https://github.com/CTPSSTAFF/existing-inequities", target="_blank" )),
                               br(),
                               h4(strong("Definitions")),
                               p(strong("Aggregation Area:"), "These are based on Community Types from the Metropolitan Area Planning Council (MAPC). Community Types are defined used land 
                                 use and housing patterns, growth trends, and projected development patterns. Because MAPC developed these definitions for the entire state, not all Community 
                                 Types are represented in the Boston MPO region. For more information, see ",
                                 a("this page.", href= "https://www.mapc.org/wp-content/uploads/2017/09/Massachusetts-Community-Types-Summary-July_2008.pdf",  target="_blank" )),
                               tags$div(
                                 tags$ul(
                                   tags$li(strong("Inner Core:"),
                                           tags$ul(
                                           tags$li(strong("Metropolitan Core Communities:"), " high density inner cities"),
                                           tags$li(strong("Streetcar Suburbs:"), " historic, high-density suburbs near the urban core")
                                           )),
                                   tags$li(strong("Regional Urban Centers:"),
                                    tags$ul(
                                     tags$li(strong("Subregional Urban Centers:"), " large, high-density urban centers that are not near Boston"),
                                   )),
                                   tags$li(strong("Maturing Suburbs:"),
                                           tags$ul(
                                             tags$li(strong("Mature Suburban Towns:"), " moderate-density towns that are nearly built-out"),
                                             tags$li(strong("Established Suburbs:"), " lower-density towns that are nearly built-out")
                                           )),
                                   tags$li(strong("Developing Suburbs:"),
                                           tags$ul(
                                             tags$li(strong("Maturing New England Towns:"), " towns with well-defined town centers, mixed densities, and room to grow"),
                                             tags$li(strong("Country Suburbs:"), " towns with very low density, with country character and room to grow")
                                           )),
                                   
                                 )
                               ),
                               p("Using Community Types allow users to visualize how transportation access varies among different types of communities, 
                                 controlling for land use and population density. Because regionwide analyses smooth out differences between communities 
                                 based on these characteristics, looking at each community type separately allows the user to visualize and identify those 
                                 differences. By controlling for land use and density, users can see where there are gaps in the transportation network or 
                                 in public transit service EJ populations and zero-vehicle households relative to other residents. Since denser communities—such 
                                 as Inner Core communities—are generally more suitable for public transit and therefore have greater transit access, the ideal 
                                 comparison is between Community Types and relative to the MPO region: ", 
                                 em(strong( "how does access differ between demographic groups and mode for a given destination and travel time?"))),
                               p(strong("Destinations:"), "Destinations in the application are defined as described below. For more details, see study’s GitHub page,", 
                                 a("here.", href= "https://github.com/CTPSSTAFF/existing-inequities", target="_blank" )),
                               tags$div(
                                 tags$ul(
                                   tags$li(strong("Healthcare (emergency):"), " includes acute care hospitals"),
                                   tags$li(strong("Healthcare (non-emergency):"), " includes medical clinics and community health centers"),
                                   tags$li(strong("Employment:"), " data from the 2018 LODES (LEHD Origin-Destination Employment Statistics)"),
                                   tags$li(strong("Essential Places:"), " includes three types of destinations: health, civic, and food. The health type consists of all healthcare destinations and retail pharmacies. The civic type consists of town halls, post offices, and libraries. The food type consists of farmer's markets, and grocery stores."),
                                   tags$li(strong("Higher Education: ")," colleges and universities where more than 50% of undergraduates live off campus and/or there is graduate enrollment."),
                                   tags$li(strong("Parks and Open Space:")," includes shared used paths and parks lager than one-half acre")
                                 )),
                               p(strong("Equity Population  Definitions")),
                               tags$div(
                                 tags$ul(
                                   tags$li("A person is considered ",strong("low-income"),
                                   " if their family income is less than 200% of the federal poverty level. Data are from the 2016-20 American Community Survey."),
                                   tags$li("A person is considered a ", strong("minority"),
                                           " if they identify as a race other than White or as Hispanic or Latino/a. Data are from the 2020 US Census."),
                                   tags$li(strong("Zero-vehicle households"), " are those that do not have access to a personal vehicle. Data are from the 2016-20 American Community Survey.")
                                 )
                               ),
                               br(),
                               h4(strong("Travel Cost Methodology")),
                               p(strong("Housing and Transportation Costs in the Boston Region:"), "These data are from the Center for Neighborhood Technology (CNT). CNT 
                                 calculates the percent of household income spent on transportation and housing costs from an in-house transportation cost model and housing 
                                 costs from the American Community Survey. The CNT data were last updated in 2017 and use 2010 Census geography. For more information about 
                                 the methodology and data used, see CNT’s", a("technical documentation.", href= "https://htaindex.cnt.org/about/HTMethods_2016.pdf")),
                               p(strong("Value of Travel Time for Selected Trips:"), "Conveyal was used to calculate travel times on the roadway and public transit networks 
                                 between origin-destination pairs. The VTT was calculated using assumptions from the Victoria Transportation Policy Institute’s ", 
                                 a("Valuing Transit Service Quality Improvements.", href = "https://www.vtpi.org/traveltime.pdf")),
                               br(),
                               h4(strong("Statement About Error")),
                               p("Like any modeling process, this work assumes a low, but not zero, amount of error due to the data 
                                 and analysis process that is used. Sources of error include the data source (American Community Survey data are estimates) and the 
                                 allocation of demographic data from vector polygons to raster cells. For more information on the methodology, see the study’s GitHub page,", 
                                 a("here.", href= "https://github.com/CTPSSTAFF/existing-inequities", target="_blank" ))
                               ), 
                          column(2)
                               
                               )
                      )
))
