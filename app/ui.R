
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
    titlePanel("Identifying Existing Destination Access Inequities in the Boston Region "),
    tabsetPanel(type = "tabs",
                tabPanel(strong("Access Plots"),
                         column(3, 
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
                                                               selected = 8)),
                         column(9,
                               fluidRow(
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
                                 plotOutput("access_plots", height = 900),
                                 reactableOutput("avgs"),
                               ))
                               ),
                      tabPanel(strong("Compare Access"),
                               br(),
                               h4(strong("Weighted Average of the Number of Accessible Opportunities for the Boston Region MPO and Aggegration Areas")),
                               p(strong("Instructions:"), "Enter your filters in the boxes below using the filters in the dropdown menus on the left. 
                                 The results show only those that are specified in the filter."),
                               p(strong("How to Interpret the Results:"), " This table should be used to compare access for selected destinations and travel 
                                 time thresholds between equity populations and their non-equity counterparts between different aggregation areas and the MPO region."),
                               tags$div(
                                 tags$ul(
                                   tags$li(strong("Average Number of Opportunities Accessible Per Person:"), " results are provided for each equity population"),
                                   tags$li(HTML("<b>Ratio:</b> the ratio of the number of destinations accessible to the equity population to the total number of 
                                   destinations accessible to their respective non-equity Population. The ratio is available for the MPO area and the 
                                   selected aggregation area. The Aggregation Area Ratio will be left blank if the Boston MPO Region is specified in 
                                   the filter. Ratios where the equity population has less access are <span style= 'color:red'> shown in red</span>."))
                                 )),
                               reactableOutput("access_all"),
                              ),
                      tabPanel(strong("Travel Costs"),
                               h4(),
                               p("From the Center for Neighborhood Technology"),
                               plotOutput("index_plot", height = 500),
                               ),
                      tabPanel(strong("About Project"),
                               column(2),
                               column(8,
                                br(), 
                                # h4(strong("Introduction")),
                               p("In 2022, the Boston Region Metropolitan Planning Organization (MPO) conducted a study called Identifying Transportation Inequities in the Boston Region. 
                                 The purpose of the study was to develop a baseline assessment of existing transportation inequities in the Boston region. MPO policies related to equity 
                                 align with federal civil rights and EJ requirements that focus on preventing future discrimination that may result from MPO investments. To improve the 
                                 effectiveness of current policies and ultimately the transportation outcomes of environmental justice (EJ) populations in the Boston region, this study 
                                 sought to quantify existing transportation inequities among EJ populations using several destination access and transportation cost metrics. This application 
                                 shows the results for that study. Detailed methodological information can be found at the study’s GitHub page,", 
                                 a("here", href= "https://github.com/CTPSSTAFF/existing-inequities", target="_blank" ), "."),
                               br(),
                               h4(strong("Definitions")),
                               p(strong("Aggregation Area:"), "These are based on Community Types from the Metropolitan Area Planning Council (MAPC). Community Types are defined used land 
                                 use and housing patterns, growth trends, and projected development patterns. Because MAPC developed these definitions for the entire state, not all Community 
                                 Types are represented in the Boston MPO region. For more information, see",
                                 a("this page", href= "https://www.mapc.org/wp-content/uploads/2017/09/Massachusetts-Community-Types-Summary-July_2008.pdf"), "."),
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
                               p("Using Community Types allow users to visualize how transportation access varies among different types of communities, controlling for land use and population 
                                 density. Because regionwide analyses smooth out differences between communities, looking at each community type separately allows the user to visualize and 
                                 identify those differences. By controlling for land use and density, users can see where there are gaps in the transportation network or in public transit 
                                 service EJ populations and zero-vehicle households. Since denser communities—such as Inner Core communities—are generally more suitable for public transit 
                                 and therefore have greater transit access, the ideal comparison is between Community Types and relative to the MPO region:", 
                                 em(strong( "how does access differ between demographic groups and mode for a given destination and travel time?"))),
                               p(strong("Destinations:"), "Destinations in the application are defined as described below. For more details, see study’s GitHub page,", 
                                 a("here", href= "https://github.com/CTPSSTAFF/existing-inequities", target="_blank" ), "."),
                               tags$div(
                                 tags$ul(
                                   tags$li(strong("Healthcare (emergency):"), " includes acute care hospitals"),
                                   tags$li(strong("Healthcare (non-emergency):"), " includes medical clinics and community health centers"),
                                   tags$li(strong("Employment:"), " data from the 2018 LODES (LEHD Origin-Destination Employment Statistics)"),
                                   tags$li(strong("Essential Places:"), " includes three types of destinations: health, civic, and food. The health type consists of all healthcare destinations and retail pharmacies. The civic type consists of townhalls, post offices, and libraries. The food type consists of farmer's markets, and grocery stores."),
                                   tags$li(strong("Higher Education: ")," colleges and universities where more than 50% of undergraduates live off campus and/or there is graduate enrollment."),
                                   tags$li(strong("Parks and Open Space:")," includes shared used paths and parks lager than one-half acre")
                                 )),
                               p(strong("Populations of Concern Definitions")),
                               tags$div(
                                 tags$ul(
                                   tags$li("A person is considered ",strong("low-income"),
                                   " if their family income is less than 200% of the federal poverty level. Data are from the 2016-20 American Community Survey."),
                                   tags$li("A person is considered a ", strong("minority"),
                                           " if they identify as a race other than White or as Hispanic or Latino/a. Data are from the 2020 US Census."),
                                   tags$li(strong("Zero-vehicle households"), " are those that do not have access to a personal vehicle. Data are from the 2016-20 American Community Survey.")
                                 )
                               ),
                               p(strong("How These Data Will Be Used")),
                               p(strong("Statement About Error:"), "Like any modeling process, this work is assumes a low, but not zero, amount of error due to data and analysis process that is used. 
                                 Sources of error include the data source (American Community Survey data are estimates) and the allocation of demographic data from 
                                 vector polygons to raster cells. For more information on the methodology, see the study’s GitHub page,", 
                                 a("here", href= "https://github.com/CTPSSTAFF/existing-inequities", target="_blank" ), ".")
                               ),



#                                
                          column(2)
                               
                               )
                      )
))
