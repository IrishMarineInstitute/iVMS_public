library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(dplyr)
library(leaflet.extras2)
library(ggplot2)
library(devtools)

model_output <- readRDS("Model_output_Anonymised.rds") #created in data prep
fishing_events <- readRDS("Fishing_Anon.rds")  #created in data prep
df <- fishing_events

inshore_grid <- readRDS("Inshore_Grid.rds")  #from inshore team
map_fishing <- readRDS("map_fishing.rds")  #created in data prep
map_mix_transit <- readRDS("map_mixtransit.rds")   #created in data prep
map_transit <- readRDS("map_transit.rds")  #created in data prep
landing_sites <- readRDS("landing_sites.rds")  #from model output/Sophie
source_url("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
#source(App_with_anonymised_data/calendarHeat.R', echo=TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "iVMS Application"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",tabName = "home", icon = icon("dashboard")),
      menuItem("Map", tabName = "map", icon = icon("map-marker-alt")),
      menuItem("Model details", tabName = "modelling", icon = icon("th"))
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                column(12,
                box(
                  background = "olive", width = NULL,
                  h4("Filter data using the widgets below"),
                  br(),
                  column(3,dateRangeInput("dateselector", "Date", start=min(fishing_events$Date),
                                          end = max(fishing_events$Date), separator = " to ")),
                  column(3,selectInput("vesselselector", "Vessels", choices=c(levels(as.factor(fishing_events$ID_anon))),
                                       selected = NULL, multiple=T)),
                  column(3,selectInput("location","ICES Area.Rectangle", choices= c(levels(fishing_events$Area_Rect)),
                                       multiple=T, selectize=T)), 
                  column(3,selectInput("gridsquare","Inshore Grid Square", choices= c(levels(as.factor(fishing_events$InshoreGrid_SQ))),
                                       multiple=T))
                  #selectInput("gear","Gear", choices= "All", multiple=T, selectize=T)
                )
                )
              ),
              fluidRow(
                       valueBoxOutput("box1",width = 4),
                       valueBoxOutput("box2",width = 4),
                       valueBoxOutput("box3",width = 4)),
              br(),
              fluidRow(
               # column(7,
                       tabBox(
                         id = "tabset1",width = 12, side = "right",
                         tabPanel("Distance",
                                  #numericInput("cutoff1", label = h3("Input fishing cut off in Km per fishing event"), value = 0),
                                  plotlyOutput("day1"),
                                  plotOutput("heat1"),
                                  ),
                         tabPanel("Duration",
                                  #numericInput("cutoff2", label = h3("Input fishing cut off in minutes"), value = 0),
                                  plotlyOutput("day2"),
                                  plotOutput("heat2"),
                                  ),
                         tabPanel("Number of Pots", 
                                  fluidRow(
                                    column(6, numericInput("spacing", label = h4("Enter a spacing parameter to calculate number of pots (Meters)"), value = 2.5)),
                                    column(6, numericInput("cutoff3", label = h4("Enter a cut-off level in pots per fishing event"), value = 4000))
                                  ),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  plotlyOutput("ev3")
                                  )
                         )
                       ),
                #column(5, 
              fluidRow(
                br(),
                tabBox(
                  id = "tabset2",width = 12, 
                  tabPanel("Time",
                           plotlyOutput("vesselplot")),
                  tabPanel("Km",
                           plotlyOutput("vesselplot2"))
                  )
                )
              ),
      tabItem(tabName = "map",
              h3("Model Output (Note: locations are interpolated)"),
              leafletOutput("map", height='90vh')
              ),
      tabItem(tabName = "modelling",
              tags$b("Model Details"),
              tags$p("The iVMS pings are collected from gps systems on board inshore fishing vessels. These pings are currently being collected at 1min intervals. This data is then cleaned to remve any pings sent while in port (a 500m buffer zone is applied around any port, quay, pier or other landing site). Once cleaned and pepped, a supervised classification model takes as input a file with raw VMS locations of fishing vessels (only potters so far) and uses a Hidden Markov Model (HMM) and a speed filter to classify locations by potential activity, i.e. fishing or not fishing (transit) based on the step length and angles between successive locations. The model is asked to discriminate between 3 modes that are later interpreted as 1-Fishing, 2-Uncertain activity (slow transit or mixture of transit and fishing), 3-Transit. The choice of a 3 states model has been made based on tests.
The model is set as a conservative one so that it is more likely to underestimate fishing locations rather than find false positives. 
Because the model needs perfectly regular GPS pings to converge properly, the VMS locations are interpolated. Therefore, the resulting file with VMS locations and modelled activity does not contain the exact REAL VMS locations but a linear interpolation of these locations. 
"),
              tags$b("Limitations"),
              tags$p("The program checks for various problems in the file that may impair the model results (number of locations per trip, ping irregularities, duplicated locations) and will remove from the analyse tracks that will compromise the function. A notification with the number of trips removed will appear on the screen. 
Despite this automatic checking some exceptions may arise and the model may give unrealistic results for some vessels. For this reason, it is possible to visually check all results by quickly going through the plots provided by the program. 
Without validation data the model cannot identify with 100% certainty the fishing location. 
"),
              hr(),
              h4("Model output data summary"),
              verbatimTextOutput("Assumptions"),
              hr()
              )
      )
  )
)
                              
server <- function(input, output, session) {
  
  # Filtering  
  fishing_filtered <- reactive({
    fishing_events %>% 
    filter(Date >= input$dateselector[1] &  Date <= input$dateselector[2])
  })
  
  fishing_filtered_v <- reactive({ 
    if (is.null(input$vesselselector)) {
      fishing_filtered() 
    } else {
      fishing_filtered() %>% 
    filter(ID_anon == input$vesselselector)
    }
  })

  fishing_filtered1 <- reactive({ 
    if (is.null(input$location)) {
        fishing_filtered_v()
        } else {
        fishing_filtered_v() %>% 
          filter(Area_Rect == input$location)
        }
    })
  fishing_filtered2 <- reactive({ 
    if (is.null(input$gridsquare)) {
      fishing_filtered1() 
    } else {
      fishing_filtered1() %>% 
        filter(InshoreGrid_SQ == input$gridsquare)
      }
  })
   
  observeEvent( 
    fishing_filtered_v(), {
    updateSelectInput(session, 
                      "location",
                      choices = c(levels(droplevels(as.factor(fishing_filtered1()$Area_Rect)))))
    }
  )
  
  observeEvent(
    input$location, { 
    updateSelectInput(session, 
                      "gridsquare",
                      choices = c(levels(droplevels(as.factor(fishing_filtered2()$InshoreGrid_SQ)))))
  }
  )

  ## Box1
  f_events <- reactive({
    unique(fishing_filtered2()$ID_trip_anon) 
  })
  f_event_1 <- reactive({
    length(f_events())
  })
  f_event_2 <- reactive({
    formatC(f_event_1(), digits = 1, format= "d",mode = "integer", big.mark = ",")
  })
  output$box1 <- renderValueBox({valueBox(value = f_event_2(), subtitle="Number of fishing events", icon = icon("fas fa-ship"))})#,
 
   ## Box2
  T_event <- reactive({
    (sum(fishing_filtered2()$Duration))/60
  })
  T_event_2 <- reactive({
    formatC(T_event(), digits = 5, format= "d",mode = "integer", big.mark = ",")
  })
  output$box2 <- renderValueBox({
    valueBox(value = T_event_2(),subtitle="Total fishing time (Hours)", icon = icon("fas fa-clock-o"))
    })
  
  ## Box3
  D_event <- reactive({
    (sum(fishing_filtered2()$Distance))/1000 
  })
  D_event_2 <- reactive({
    formatC(D_event(), digits = 8, format= "d",mode = "integer", big.mark = ",")
  })
  output$box3 <- renderValueBox({
    valueBox(value = D_event_2(),subtitle="Total fishing distance (Km)", icon = icon("fas fa-anchor"))
  })
  
  output$Assumptions <- renderPrint({
    summary(model_output) 
  })

  #plots
  output$day1 <- renderPlotly(
    fishing_filtered2() %>% 
      mutate(Day = as.Date(Date, format = "%Y-%m-%d")) %>% 
      mutate(DistanceKm = Distance/1000) %>% 
      group_by(Day,ID_anon) %>%
      summarise(Sum_dist = sum(DistanceKm)) %>%
      arrange(Day) %>%
      plot_ly(x = ~Day, y = ~Sum_dist, color = ~ID_anon, type="bar") %>% 
      layout(title = "Fishing Activity (Km) by Day",
             xaxis = list(title = "Date",type = 'date'),
             yaxis = list(title = "Fishing Activity (Kilometers)"),
             barmode = 'stack')
  )
  output$day2 <- renderPlotly( 
    fishing_filtered2() %>%
      mutate(Day = as.Date(Date, format = "%Y-%m-%d")) %>% 
      group_by(Day,ID_anon) %>%
      summarise(Sum_Dur = sum(Duration)/60) %>%
      arrange(Day) %>%
      plot_ly(x = ~Day, y = ~Sum_Dur, color = ~ID_anon,type="bar") %>% 
      layout(title = "Fishing Activity (Hours) by Day",
             xaxis = list(title = "Date", type = 'date'),
             yaxis = list(title = "Fishing Activity (Hours)"),
             barmode = 'stack')
  )
output$heat1 <- renderPlot(
  calendarHeat(dates = fishing_filtered2()$Date,
               values = fishing_filtered2()$Distance, 
               colors=c("#B5E384","#FFFFBD", "#FFAE63","#D61818"),
               ncolors = 99, 
               title = "Fishing Activity (Distance (km))")
    )

  fishing_filtered3 <- reactive({
    fishing_filtered2() %>% 
     mutate(Duration_hrs = Duration/60) 
  })
  
  output$heat2 <-renderPlot(
    calendarHeat(dates = fishing_filtered3()$Date,
                 values = fishing_filtered3()$Duration_hrs,
                 colors=c("#B5E384","#FFFFBD", "#FFAE63","#D61818"),
                 ncolors = 99, 
                 title = "Fishing Activity (Duration (hrs))")
  )

  spacing <- reactive({
    input$spacing
  })
  
  fishing_filtered4 <- reactive({
    fishing_filtered2() %>% 
    mutate(No_of_pots = Distance/spacing()) %>% 
    group_by(ID_trip_anon) %>%
    summarise(Sum_Pots = sum(No_of_pots)) 
  })
  
    col<-reactive({
      ifelse(fishing_filtered4()$Sum_Pots >= input$cutoff3,"#FF6666","#009933")
  })

  output$ev3 <- renderPlotly(
    plot_ly(fishing_filtered4(), x = ~ID_trip_anon, y = ~Sum_Pots, type = "bar", marker=list(color=col())) %>% 
      layout(title = "Estimated Number of Pots laid per Fishing Trip ",
             xaxis = list(title = "Fishing Trip ID (Trip ID number)"),
             yaxis = list(title = "Estimated Number of Pots"),
             showlegend = F)
  )
  
  
  output$vesselplot <-  renderPlotly(
    fishing_filtered() %>% 
      group_by(ID_anon) %>%
      summarise(Sum_Dur = (sum(Duration))/60) %>% 
      plot_ly(x = ~ID_anon, y = ~Sum_Dur, type="bar", color = ~ID_anon) %>% 
      layout(title = "Fishing Activity (Hours) by Vessel",
             xaxis = list(title = "Vessel ID"),
             yaxis = list(title = "Total Fishing Activity (Hours)"),
             showlegend = F,
             margin = list(t = 75))
  )
  output$vesselplot2<-  renderPlotly(
    fishing_filtered() %>% 
      mutate(DistanceKm = Distance/1000) %>% 
      group_by(ID_anon) %>%
      summarise(Sum_dist = sum(DistanceKm)) %>% 
      plot_ly(x = ~ID_anon, y = ~Sum_dist, type="bar", color = ~ID_anon) %>% 
      layout(title = "Fishing Activity (Km) by Vessel",
             xaxis = list(title = "Vessel ID"),
             yaxis = list(title = "Total Fishing Activity (Km)"),
             showlegend = F,
             margin = list(t = 75))
  )
 
  output$map <- renderLeaflet({
    leaflet(fishing_filtered()) %>% 
      setView(lng = -6.7, lat = 52.15, zoom =10 ) %>%
      addTiles(group = "Basemap") %>%
      addCircleMarkers(data=map_fishing, lat= ~latitude,lng = ~longitude, color = 'red',
                       radius = 5, stroke = TRUE, weight =1, fillOpacity =0.5, 
                       popup = ~paste(ID_anon,Date,sep = "<br />"),
                       group = "Fishing"
      ) %>%
      addCircleMarkers(data=map_mix_transit, lat= ~latitude,lng = ~longitude, color = 'yellow',
                       radius = 3, stroke = FALSE,weight =1, fillOpacity =0.5, 
                       popup = ~paste(ID_anon,Date,sep = "<br />"),
                       group = "Mix Transit"
      ) %>%
      addCircleMarkers(data=map_transit, lat= ~latitude,lng = ~longitude, color = 'green',
                       radius = 3, stroke = FALSE,weight =1, fillOpacity =0.2, 
                       popup = ~paste(ID_anon,Date,sep = "<br />"),
                       group = "Transit"
      ) %>%
      addPolygons(group = "InshoreGrid", data = inshore_grid, weight = 1, opacity = 0.3, fill=FALSE,
                  popup = ~paste(Label)) %>%
      addMarkers(data=landing_sites, lat= ~Latitude,lng = ~Longitude,
                 popup = ~paste(NAME,TYPE,sep = "<br />"),
                       group = "Landing Sites"
      ) %>%
      addLegend("bottomright", colors = c('red','yellow','green'), labels = c("Fishing","Mix Transit","Transit"), 
                title = "Activity type",opacity = 1) %>% 
      hideGroup("InshoreGrid") %>% 
      hideGroup("Landing Sites") %>% 
      addLayersControl(baseGroups = "Basemap", 
                       overlayGroups = c("Fishing","Mix Transit","Transit","InshoreGrid","Landing Sites"),
                       options = layersControlOptions(collapsed = FALSE))
  })


  
}

shinyApp(ui, server)