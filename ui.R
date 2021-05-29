
library(shiny)
library(leaflet)
library(shinydashboard)
library(plotly)
library(DT)

dashboardPage(skin = "purple",
  dashboardHeader(title = "DTC Solution Toolkit", titleWidth = NULL,
                  tags$li(class = "dropdown",style = "margin-top: 5px;"
                  )),
  
  dashboardSidebar(
    width = 180,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Demand-Supply", tabName = "Demand_Supply", icon = icon("glyphicon glyphicon-search", lib = "glyphicon")),
      menuItem("Performance", tabName = "performance", icon = icon("table")),
      menuItem("Revenue", tabName = "revenue", icon = icon("table")),
      menuItem("Important Links", tabName = "Website",icon = icon("link"),
               menuItem(tags$a(href = 'https://www.dubaitaxi.ae/' ,target="_blank",'Dubai Taxi Homepage')),
               menuItem(tags$a(href = 'https://www.dubaitaxi.ae/en/Our-Services' ,target="_blank",'Our Services')),
               menuItem(tags$a(href = 'https://www.dubaitaxi.ae/en/Contact-Us' ,target="_blank",'Contact Us'))
      )
    )
  ),
  

  dashboardBody(
    
    
      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 16px;
      }
    '))
    ),
    tabItems(
      tabItem("dashboard",

              column(tags$p(tags$b(style='font-size: 25px;color:#4A235A',"Company Performance")),width = 10),
              fluidRow(
                valueBoxOutput("rate"),
                valueBoxOutput("count"),
                valueBoxOutput("users")
              ),
              
              
              column(4,plotlyOutput("google",height = 350)),
              column(4,plotlyOutput("playstore",height = 350)),
              column(4,plotlyOutput("apple",height = 350)),
           
      ),
      tabItem("Demand_Supply",
              tags$p(tags$b(style='font-size: 25px;color:#4A235A',"Trip Recommendation")),
              
              leafletOutput("mymap", height = "620"),
              #add location name
              absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                            draggable = FALSE, top = 110, left = "auto", right = 10, bottom = "auto",
                            width = "200", height = "auto",
                            
                            
                tags$div(selectInput("driver_id","Select Driver ID",selected = driver_id[1], multiple = FALSE,choices = driver_id),id = 'select_option'),
              
              
              ),
              
              absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                            draggable = FALSE, top = 110, left = 250, right = 10, bottom = "auto",
                            width = "250", height = "auto",
                            
                            
                            tags$div(uiOutput('map_text')),
                            
              ),
              
              conditionalPanel("false", icon("crosshair"))
              
      ),
      tabItem("performance",
              tags$p(tags$b(style='font-size: 25px;color:#4A235A',"Driver Performance")),
              
              box(img(src='driver_3.png', align = "right", height= 110,width = 110), 
                  tags$p(style='font-size: 16px;color:#a10000',tags$b("Top Driver based on Rank")),
                  tags$p("Driver Name: ",tags$b("Arnold")),
                  tags$p("Driver ID: ",tags$b("950292")),
                  tags$p("Rank: ",tags$b("1")),
                  tags$p("Total Income: ",tags$b("AED 314")),
                  tags$p("Average Rating: ",tags$b("4")),
                  
                  width = 4),
              
              box(img(src='driver_2.png', align = "right", height= 110,width = 110),
                  tags$p(style='font-size: 16px;color:#a10000',tags$b("Top Driver based on Income")),
                  tags$p("Driver Name:",tags$b(" Omar")),
                  tags$p("Driver ID:",tags$b(" 950186")),
                  tags$p("Rank: ",tags$b("2")),
                  tags$p("Total Income: ",tags$b("AED 852")),
                  tags$p("Average Rating: ",tags$b("3.4")),
                  
                  width = 4),
              
              box(img(src='driver_3.png', align = "right", height= 110,width = 110), 
                  tags$p(style='font-size: 16px;color:#a10000',tags$b("Top Driver based on Customer Rating")),
                  tags$p("Driver Name: ",tags$b("Arnold")),
                  tags$p("Driver ID: ",tags$b("950292")),
                  tags$p("Rank: ",tags$b("1")),
                  tags$p("Total Income: ",tags$b("AED 314")),
                  tags$p("Average Rating: ",tags$b("4")),
                  
                  width = 4),
              dataTableOutput('table')
      ),
      tabItem("revenue",
              tags$p(tags$b(style='font-size: 25px;color:#4A235A',"Revenue Analysis")),
                  
             box(tags$p(style='font-size: 20px;color:#1B4F72',"7 day moving Avg Revenue: "),
                tags$p(style='font-size: 20px;color:#1B4F72',tags$b("AED ", round(raw_data$sevenma[length(na.omit(raw_data$sevenma))], digits = 0))),
                  
                tags$p(style='font-size: 20px;color:#1B4F72',"30 day moving Avg Revenue: "),
                tags$p(style='font-size: 20px;color:#1B4F72',tags$b("AED ", round(raw_data$thirtyma[length(na.omit(raw_data$thirtyma))], digits = 0))),
                
                height = 197, width = 3),

              
              box(plotlyOutput("ma_bar", height = 175), width = 6),
              box(plotlyOutput("donut", height = 175, width = "90%"), width = 3),
              
              box(plotlyOutput("line_revenue", height = 170), width = 12),
              
             box(plotlyOutput("line_error", height = 170), width = 12),
             
              conditionalPanel("false", icon("crosshair"))
              
      )
    )
  )
)

