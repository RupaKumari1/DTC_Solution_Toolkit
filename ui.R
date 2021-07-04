# Shiny Home
# DTC Solution Toolkit - This analytical framework will facilitate in improving 
# i) Customer and driver partner  experience
# ii) Improved demand supply balance
# iii) Better revenue analysis and monitoring
# Authors: Rupa Kumari & Md Mehran Abul
# Date: July 5, 2021 

#ui.R

#===============================================================================
#                               SHINY USER INTERFACE                           #
#===============================================================================


# Installing required packages (No need to install if already done)
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("shinydashboard")
# install.packages("plotly")
# install.packages("DT")

#Load packages and modules
library(shiny)
library(leaflet)
library(shinydashboard)
library(plotly)
library(DT)

# Initiating UI dashboard
dashboardPage(skin = "purple",
              dashboardHeader(title = "DTC Solution Toolkit", titleWidth = NULL,
                              tags$li(class = "dropdown",style = "margin-top: 5px;"
                              )),
              
              # Menu Items
              dashboardSidebar(
                width = 180,
                sidebarMenu(
                  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                  menuItem("Performance", tabName = "performance", icon = icon("search-dollar", lib = "font-awesome")),
                  menuItem("Demand-Supply", tabName = "demand_Supply", icon = icon("balance-scale-right", lib = "font-awesome")),
                  menuItem("Revenue", tabName = "revenue", icon = icon("money-bill-wave", lib = "font-awesome")),
                  menuItem("Important Links", tabName = "Website",icon = icon("link"),
                           menuItem(tags$a(href = 'https://www.dubaitaxi.ae/' ,target="_blank",'Dubai Taxi Homepage')),
                           menuItem(tags$a(href = 'https://www.dubaitaxi.ae/en/Our-Services' ,target="_blank",'Our Services')),
                           menuItem(tags$a(href = 'https://www.dubaitaxi.ae/en/Contact-Us' ,target="_blank",'Contact Us'))
                  )
                )
              ),
              
              # Dashboard Body
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
                  # Dashboard tab items
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
                  
                  # Performance tab items
                  tabItem("performance",
                          tags$p(tags$b(style='font-size: 25px;color:#4A235A',"Driver Performance")),
                          
                          box(img(src='driver_1.png', align = "right", height= 110,width = 110), 
                              uiOutput("rank_text"),
                              width = 4),
                          
                          box(img(src='driver_1.png', align = "right", height= 110,width = 110),
                              uiOutput("income_text"),
                              width = 4),
                          
                          box(img(src='driver_2.png', align = "right", height= 110,width = 110), 
                              uiOutput("income_trip"),
                              width = 4),
                          dataTableOutput('table'),
                          tags$br(),
                          downloadButton("downloadData", "Download")
                  ),
                  # Demand_Supply tab items
                  tabItem("demand_Supply",
                          tags$p(tags$b(style='font-size: 25px;color:#4A235A',"Trip Recommendation")),
                          leafletOutput("mymap", height = "620"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE, draggable = FALSE, top = 110, left = "auto", right = 10, bottom = "auto",width = "200", height = "auto",
                                        tags$div(selectInput("select_date","Select Date",selected = select_date[1], multiple = FALSE,choices = select_date),id = 'select_option'),
                                        tags$div(selectInput("select_hour","Select Hour",selected = select_hour[1], multiple = FALSE,choices = select_hour),id = 'select_option')),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,draggable = FALSE, top = 110, left = 250, right = 10, bottom = "auto",width = "300", height = "auto",
                                        tags$div(uiOutput('map_text')),),
                          
                          conditionalPanel("false", icon("crosshair"))
                          
                  ),
                  # Revenue tab items
                  tabItem("revenue",
                          tags$p(tags$b(style='font-size: 25px;color:#4A235A',"Revenue Analysis")),
                          
                          box(tags$p(style='font-size: 20px;color:#1B4F72',"7 Day Moving Avg Revenue: "),
                              tags$p(style='font-size: 20px;color:#1B4F72',tags$b("AED ", seven_days_ui)),
                              tags$p(style='font-size: 20px;color:#1B4F72',"30 Day Moving Avg Revenue: "),
                              tags$p(style='font-size: 20px;color:#1B4F72',tags$b("AED ", seven_days_ui)),
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

