# Shiny Server
# DTC Solution Toolkit - This analytical framework will facilitate in improving 
# i) Customer and driver partner  experience
# ii) Improved demand supply balance
# iii) Better revenue analysis and monitoring
# Authors: Rupa Kumari & Md Mehran Abul
# Date: July 5, 2021 

#server.R

#===============================================================================
#                               SHINYSERVER                                    #
#===============================================================================


# Installing required packages (No need to install if already done)
# install.packages("leaflet")
# install.packages("DT")
# install.packages("plotly")
# install.packages("dplyr")
# install.packages("leaflet.extras")
# install.packages("rvest")


#Load packages and modules 
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(leaflet.extras)
library(rvest)

# Server function
function(input, output, session) {
  
  # Value box of company performance
  output$rate <- renderValueBox({
    valueBox(
      total_trip_ui, paste0("Total Trip from ",min_month, " to ",max_month), icon = icon("glyphicon glyphicon-map-marker", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  # Value box of company performance
  output$count <- renderValueBox({
    valueBox(
      paste0(total_income, ' AED(in Millions)'), paste0("Total Income from ",min_month, " to ",max_month), icon = icon("money-bill-wave", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  # Value box of company performance
  output$users <- renderValueBox({
    valueBox(
      paste0(rev_per_trip, ' AED'), paste0("Revenue Per Trip from ",min_month, " to ",max_month), icon = icon("coins", lib = "font-awesome"),
      color = "light-blue"
    )
  })
  
  # Gauge chart of DTC rating from trip advisor
  output$google <- renderPlotly({
    url <- 'https://www.tripadvisor.com/ShowUserReviews-g295424-d7708943-r757284301-Dubai_Taxi-Dubai_Emirate_of_Dubai.html'
    webpage <- read_html(url)
    rating <- webpage %>%
      html_nodes("div.ratingContainer") %>%
      html_text()

    rating_v <-substr(rating, 1,3)
    rating_int <-as.numeric(gsub(",", ".", rating_v))
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = rating_int,
      title = list(text = "Trip Advisor Rating", font = list(size = 24)),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(0, 5), tickwidth = 1, tickcolor = "darkblue"),
        bar = list(color = "blue"),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray"
      )
    ) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30), paper_bgcolor='#F5B7B1')
    
    fig
  })
  
  # Gauge chart of DTC rating from play store
  output$playstore <- renderPlotly({
    url <- 'https://play.google.com/store/apps/details?id=com.dtc.dtccustomer&hl=en&gl=US'
    webpage <- read_html(url)
    rating <- webpage %>%
      html_nodes("div.BHMmbe") %>%
      html_text()
    
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = rating,
      title = list(text = "Play Store Rating", font = list(size = 24)),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(0, 5), tickwidth = 1, tickcolor = "darkblue"),
        bar = list(color = "blue"),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray"
      )
    ) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30),  paper_bgcolor='#EDBB99')
    
    fig
  })
  
  # Gauge chart of DTC rating from app store
  output$apple <- renderPlotly({
    url <- 'https://apps.apple.com/in/app/dtc/id1453313475'
    webpage <- read_html(url)
    rating <- webpage %>%
      html_nodes(".we-customer-ratings__averages__display") %>%
      html_text()
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = rating,
      title = list(text = "App Store Rating", font = list(size = 24)),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(0, 5), tickwidth = 1, tickcolor = "darkblue"),
        bar = list(color = "blue"),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray"
      )
    ) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30), paper_bgcolor='#A2D9CE')
    
    fig
  })
  
  # Top Driver details based on rank
  output$rank_text <- renderUI({
    tagList(
      tags$p(style='font-size: 16px;color:#a10000',tags$b("Top Driver based on Rank")),
      tags$p("Driver ID: ",tags$b(Driver_Scorecard[1,]$DID)),
      tags$p("Trips: ",tags$b(Driver_Scorecard[1,]$ShiftSeqNumber)),
      tags$p("Rank: ",tags$b(Driver_Scorecard[1,]$Rank)),
      tags$p("Total Income: AED ",tags$b(round(Driver_Scorecard[1,]$Total_Income,2))),
      tags$p("Income per 1K distance ",tags$b(round(Driver_Scorecard[1,]$Income_per_1Kdis,2))),
    )
  })
  
  # Top Driver details based on total income
  output$income_text <- renderUI({
    Driver_Scorecard <- Driver_Scorecard[order(Driver_Scorecard$Total_Income, decreasing = TRUE),]  
    tagList(
      tags$p(style='font-size: 16px;color:#a10000',tags$b("Top Driver based on Total Income")),
      tags$p("Driver ID: ",tags$b(Driver_Scorecard[1,]$DID)),
      tags$p("Trips: ",tags$b(Driver_Scorecard[1,]$ShiftSeqNumber)),
      tags$p("Rank: ",tags$b(Driver_Scorecard[1,]$Rank)),
      tags$p("Total Income: AED ",tags$b(round(Driver_Scorecard[1,]$Total_Income,2))),
      tags$p("Income per 1K distance ",tags$b(round(Driver_Scorecard[1,]$Income_per_1Kdis,2))),
    )
  })
  
  # Top Driver details based on total trip
  output$income_trip <- renderUI({
    Driver_Scorecard <- Driver_Scorecard[order(Driver_Scorecard$ShiftSeqNumber, decreasing = TRUE),]  
    tagList(
      tags$p(style='font-size: 16px;color:#a10000',tags$b("Top Driver based on Total Trip")),
      tags$p("Driver ID: ",tags$b(Driver_Scorecard[1,]$DID)),
      tags$p("Trips: ",tags$b(Driver_Scorecard[1,]$ShiftSeqNumber)),
      tags$p("Rank: ",tags$b(Driver_Scorecard[1,]$Rank)),
      tags$p("Total Income: AED ",tags$b(round(Driver_Scorecard[1,]$Total_Income,2))),
      tags$p("Income per 1K distance ",tags$b(round(Driver_Scorecard[1,]$Income_per_1Kdis,2))),
    )
  })
  
  # Driver performance table
  names(table_data) <- c('Driver ID', 'Total Trips', 'Total Income(AED)', 'Total Distance', 'Income per 1K Distance', 'Rank') 
  output$table <- DT::renderDataTable({data = table_data
  datatable(table_data,rownames = FALSE, selection='single',options = list(
    columnDefs = list(list(className = 'dt-left', targets = '_all')), pageLength = 5,lengthChange = FALSE, scrollX = TRUE))
  })
  
  #Download driver performance in csv file
  output$downloadData <- downloadHandler(
    filename = paste0("Loan_Pred.csv"),
    content = function(filename) {
      write.csv(table_data, filename,row.names=FALSE)
    }
  ) 
  
  #Display geographical heat map as per date and hour selection
  observeEvent(list(input$select_date, input$select_hour),{
    if(!is.na(input$select_date) && !is.na(input$select_hour)){
      hour <- input$select_hour
      date <- input$select_date

      selected_date_data <- Trip_Recommendation[which(Trip_Recommendation$Date == date),]
      selected_data <- selected_date_data[which(selected_date_data$Hour == hour),]
     
      output$mymap <- renderLeaflet({
        leaflet("mymap", data = selected_data) %>%
          addTiles() %>%
          addHeatmap(lng=selected_data$Longitude,
                     lat=selected_data$Latitude,
                     intensity=selected_data$Trip_forecast,
                     max=20,radius=20,blur=20
          )%>%
          addMarkers(lng=selected_data$Longitude,
                     lat=selected_data$Latitude,
                     label = paste0("Area Number : ",selected_data$Area, ", Expected Trip Count : ",round(selected_data$Trip_forecast), 0)
          )
      })
      
      temp_hour <- if ((as.numeric(hour) +1) == 24) 0 else as.numeric(hour) +1

      next_hour_data = selected_date_data[which(selected_date_data$Hour == temp_hour),]
      next_hour_data <- next_hour_data[order(next_hour_data$Trip_forecast, decreasing = TRUE),]

      #Display map panel with data forcasted trip area for next one hour slot as per date and hour selection
      output$map_text=renderUI({
        tagList(
          tags$p(style='font-size: 16px;color:#a10000',tags$b("Forcasted trip area for next one hour slot")),
          p(style='font-size: 15px;color:#4A235A',HTML(paste0("Expected Average Trip: ", tags$b(round(next_hour_data[1,]$Trip_forecast,0)),
                                                              " in Area ",next_hour_data[1,]$Area))),

          p(style='font-size: 15px;color:#4A235A',HTML(paste0("Expected Average Trip: ", tags$b(round(next_hour_data[2,]$Trip_forecast,0)),
                                                              " in Area ",next_hour_data[2,]$Area))),

          p(style='font-size: 15px;color:#4A235A',HTML(paste0("Expected Average Trip: ", tags$b(round(next_hour_data[3,]$Trip_forecast,0)),
                                                              " in Area ",next_hour_data[3,]$Area))),

          p(style='font-size: 15px;color:#4A235A',HTML(paste0("Expected Average Trip: ", tags$b(round(next_hour_data[4,]$Trip_forecast,0)),
                                                              " in Area ",next_hour_data[4,]$Area))),

        )
      })
    }
  })
  
  #Line chart of predicted revenue along with actual revenue
  output$line_revenue <- renderPlotly({
    fig <- plot_ly(data = Revenue_Data, x = Revenue_Data$f_date, y = Revenue_Data$Forecast_Income, name = "Predicted", type = 'scatter', mode = 'lines')
    fig <- fig %>% layout(title = "Forecasted Revenue", xaxis = list(title = "Months"),yaxis = list (title = "Predicted Revenue"))
    fig <- fig %>% add_trace(y = Revenue_Data$Total_Income, name = 'Actual', mode = 'lines+markers')
    fig
  })
  
  #Line chart of predicted revenue along with actual revenue
  output$donut <- renderPlotly({
    ratings <- c(0)
    count <- c(100)
    colors <- c('green')
    df <- data.frame(ratings, count)
    
    fig <- df %>% plot_ly(labels = ~ratings, values = ~count, 
                          marker = list(colors = ~colors, line = list(color = '#FFFFFF', width = 2.5)),
                          textinfo = "none", hoverinfo = "none"
    )
    fig <- fig %>% add_pie(hole = 0.8)
    fig <- fig %>% layout(title = donut_title,  
                          #  font = list(family = "sans serif",size = 10, color = 'black'),
                          showlegend = F,font=list(size = 10, color = "black"),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig <- layout(fig, annotations=list(text=donut_text, "showarrow"=F))
    
    fig
  })
  
  #Line chart of model error
  output$line_error <- renderPlotly({
    fig <- plot_ly(data = Revenue_Data, x = Revenue_Data$f_date, y = Revenue_Data$Error ,name = "Error", type = 'scatter', mode = 'lines')
    fig <- fig %>% layout(title = "Model Performance", xaxis = list(title = "Months"),yaxis = list (title = "Error(%)"))
    fig
  })
  
  #Bar chart of 30 day moving averages along with 14 day moving averages 
  output$ma_bar <- renderPlotly({
    len = length(na.omit(Revenue_Data$Thirtyma))
    df = Revenue_Data[(len-14):len,]
    fig <- plot_ly(data = df, x = df$f_date, y = df$Thirtyma, type = 'bar',name = "30 day MA",  marker = list(color = 'rgb(49,130,189)'))
    fig <- fig %>% add_trace(y = df$Sevenma, name = "7 day MA",marker = list(color = 'rgb(204,204,204)'))
    fig <- fig %>% layout(title = "Moving Averages",xaxis = list(title = ""),yaxis = list(title = "Revenue(AED)"))
    fig
  })
}

