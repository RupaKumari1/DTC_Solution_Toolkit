library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(leaflet.extras)

function(input, output, session) {
  
  
  output$mymap <- renderLeaflet({
    leaflet("mymap", data = selected_data) %>%
      addTiles() %>%
      addHeatmap(lng=selected_data$Longitude,lat=selected_data$Latitude,intensity=selected_data$Footfall_Count,max=20,radius=2,blur=1)
  })
  
  
  observeEvent(input$driver_id,{
    if(!is.na(input$driver_id)){
      driver_id <- input$driver_id
      selected_data <- footfall_data[which(footfall_data$DID == driver_id),]
      
      output$mymap <- renderLeaflet({
        leaflet("mymap", data = selected_data) %>%
          addTiles() %>%
          addHeatmap(lng=selected_data$Longitude,
                     lat=selected_data$Latitude,
                     intensity=selected_data$Footfall_Count,
                     layerId = selected_data$SeqNumber,
                     max=20,radius=20,blur=20
                     )%>% 
          addMarkers(lng=selected_data$Longitude,
                     lat=selected_data$Latitude,
                     popup = paste0("Area Number : ",selected_data$Area, ", Passenger Count : ",selected_data$Footfall_Count)
                     )%>% 
          addCircleMarkers(lng=unique(selected_data$D_Longitude),
                     lat=unique(selected_data$D_Latitude),
                     radius = 5,
                     color = 'blue',
                     fillOpacity = 0.9,
                     popup = paste0("Area Number : ",unique(selected_data$D_Area))
                     )%>% 
          addCircleMarkers(lng=unique(selected_data$D_Longitude),
                           lat=unique(selected_data$D_Latitude),
                           radius = 10,
                           color = 'red',
                           fillOpacity = 0.1,
                           popup = paste0("Area Number : ",unique(selected_data$D_Area))
          )
      })
      
      
      output$map_text=renderUI({
        tagList(
          p(style='font-size: 15px;color:#4A235A',HTML(paste0("Current Time: ", tags$b(Sys.time())))),
          p(style='font-size: 15px;color:#4A235A',HTML(paste0("Driver Name: ", tags$b(unique(selected_data$Name))))),
          p(style='font-size: 15px;color:#4A235A',HTML(paste0("Rating: ", tags$b(unique(selected_data$Rating))))),
          p(style='font-size: 15px;color:#4A235A',HTML(paste0("Total Trip: ", tags$b(unique(selected_data$Total_trip))))),
          p(style='font-size: 15px;color:#4A235A',HTML(paste0("Driver Area Number: ",tags$b(unique(selected_data$D_Area)))))
        )
      })
      
      
    }
  })
  

  output$rate <- renderValueBox({
    valueBox(
      16, "Total Trip", icon = icon("glyphicon glyphicon-map-marker", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$count <- renderValueBox({
    valueBox(
      "1701", "Total Income", icon = icon("money-bill-wave", lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  output$users <- renderValueBox({
    valueBox(
      "113", "Revenue Per Trip", icon = icon("coins", lib = "font-awesome"),
      color = "light-blue"
    )
  })
  
  output$playstore <- renderPlotly({
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = 3.7,
      title = list(text = "Playstore Rating", font = list(size = 24)),
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
  
  output$google <- renderPlotly({
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = 3.8,
      title = list(text = "Google Rating", font = list(size = 24)),
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
  
  output$apple <- renderPlotly({
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = 3.7,
      title = list(text = "Apple Store Rating", font = list(size = 24)),
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
  
  Driver_Performance_Data <- get_driver_performance_data()
 
  output$table <- DT::renderDataTable({data = Driver_Performance_Data
  datatable(Driver_Performance_Data,rownames = FALSE, selection='single',options = list(
    columnDefs = list(list(className = 'dt-left', targets = '_all')),
    pageLength = 5,
    lengthChange = FALSE, scrollX = TRUE
  )
  )
  
  })
  
  
  output$line_revenue <- renderPlotly({
    fig <- plot_ly(data = final_line_df, x = final_line_df$month, y = final_line_df$Predicted.Revenue,name = "Predicted", type = 'scatter', mode = 'lines')
    
    fig <- fig %>% layout(title = "Predicted Revenue",
                          xaxis = list(title = "Months"),
                          yaxis = list (title = "Predicted Revenue"))
    fig <- fig %>% add_trace(y = final_line_df$Actual.Revenue, name = 'Actual', mode = 'lines+markers')
    
    fig
  })
  
  
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
    fig <- fig %>% layout(title = "3,17,946 AED Above May-21 Target",  
                        #  font = list(family = "sans serif",size = 10, color = 'black'),
                          showlegend = F,
                          font=list(size = 10, color = "black"),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig <- layout(fig, annotations=list(text="103% Target Met", "showarrow"=F))
    
    fig
  })
  
  output$line_error <- renderPlotly({
    fig <- plot_ly(data = final_line_df, x = final_line_df$month, y = final_line_df$Error,name = "Error", type = 'scatter', mode = 'lines')
    
    fig <- fig %>% layout(title = "Model Error",
                          xaxis = list(title = "Months"),
                          yaxis = list (title = "Error"))
    
    fig
  })
  
  
  output$ma_bar <- renderPlotly({
    len = length(na.omit(raw_data$thirtyma))
    df = raw_data[(len-14):len,]
    
    fig <- plot_ly(data = df, x = df$f_date, y = df$thirtyma, type = 'bar',
                   name = "30 day MA",  marker = list(color = 'rgb(49,130,189)'))
    
    fig <- fig %>% add_trace(y = df$sevenma, name = "7 day MA", 
                             marker = list(color = 'rgb(204,204,204)'))
    
    fig <- fig %>% layout(title = "14 days Moving Averages",
                          xaxis = list(title = ""),
                          yaxis = list(title = "Revenue(AED)"))
    
    fig
  })
  
  
}

