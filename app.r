library(shiny)
library(tidyverse)

ui <- fluidPage(
  numericInput('gridSize', 'Grid Size', value = 50, step = 1),
  plotOutput('grid', click = 'grid_click', height = 900),
  actionButton('clear', 'Clear Grid'),
  textInput('patternName', 'Pattern Name:'),
  # downloadButton("downloadData", "Download Pattern")
  actionButton("downloadData", "Download Pattern"),
  verbatimTextOutput('test')
)

server <- function(input, output, session) {
  tableVals <- reactiveValues(
    data = tibble(
      row = numeric(),
      col = numeric()
    )
  )
  
  observeEvent(input$grid_click, {
    newLine <- tibble(
      row = round(input$grid_click$y),
      col = round(input$grid_click$x),
    )
    
    if(paste0(round(input$grid_click$y),'-',round(input$grid_click$x)) %in% pull(mutate(tableVals$data, pst = paste0(row,'-',col)), pst)){
      tableVals$data <- mutate(tableVals$data, pst = paste0(row,'-',col)) %>% 
        filter(pst != paste0(input$grid_click$y,'-',input$grid_click$x)) %>% 
        select(-pst)
    } else {
      tableVals$data <- rbind(tableVals$data, newLine) 
    }
  })
  
  observeEvent(input$clear, {
    tableVals$data <- tibble(
      row = numeric(),
      col = numeric()
    )
  })
  
  grid_data <- reactive({
    req(input$gridSize)
    
    expand_grid(
      row = 1:input$gridSize,
      col = 1:input$gridSize
    )
  })
  
  grid_display <- function(){
    ggplot() + 
      geom_tile(data = grid_data(), aes(col, row), fill = 'white', colour = 'grey') + 
      theme_void() + 
      coord_equal() + 
      geom_tile(data = tableVals$data, aes(col, row), fill = 'black', colour = 'lightgrey', width = 1, height = 1) +
      scale_y_reverse()
  }
  
  output$grid <- renderPlot({
    grid_display()
  })
  
  downloadable_data <- reactive({
    tableVals$data %>% 
      mutate(
        col = col - round(input$gridSize/2),
        row = row - round(input$gridSize/2)
      )
  })
  
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste0('patterns/',input$patternName, ".csv")
  #   },
  #   content = function(file) {
  #     write.csv(downloadable_data(), file, row.names = FALSE)
  #   }
  # )
  output$test <- renderText({
    req(input$patternName)
    
    input$patternName %in% str_remove(list.files('patterns'), '\\.csv')
  })
  
  observeEvent(input$downloadData, {
    write.csv(downloadable_data(),  paste0('patterns/',input$patternName, ".csv"), row.names = FALSE)
  })

}

shinyApp(ui, server)