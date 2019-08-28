shinyServer(function(input, output, session) {
  
  temp_data <- reactive({
    gpw %>%
      filter(Region %in% input$region_data)
  })  
  
  observe({
    now <- as.character(input$country_data)
    updatePickerInput(session,
                         "country_data", "Select Countries (from dropdown list)",
                         choices = c(unique(as.character(temp_data()$Country))),
                         selected = input$country_data)
  })
  
  dataTable <- reactive({
    if ("All" %in% input$country_data) {
      temp_data() %>%
        filter(Year >= input$year_data[1] & Year <= input$year_data[2]) %>%
        select(Country, Region, Year, input$indicators_data)
    } else {
      temp_data() %>%
        filter(Country %in% input$country_data) %>%
        filter(Year >= input$year_data[1] & Year <= input$year_data[2]) %>%
        select(Country, Region, Year, input$indicators_data)
    }
  })  
  
  output$database <- DT::renderDataTable({
    datatable(
      dataTable(),
      options = list(pageLength = 20, lengthMenu = c(10, 20, 25, 50, 100))
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("gpw", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataTable(), file, row.names = FALSE)
    }
  )
  
  output$indicatorMetadata <- renderUI({
    req(input$indicator_meta)
    index <- match(input$indicator_meta, gpw_choices_full)
    indic <- as.character(gpw_colnames[index])
    h2(indic)
  })
  
  output$indicator_name <- renderUI({
    req(input$indicator_meta)
    HTML(paste(h3("Indicator"), "\n\n",
               tags$p(ind_label(input$indicator_meta))))
  })
  
  output$indicator_definition <- renderUI({
    req(input$indicator_meta)
    HTML(paste(h3("Definition"), "\n\n",
         tags$p(ind_def(input$indicator_meta))))
  })
  
  output$indicator_numerator <- renderUI({
    req(input$indicator_meta)
    HTML(paste(h3("Numerator"), "\n\n",
         tags$p(ind_num(input$indicator_meta))))
  })
  
  output$indicator_denominator <- renderUI({
    req(input$indicator_meta)
    HTML(paste(h3("Denominator"), "\n\n",
         tags$p(ind_den(input$indicator_meta))))
  })
  
  output$indicator_methods <- renderUI({
    req(input$indicator_meta)
    HTML(paste(h3("Methodology"), "\n\n",
         tags$p(ind_meth(input$indicator_meta))))
  })
  
  output$indicator_limitations <- renderUI({
    req(input$indicator_meta)
    HTML(paste(h3("Limitations"), "\n\n",
         tags$p(ind_lim(input$indicator_meta))))
  })
  
  output$indicator_data_source <- renderUI({
    req(input$indicator_meta)
    HTML(paste(h3("Data Sources"), "\n\n",
         tags$p(ind_source(input$indicator_meta))))
  })
  
  output$indicator_data_avail <- renderUI({
    req(input$indicator_meta)
    HTML(paste(h3("Data Availability"), "\n\n",
         tags$p(ind_avail(input$indicator_meta))))
  })
  
})
