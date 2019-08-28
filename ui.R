shinyUI(
  navbarPage(
  "GPW 13 Programmatic Indicators",
  tabPanel("Database",
  sidebarLayout(
    sidebarPanel(
      HTML(paste(span("Data compiled from ", style="font-weight: bold;"), 
                 a(href="https://unstats.un.org/sdgs/indicators/database", "UNSD SDG Database"), 
                 " and ", 
                 a(href="https://www.who.int/gho/en/", "WHO Global Health Observatory"), " 2019")), br(), br(),
      checkboxGroupInput("region_data", "Select Regions",
                         choices = levels(gpw$Region),
                         selected = levels(gpw$Region)),
      pickerInput("country_data", "Select Countries (from dropdown list)",
                     choices = c(levels(gpw$Country)),
                     selected = sample(levels(gpw$Country), 15),
                     multiple = TRUE,
                     options = list(title = "Select Countries",
                                    size = 15,
                                    `actions-box` = TRUE)), br(),
      sliderInput("year_data", "Select Year Range",
                  min = 2000, max = 2018,
                  value = c(2013, 2016),
                  sep = ""),
      prettySwitch("latest", "Latest Available Data",
                   status = "success", fill = TRUE), br(),
      pickerInput("indicators_data", "Select Indicators (from dropdown list)",
                     choices = gpw_colnames,
                     multiple = TRUE,
                     selected = sample(gpw_colnames, 10),
                     options = list(title = "Select Indicators",
                                    `actions-box` = TRUE,
                                     size = 15)
                  ), 
      downloadButton("downloadData", "Download Data"),
      width = 3
    ),
    mainPanel(
      DT::dataTableOutput("database")
    )
  )
  ),
  tabPanel("Metadata",
    sidebarLayout(
      sidebarPanel(
        HTML(paste(span("Source: ", style="font-weight: bold;"), a(href="http://bit.ly/gpw13-metadata", "WHO GPW13 Programmatic Indicators Metadata 2019"))), br(), br(),
        selectInput("indicator_meta", "Select Indicator",
                    choices = gpw_choices_full,
                    selected = "ihr"),
        width = 3
      ),
      mainPanel(
        htmlOutput("indicatorMetadata"), br(),
        fluidRow(
          column(6, htmlOutput("indicator_name")),
          column(6, htmlOutput("indicator_definition"))
        ), hr(),
        fluidRow(
          column(6, htmlOutput("indicator_numerator")),
          column(6, htmlOutput("indicator_denominator"))
        ), hr(),
        fluidRow(
          column(6, htmlOutput("indicator_methods")),
          column(6, htmlOutput("indicator_limitations"))
        ), hr(),
        fluidRow(
          column(6, htmlOutput("indicator_data_source")),
          column(6, htmlOutput("indicator_data_avail"))
        )
      )
    )
  )
))
