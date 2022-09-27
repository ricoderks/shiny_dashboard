library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "SODA - dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Files", 
               tabName = "Files", 
               icon = icon("file-excel")),
      menuItem(text = "Lipidomics", 
               tabName = "Lipidomics", 
               icon = icon("magnifying-glass-chart")),
      menuItem(text = "Proteomics", 
               tabName = "Proteomics", 
               icon = icon("magnifying-glass-chart"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "Files",
        h2("Files"),
        p("Load files here")
      ), # end files
      
      tabItem(
        tabName = "Lipidomics",
        fluidRow(
          checkboxGroupButtons(inputId = "showPlots",
                               label = "",
                               choices = c("Histogram" = "histogram",
                                           "Controls" = "histogram_control"),
                               selected = "histogram")
        ),
        fluidRow(
          box(
            id = "histogram",
            title = "Histogram",
            solidHeader = TRUE,
            status = "primary",
            plotOutput(outputId = "plot1", 
                       height = 250)
          ),
          
          box(
            id = "histogram_control",
            title = "Controls",
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            sliderInput(inputId = "slider", 
                        label = "Number of observations:", 
                        min = 1, 
                        max = 100, 
                        value = 50)
          )
        )
      ), # end lipidomics
      
      tabItem(
        tabName = "Proteomics",
        fluidRow(
          box(
            title = "Scatter plot",
            solidHeader = TRUE,
            plotOutput(outputId = "plot2", 
                       height = 250)
          ),
        )
      ) # end proteomics
      
    ) # end tabitems
  )
)

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  observe({
    shinyjs::toggle(
      id = "histogram",
      condition = "histogram" %in% input$showPlots
    )
    
    shinyjs::toggle(
      id = "histogram_control",
      condition = "histogram_control" %in% input$showPlots
    )
  })
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$plot2 <- renderPlot({
    plot(x = 1:length(histdata), 
         y = histdata)
  })
}

shinyApp(ui = ui, 
         server = server,
         options = list("launch.browser" = TRUE))