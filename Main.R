##################################################### Install libs (run only once)
#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("corrplot") # for correlation plots
#install.packages("ggcorrplot")

##################################################### Load necessary libraries
library(shiny)
library(ggplot2)
library(corrplot)
library(ggcorrplot)

#####################################################  Define UI
ui <- fluidPage(
  titlePanel("ChemPlot"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      width = 2,  # Set the width to 2 (20%)
      fileInput("file", "Choose an Excel file"),
      selectInput("x_axis", "X Axis", ""),
      selectInput("y_axis", "Y Axis", ""),
      actionButton("plot_btn", "Plot"),
      actionButton("corrplot_btn", "Corrplot"),
      downloadButton("download_plot", "Download Plot")
    ),
    
    mainPanel(
      width = 10,  # Set the width to 10 (80%)
      plotOutput("main_plot", width = "100%")  # Set the width of the plotOutput to 100%
    )
  )
)

#####################################################  Define server
server <- function(input, output, session) {
  
  # Reactive values
  data <- reactiveValues(
    df = NULL,
    plot = NULL
  )
  
  observeEvent(input$file, {
    data$df <- readxl::read_excel(input$file$datapath, sheet = "simple_ppm")
    updateSelectInput(session, "x_axis", choices = colnames(data$df))
    updateSelectInput(session, "y_axis", choices = colnames(data$df))
  })
  
  observeEvent(input$plot_btn, {
    if (!is.null(data$df)) {
      data$plot <- ggplot(data$df, aes_string(x = input$x_axis, y = input$y_axis)) +
        geom_point() +
        labs(title = "Scatter Plot")
      output$main_plot <- renderPlot({
        print(data$plot)
      })
    }
  })
  
  observeEvent(input$corrplot_btn, {
    if (!is.null(data$df)) {
      # Exclude the first three columns
      corr <- cor(data$df[, -c(1:3)], use = "complete.obs")
      
      data$plot <- ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = FALSE,
                              outline.col = "white",
                              ggtheme = ggplot2::theme_gray,
                              colors = c("#6D9EC1", "white", "#E46726"))
      
      output$main_plot <- renderPlot({
        print(data$plot)
      })
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      if (!is.null(data$plot)) {
        ggsave(file, data$plot, height = 10, width = 10, units = "in")  # Adjust height and width as needed
      }
    }
  )

}

##################################################### Run the application
shinyApp(ui, server)


