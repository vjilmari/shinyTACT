library(shiny)
library(jsonlite)
library(dplyr)

source("TACT_fin.R")
source("TACT_languages.R")

# import languages
translations <- list(
  fi = fromJSON("fi.json"),
  en = fromJSON("en.json")
)

ui <- fluidPage(
  selectInput("language", "Kieli/Language:", choices = c("fi", "en"), selected="fi"),
  uiOutput("title_panel"),
  tags$style(HTML("
    .sidebar {
      position: sticky;
      top: 0;
      height: calc(100vh - 70px); /* Set the height based on your app's layout */
      overflow-y: auto;
    }
    #tactPlot {
      max-width: 100%;
      width: auto;
      height: auto;
      padding-bottom: 100%;
      position: relative;
    }
    #tactPlot > .plot-container {
      position: absolute;
      width: 100%;
      height: 100%;
    }
  ")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("r", textOutput("slider_label"), min = -1, max = 1, value = 0, step = 0.01),
      textInput("x_label", textOutput("x_label_text"), value = "X"),
      textInput("y_label", textOutput("y_label_text"), value = "Y"),
      sliderInput("font_size", textOutput("font_size_text"), min = 0.1, max = 3, value = 1, step = 0.1),
      tags$p(textOutput("tact_info_text"),
             tags$br(),
             tags$a("Mõttus, R. (2022). What Correlations Mean for Individual People: A Tutorial for Researchers, Students and the Public. Personality Science, 3, 1–27. https://doi.org/10.5964/ps.7467",
                    href = "https://doi.org/10.5964/ps.7467",
                    target = "_blank")
      ),
      class = "sidebar"
    ),
    mainPanel(
      plotOutput("tactPlot")
    )
  )
)

server <- function(input, output, session) {
  selected_language <- reactive({
    input$language
  })
  
  output$title_panel <- renderUI({
    HTML(paste0("<h1>", translations[[selected_language()]]$title, "</h1>"))
  })
  
  output$slider_label <- renderText({
    translations[[selected_language()]]$r_slider
  })
  
  output$x_label_text <- renderText({
    translations[[selected_language()]]$name_x
  })
  
  output$y_label_text <- renderText({
    translations[[selected_language()]]$name_y
  })
  
  output$font_size_text <- renderText({
    translations[[selected_language()]]$font_prompt
  })
  
  output$tact_info_text <- renderText({
    translations[[selected_language()]]$tact_paper_prompt
  })
  
  output$tactPlot <- renderPlot({
    # Use the chosen labels if given
    x_label <- input$x_label
    y_label <- input$y_label
    
    # Set font size
    par(cex.lab = input$font_size, cex.axis = input$font_size, cex.main = input$font_size,
        mar = c(5, 5, 4, 2) + 0.1)
    
    # Call TACT_fin function with 
    plot <- TACT_fin(r = input$r, distribution = "normal",
                     Xlab = bquote(bold(.(x_label))),  # Bold face axis labels
                     Ylab = bquote(bold(.(y_label))),
                     font_size = input$font_size,
                     language=selected_language())
    plot
  })
}

shinyApp(ui, server)