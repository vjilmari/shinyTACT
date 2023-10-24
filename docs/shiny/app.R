library(shiny)
library(shinyjs)
library(jsonlite)
library(dplyr)

source("TACT_fun_basic.R")
source("TACT_languages.R")

# import languages
translations <- list(
  fi = fromJSON("fi.json"),
  en = fromJSON("en.json"),
  ge = fromJSON("ge.json"),
  es = fromJSON("es.json")
)


ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  # Add custom CSS to style the radio button options
  tags$style(HTML("
    .radio-btn-inline {
      display: inline-block;
      margin-right: 10px;
    }
  ")),
  
  div(id = "radioButtons",
      radioButtons("mode_choice", "Mode",
                   c("Basic", "Advanced"),
                   inline = TRUE  # Display the radio buttons inline
      )
  ),
  
  # PANEL FOR BASIC MODE
  conditionalPanel(
    condition = 'input.mode_choice == "Basic"',
    selectInput("language_basic", "Language:",
                choices = c(
                  "Suomi" = "fi",
                  "English" = "en",
                  "Deutsch" = "ge",
                  "Eesti keel" = "es"
                ),
                selected = "fi"
    ),
    uiOutput("title_panel_basic"),
    tags$style(HTML("
    .sidebar_basic { 
      position: sticky;
      top: 0;
      height: calc(100vh - 70px);
      overflow-y: auto;
    }
    #tactPlot_basic { 
      max-width: 100%;
      width: auto;
      height: auto;
      padding-bottom: 100%;
      position: relative;
    }
    #tactPlot_basic > .plot-container { 
      position: absolute;
      width: 100%;
      height: 100%;
    }
    .text-below-plot_basic { 
      background-color: white;
      border: 1px solid #000;
      border-radius: 5px;
    }
  ")),
    sidebarLayout(
      sidebarPanel(
        width=5,
        sliderInput("r_basic", textOutput("slider_label_basic"), min = -1, max = 1, value = 0, step = 0.01), 
        textInput("x_label_basic", textOutput("x_label_text_basic"), value = "X"), 
        textInput("y_label_basic", textOutput("y_label_text_basic"), value = "Y"), 
        sliderInput("font_size_basic", textOutput("font_size_text_basic"), min = 0.1, max = 3, value = 1, step = 0.1), 
        sliderInput("point_size_basic", textOutput("point_size_text_basic"), min = 0.1, max = 3, value = 1, step = 0.1), 
        tags$p(textOutput("tact_info_text_basic"), 
               tags$br(),
               tags$a("Mõttus, R. (2022). What Correlations Mean for Individual People: A Tutorial for Researchers, Students and the Public. Personality Science, 3, 1–27. https://doi.org/10.5964/ps.7467",
                      href = "https://doi.org/10.5964/ps.7467",
                      target = "_blank")
        ),
        class = "sidebar_basic" 
      ),
      mainPanel(
        width=7,
        plotOutput("tactPlot_basic"), 
        uiOutput("text_below_plot_basic") 
      )
    )
  )
  ,
  # ADVANCED PANEL MODEL
  conditionalPanel(
    condition = 'input.mode_choice == "Advanced"',
    selectInput("language_advanced", "Language:",
                choices = c(
                  "Suomi" = "fi",
                  "English" = "en",
                  "Deutsch" = "ge",
                  "Eesti keel" = "es"
                ),
                selected = "fi"
    ),
    uiOutput("title_panel_advanced"),
    tags$style(HTML("
    .sidebar_advanced { 
      position: sticky;
      top: 0;
      height: calc(100vh - 70px);
      overflow-y: auto;
    }
    #tactPlot_advanced { 
      max-width: 100%;
      width: auto;
      height: auto;
      padding-bottom: 100%;
      position: relative;
    }
    #tactPlot_advanced > .plot-container { 
      position: absolute;
      width: 100%;
      height: 100%;
    }
    .text-below-plot_advanced { 
      background-color: white;
      border: 1px solid #000;
      border-radius: 5px;
    }
  ")),
    sidebarLayout(
      sidebarPanel(
        width=5,
        sliderInput("r_advanced", textOutput("slider_label_advanced"), min = -1, max = 1, value = 0, step = 0.01), 
        textInput("x_label_advanced", textOutput("x_label_text_advanced"), value = "X"), 
        textInput("y_label_advanced", textOutput("y_label_text_advanced"), value = "Y"), 
        sliderInput("font_size_advanced", textOutput("font_size_text_advanced"), min = 0.1, max = 3, value = 1, step = 0.1), 
        sliderInput("point_size_advanced", textOutput("point_size_text_advanced"), min = 0.1, max = 3, value = 1, step = 0.1), 
        selectInput("distribution_advanced", "Variable Distribution",
                    choices = c("normal", "uniform", "skewed"),
                    selected = "normal"
        ),
        numericInput(inputId="n_advanced",
                     label= "Simulation sample size", value = 1000000, min = 10, max = 1000000000),
        numericInput(inputId="n.plotted_advanced",
                     label= "Number of data points plotted", value = 1000, min = 1, max = 100000),
        numericInput(inputId="cutoffsx1_advanced",
                     label= "First cutoff", value = round(1/3,3), min = 1/10^8, max = 1-1/10^8),
        numericInput(inputId="cutoffsx2_advanced",
                     label= "Second cutoff", value = round(2/3,3), min = 1/10^8, max = 1-1/10^8),
        tags$p(textOutput("tact_info_text_advanced"), 
               tags$br(),
               tags$a("Mõttus, R. (2022). What Correlations Mean for Individual People: A Tutorial for Researchers, Students and the Public. Personality Science, 3, 1–27. https://doi.org/10.5964/ps.7467",
                      href = "https://doi.org/10.5964/ps.7467",
                      target = "_blank")
        ),
        class = "sidebar_advanced" 
      ),
      mainPanel(
        width=7,
        plotOutput("tactPlot_advanced"), 
        uiOutput("text_below_plot_advanced") 
      )
    )
  )
)

server <- function(input, output) {
  # BASIC server
  selected_language <- reactive({
    input$language_basic
  })
  
  output$title_panel_basic <- renderUI({
    HTML(paste0("<h1>", translations[[selected_language()]]$title, "</h1>"))
  })
  
  output$slider_label_basic <- renderText({
    translations[[selected_language()]]$r_slider
  })
  
  output$x_label_text_basic <- renderText({
    translations[[selected_language()]]$name_x
  })
  
  output$y_label_text_basic <- renderText({
    translations[[selected_language()]]$name_y
  })
  
  output$font_size_text_basic <- renderText({
    translations[[selected_language()]]$font_prompt
  })
  
  output$point_size_text_basic <- renderText({
    translations[[selected_language()]]$point_prompt
  })
  
  output$tact_info_text_basic <- renderText({
    translations[[selected_language()]]$tact_paper_prompt
  })
  
  output$tactPlot_basic <- renderPlot({
    # Use the chosen labels if given
    x_label <- input$x_label_basic
    y_label <- input$y_label_basic
    
    # Set font size
    par(cex.lab = input$font_size_basic, cex.axis = input$font_size_basic, cex.main = input$font_size_basic,
        mar = c(5, 5, 4, 2) + 0.1)
    
    # Call TACT_fun_basic function with 
    plot <- TACT_fun_basic(r = input$r_basic, distribution = "normal",
                           Xlab = bquote(bold(.(x_label))),  # Bold face axis labels
                           Ylab = bquote(bold(.(y_label))),
                           font_size = input$font_size_basic,
                           Cex_multiplier = input$point_size_basic,
                           language = selected_language())
    plot$plot
    
    # Below the plot text 
    output$text_below_plot_basic <- renderUI({
      if (input$r_basic >= 0) {
        bullet_points <- ""
        
        if (abs(diff(plot$cutoffsx)) > 0.001 & abs(diff(plot$cutoffsy)) > 0.001 & input$language_basic != "es") {
          bullet_points <- paste(
            "<li>", 
            round(sum(diag(t(apply(plot$crosstabs, 2, rev)))) / sum(plot$crosstabs) * 100, 1),
            TACT_languages[TACT_languages$language == input$language_basic, "match_1"],
            "</li>",
            "<li>",
            TACT_languages[TACT_languages$language == input$language_basic, "match_2"],
            "</li>",
            sep = ""
          )
        } else if (abs(diff(plot$cutoffsx)) > 0.001 & abs(diff(plot$cutoffsy)) > 0.001 & input$language_basic == "es") {
          bullet_points <- paste(
            "<li>Keskmiselt ",
            round(sum(diag(t(apply(plot$crosstabs, 2, rev)))) / sum(plot$crosstabs) * 100, 1),
            "% väärtustest kattuvad",
            "</li>",
            "<li>Seose täieliku puudumise korral kattuks 33.3%</li>",
            sep = ""
          )
        }
        
        # Create an unordered list with bullet points
        HTML(paste0("<ul class='text-below-plot_basic' style='font-size: ",
                    input$font_size_basic, "em;'>", bullet_points, "</ul>"))
      }
    })
  })
  
  
  # ADVANCED server
  selected_language <- reactive({
    input$language_advanced
  })
  
  output$title_panel_advanced <- renderUI({
    HTML(paste0("<h1>", translations[[selected_language()]]$title, "</h1>"))
  })
  
  output$slider_label_advanced <- renderText({
    translations[[selected_language()]]$r_slider
  })
  
  output$x_label_text_advanced <- renderText({
    translations[[selected_language()]]$name_x
  })
  
  output$y_label_text_advanced <- renderText({
    translations[[selected_language()]]$name_y
  })
  
  output$font_size_text_advanced <- renderText({
    translations[[selected_language()]]$font_prompt
  })
  
  output$point_size_text_advanced <- renderText({
    translations[[selected_language()]]$point_prompt
  })
  
  output$tact_info_text_advanced <- renderText({
    translations[[selected_language()]]$tact_paper_prompt
  })
  
  output$tactPlot_advanced <- renderPlot({
    # Use the chosen labels if given
    x_label <- input$x_label_advanced
    y_label <- input$y_label_advanced
    
    # Set font size
    par(cex.lab = input$font_size_advanced, cex.axis = input$font_size_advanced, cex.main = input$font_size_advanced,
        mar = c(5, 5, 4, 2) + 0.1)
    
    # Call TACT_fun_advanced function with 
    plot <- TACT_fun_basic(r = input$r_advanced, distribution = input$distribution_advanced,
                           Xlab = bquote(bold(.(x_label))),  # Bold face axis labels
                           Ylab = bquote(bold(.(y_label))),
                           n = input$n_advanced,n.plotted = input$n.plotted_advanced,
                           cutoffsx=c(input$cutoffsx1_advanced,input$cutoffsx2_advanced),
                           cutoffsy=c(input$cutoffsx1_advanced,input$cutoffsx2_advanced),
                           font_size = input$font_size_advanced,
                           Cex_multiplier = input$point_size_advanced,
                           language = selected_language())
    plot$plot
    
    # Below the plot text 
    output$text_below_plot_advanced <- renderUI({
      if (input$r_advanced >= 0) {
        bullet_points <- ""
        
        if (abs(diff(plot$cutoffsx)) > 0.001 & abs(diff(plot$cutoffsy)) > 0.001 & input$language_advanced != "es") {
          bullet_points <- paste(
            "<li>", 
            round(sum(diag(t(apply(plot$crosstabs, 2, rev)))) / sum(plot$crosstabs) * 100, 1),
            TACT_languages[TACT_languages$language == input$language_advanced, "match_1"],
            "</li>",
            "<li>",
            TACT_languages[TACT_languages$language == input$language_advanced, "match_2"],
            "</li>",
            sep = ""
          )
        } else if (abs(diff(plot$cutoffsx)) > 0.001 & abs(diff(plot$cutoffsy)) > 0.001 & input$language_advanced == "es") {
          bullet_points <- paste(
            "<li>Keskmiselt ",
            round(sum(diag(t(apply(plot$crosstabs, 2, rev)))) / sum(plot$crosstabs) * 100, 1),
            "% väärtustest kattuvad",
            "</li>",
            "<li>Seose täieliku puudumise korral kattuks 33.3%</li>",
            sep = ""
          )
        }
        
        # Create an unordered list with bullet points
        HTML(paste0("<ul class='text-below-plot_advanced' style='font-size: ",
                    input$font_size_advanced, "em;'>", bullet_points, "</ul>"))
      }
    })
  })
}

shinyApp(ui, server)
