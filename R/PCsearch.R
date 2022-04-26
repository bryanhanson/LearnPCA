#'
#'  Shiny application to demonstrate the search for the 1st two principal components
#'  for a randomly generated set of data.
#'
#'  @return  None.  A web page opens with the application running.
#'  @author Bryan A. Hanson, David T. Harvey
#'
#' @import shiny
#' @import markdown
#' @importFrom graphics abline legend points segments text title
#' @importFrom stats runif var
#' @export
#'

PCsearch <- function() {

  # define colors
  pPC1_col <- "#f748a5"
  pPC1_colname <- "pink"
  pPC2_col <- "#3db7ed"
  pPC2_colname <- "light blue"
  proj_col <- 

  # ui.R ----
  ui <- fluidPage(

    # App title ----
    titlePanel("Searching for the Principal Component"),
 
    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(
        sliderInput(
          inputId = "rot_axes",
          label = "Angle to rotate test axis:",
          min = 0,
          max = 90,
          value = 15,
          step = 0.5
        ),
        checkboxInput(
          inputId = "show_all_PC1",
          label = "Show all projections on proposed PC1",
          value = FALSE
        ),
        sliderInput(
          inputId = "x_center",
          label = "Ellipse center x value:",
          min = -5,
          max = 5,
          value = 0,
          step = 0.5
        ),
        sliderInput(
          inputId = "y_center",
          label = "Ellipse center y value:",
          min = -5,
          max = 5,
          value = 0,
          step = 0.5
        ),
        sliderInput(
          inputId = "x_range",
          label = "Ellipse x range:",
          min = 4,
          max = 20,
          value = 10,
          step = 1
        ),
        sliderInput(
          inputId = "y_range",
          label = "Ellipse y range:",
          min = 4,
          max = 20,
          value = 4,
          step = 1
        ),
        sliderInput(
          inputId = "rot_ellipse",
          label = "Angle to rotate initial ellipse:",
          min = 0,
          max = 45,
          value = 30,
          step = 1
        ),
        numericInput(
          inputId = "seed",
          label = "seed",
          value = 31
        ),
        numericInput(
          inputId = "n",
          label = "No. of data points",
          value = 20
        )
      ),

      # Main panel for displaying outputs ----
      mainPanel(

        # Output: Tabset ----
        tabsetPanel(
          type = "tabs",
          tabPanel("Plot", plotOutput(outputId = "PCPlot", width = "700px", height = "700px")),
          # tabPanel("Plot", plotOutput(outputId = "PCPlot")),
          tabPanel("Parameters", uiOutput("parameters")),
          tabPanel("User Guide", uiOutput("user_guide"))
        ),
      )
    )
  )

  # server.R ----
  server <- function(input, output, session) {

    # get local files (addResourcePath did not work here)
    # app.css is identical to markdown.css but 'body' is commented out as it
    # was overriding the main container
    www <- system.file("www", package = "LearnPCA")
    p_file <- paste(www, "parameters.md", sep = "/")
    ug_file <- paste(www, "user_guide.md", sep = "/")
    css_file <- paste(www, "app.css", sep = "/")

    output$parameters <- renderUI({
      HTML(markdown::markdownToHTML(p_file, stylesheet = css_file))
    })

    output$user_guide <- renderUI({
      HTML(markdown::markdownToHTML(ug_file, stylesheet = css_file))
    })

    output$PCPlot <- renderPlot({
      .demo_PC_search(
        rot_ellipse = input$rot_ellipse,
        x_center = input$x_center,
        y_center = input$y_center,
        x_range = input$x_range,
        y_range = input$y_range,
        rot_axes = input$rot_axes,
        seed = as.integer(input$seed),
        n = as.integer(input$n),
        show_all_PC1 = input$show_all_PC1,
        shiny = TRUE
      )
    })
  }

  # Run the app ----
  shinyApp(ui = ui, server = server)
}
