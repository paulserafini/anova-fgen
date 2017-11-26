library(shinythemes)

shinyUI(fluidPage(theme = "simplex.css",
  headerPanel('', windowTitle = ''),
  
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Patua+One');
                    h1 {
                      font-family: 'Patua One';
                      font-weight: bold;
                      line-height: 1.1;
                      color: #333;
                    }
                    sup {
                      top: -3px;
                    }
                    td {
                    white-space: nowrap;
                    width: 1px;
                    padding-left: 8px;
                    padding-right: 8px;
                    padding-top: 6px;
                    padding-bottom: 6px;
                    color: #717171;
                    }"))),

  HTML("<br><br>"),
 
  sidebarLayout(
  sidebarPanel(
    
  sliderInput("num_factors", "Number of factors:",
                min = 2, max = 4,
                value = 2),
  uiOutput("factortypeInput"),
  radioButtons("step", "Step:", c("1: Add variances" = 1, "2: Remove fixed provisional variances" = 2, "3: Fill in weights" = 3, "4: Simplify weights" = 4)),
  actionButton("runButton", "Run"),
  HTML('<br><br>'),
  helpText("Note: The superscript on sigma is omitted because it doesn't play well with the effect subscript.")
  ),

  mainPanel(
    htmlOutput("foutput")
  ))
)
)