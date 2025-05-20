# 
# 
# Outline:
# ui
#   fluidPage
#     fluidRow "Top Row"
#       sidebarLayout
#         sidebarPanel "Left"
#           tabsetPanel "Study Type"
#             tabPanel "Estimation"
#               conditionalPanel "Sample size"
#               conditionalPanel "Half-width CI"
#             tabPanel "Classification"
#             tabPanel "Comparison"
#         mainPanel "Right"
#           tabsetPanel "Output Type"
#             tabPanel "Table"
#             tabPanel "Plots"
#               conditionalPanel "Sample size"
#               conditionalPanel "Half-width CI"
#             tabPanel "Statement"
#     fluidRow "Bottom Row (footer)"


# Workaround for Issue 468227
downloadButton = function(...) {
  tag = shiny::downloadButton(...)
  tag$attribs$download = NULL
  tag
}

# Functions to instantiate inputs
in_calc_type = function()selectInput(inputId = "calc_type",label="Solve for:",choices=c("Sample size","Half-width CI"))
in_p = function()textInput('p', 'Expected coverage proportion', "0.10, 0.25")
in_d = function()textInput('d', 'Desired half-width CI', "0.05, 0.10")
in_n = function()textInput('n', 'Sample size', "300, 900")
in_P0 = function()textInput("P0", "The programmatic threshold", "0.7, 0.8")
in_delta = function()textInput("delta", "A coverage percent defining a distance from P0", "0.01, 0.05")
in_direction = function()selectInput('direction', 'Direction', c("below","above"), "below",FALSE)
in_alpha = function(initial_val)selectInput('alpha', 'Type I error rate', c("0.01","0.025","0.05","0.10"), initial_val,TRUE)
in_m = function()textInput('m', 'Target number of respondents per cluster', "5, 15")
in_icc = function()textInput('icc', 'intracluster correlation coefficient', "1/22, 1/6")
in_cv = function()textInput('cv', 'Coefficient of variation of sample weights', "0.50")
in_r = function()textInput('r', 'Anticipated non-response rate, from 0 to 1', "0.15")

ui = function(request){
  fluidPage(
    fluidRow( #fluidRow main
      title = "Cluster Survey Sample Size",
      shinyFeedback::useShinyFeedback(),
      titlePanel("Cluster Survey Sample Size"),
      sidebarLayout(
        sidebarPanel(
          p("Select a Study Type:"),
          tabsetPanel(
            id="study_type",
            helpText("Enter comma-separated values and select from drop-down menus"),
            # Estimation
            tabPanel(
              "Estimation",
              condition = "input.study_type == 'Estimation'",
              in_calc_type(),
              conditionalPanel(
                condition = "input.calc_type == 'Sample size'",
                in_d()
              ),
              conditionalPanel(
                condition = "input.calc_type == 'Half-width CI'",
                in_n()
              ),
              in_p(),
              in_alpha("0.05"),
              in_m(),in_icc(),in_cv(),in_r()
            ),
            # Classification
            tabPanel(
              "Classification",
              condition = "input.study_type == 'Classification'",
              in_P0(),
              in_delta(),
              in_direction(),
              in_alpha(c("0.01","0.05")),
              in_m(),in_icc(),in_cv(),in_r()
            ),
            # Comparison
            # ESS_2Grp_2sided<-function(P1, Delta, Alpha=0.05, Beta=0.20,SS_ratio=1.0)
            # ESS_1Grp_1sided<-function(P1, P2, Alpha=0.05, Beta=0.20,ESS1)
            tabPanel(
              "Comparison",
              condition = "input.study_type == 'Comparison'",
              selectInput(inputId = "calc_type_Com",label="Solve for:",choices=c("2 Group, 2-Sided","1 Group, 1-Sided")),
              conditionalPanel(
                condition = "input.calc_type_Com == '2 Group, 2-Sided'",
              ),
              conditionalPanel(
                condition = "input.calc_type_Com == '1 Group, 1-Sided'",
              ),
            ),
            selectInput('beta', 'Type II error rate', c("0.10","0.20"), "0.20",TRUE),
          )
        ),
        # Main panel
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Table",
              verbatimTextOutput("dev"),
              # Bookmark feature not currently supported in Shinylive
              #fluidRow(downloadButton("ESSdfDownload","Download"), bookmarkButton()),
              fluidRow(downloadButton("ESSdfDownload","Download")),
              fluidRow(tableOutput("ESSdf"))
            ),
            tabPanel(
              "Plots",
              conditionalPanel(
                condition = "input.calc_type == 'Sample size'",
                plotOutput("plot1"),
                plotOutput("plot2")
              ),
              conditionalPanel(
                condition = "input.calc_type == 'Half-width CI'",
                plotOutput("plot")
              )
            ),
            tabPanel(
              "Statement",
              verbatimTextOutput("statement_message")
            ),
            tabPanel(
              "R",
              verbatimTextOutput("R_print")
            )          
          )
        )
      )
    ),# fluidRow main
    fluidRow( # fluidRow footer 
      p("footer")
    ) # fluidRow footer
  )
}








#ui <- function(request){fluidPage(
#  
#  title = "Cluster Survey Sample Size",
#   
#  shinyFeedback::useShinyFeedback(),
#
#  titlePanel("Cluster Survey Sample Size"),
#
#  sidebarLayout(
#
#    sidebarPanel(
#
#	  selectInput(inputId = "study_type",
#                  label = "Choose a Study Type:",
#                  choices = c("Estimation", "Classification", "Comparison")),
#
#	  # Estimation
#      conditionalPanel(
#        condition = "input.study_type == 'Estimation'",
#        helpText("Enter comma-separated values, then click 'Update View'"),
#		selectInput(inputId = "estimation_n_or_d",label="Solve for:",choices=c("Sample size","Half-width CI")),
#		conditionalPanel(
#		  condition = "input.estimation_n_or_d == 'Sample size'",
#          actionButton("Btn_Est_SS","Update View", icon("refresh")),
#          textInput('d', 'Desired half-width CI', "0.05, 0.10")
#		 ),
#		conditionalPanel(
#		  condition = "input.estimation_n_or_d == 'Half-width CI'",
#          actionButton("Btn_Est_CI","Update View", icon("refresh")),
#          textInput('n', 'Sample size', "300, 900"),
#		 ),
#        textInput('p', 'Expected coverage proportion', "0.10, 0.25"),
#        textInput('m', 'Target number of respondents per cluster', "5, 15"),
#        textInput('icc', 'intracluster correlation coefficient', "1/22, 1/6"),
#        textInput('cv', 'Coefficient of variation of sample weights', "0.50"),
#        textInput('r', 'Anticipated non-response rate, from 0 to 1', "0.15"),
#        textInput('alpha', 'Type I error rate; usually 0.05 for 95% CI', "0.05")
#
#      ),
#	  
#	  # Classification
#      conditionalPanel(
#        condition = "input.study_type == 'Classification'",
#        numericInput("num", "Enter a number:", value = 1)
#      ),
#	  
#	  # Comparison
#      conditionalPanel(
#        condition = "input.study_type == 'Comparison'",
#        textInput("text", "Enter text:")
#      ),
#	  
#    ),
#
#    # Main panel
#    mainPanel(
#	  # Bookmark feature not currently supported in Shinylive
#      #fluidRow(downloadButton("ESSdfDownload","Download"), bookmarkButton()),
#	  fluidRow(downloadButton("ESSdfDownload","Download")),
#      fluidRow(tableOutput("ESSdf"))
#    )
#	
#	
#  )
#)}
