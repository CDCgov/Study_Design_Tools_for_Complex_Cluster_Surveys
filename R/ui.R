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
              selectInput(inputId = "calc_type",label="Solve for:",choices=c("Sample size","Half-width CI")),
              conditionalPanel(
                condition = "input.calc_type == 'Sample size'",
                textInput('d', 'Desired half-width CI', "0.05, 0.10")
              ),
              conditionalPanel(
                condition = "input.calc_type == 'Half-width CI'",
                textInput('n', 'Sample size', "300, 900"),
              ),
              selectInput('alpha', 'Type I error rate; usually 0.05 for 95% CI', c("0.025","0.05","0.10"), "0.05",TRUE),
              textInput('p', 'Expected coverage proportion', "0.10, 0.25"),
            ),
            # Classification
            tabPanel(
              "Classification",
              condition = "input.study_type == 'Classification'",
              textInput("P0", "The programmatic threshold", "0.7, 0.8"),
              textInput("delta", "A coverage percent defining a distance from P0", "0.01, 0.05"),
              textInput("alphaCL", "Type I error", "0.01, 0.05"),
              textInput("betaCL", "Type II error", "0.20, 0.10"),
              selectInput('direction', 'Direction', c("below","above"), "below",FALSE),
            ),
            # Comparison
            # ESS_2Grp_2sided<-function(P1, Delta, Alpha=0.05, Beta=0.20,SS_ratio=1.0)
            # ESS_1Grp_1sided<-function(P1, P2, Alpha=0.05, Beta=0.20,ESS1)
            tabPanel(
              "Comparison",
              condition = "input.study_type == 'Comparison'",
              textInput("text", "Enter text:")
            ),
            textInput('m', 'Target number of respondents per cluster', "5, 15"),
            textInput('icc', 'intracluster correlation coefficient', "1/22, 1/6"),
            textInput('cv', 'Coefficient of variation of sample weights', "0.50"),
            textInput('r', 'Anticipated non-response rate, from 0 to 1', "0.15")
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
