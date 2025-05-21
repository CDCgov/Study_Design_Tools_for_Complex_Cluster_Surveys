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
in_calc_type = function()selectInput("calc_type","Solve for:",c("Sample size","Half-width CI"))
in_p = function()textInput('p', 'p: Expected coverage proportion', "0.10, 0.25")
in_d = function()textInput('d', 'd: Desired half-width CI', "0.05, 0.10")
in_n = function()textInput('n', 'n: Sample size', "300, 900")
in_P0 = function()textInput("P0", "P0: Programmatic threshold", "0.7, 0.8")
in_delta = function()textInput("delta", "delta: A coverage percent defining a distance from P0", "0.01, 0.05")
in_direction = function()selectInput('direction', 'Direction', c("below","above"), "below",FALSE)
in_m = function()textInput('m', 'm: Target number of respondents per cluster', "5, 15")
in_icc = function()textInput('icc', 'icc: Intracluster correlation coefficient', "1/22, 1/6")
in_cv = function()textInput('cv', 'cv: Coefficient of variation of sample weights', "0.50")
in_r = function()textInput('r', 'r: Anticipated non-response rate, from 0 to 1', "0.15")
in_calc_typeCo = function()selectInput("calc_type_Com","Study sub-type:",c("2 Group, 2-Sided","1 Group, 1-Sided"))
in_P1 = function()textInput('P1', 'P1: Estimated coverage level from one of the two surveys', "0.5")
in_deltaCo = function()textInput('deltaCo', 'delta: difference above P1 form which the survey should be well powered to reject the null hypothesis', "0.10")
in_ssr = function()textInput('ssr', 'ssr: Sample size ratio between two groups n2:n1', "1.1")
in_PA = function()textInput('PA', 'PA: Coverage for the previously conducted survey', "0.7")
in_PB = function()textInput('PB', 'PB: Coverage for the planned survey', "0.8")
in_essa = function()textInput('essa', 'essa: Effective sample size from early conducted survey', "174")
in_alphaEstn = function()selectInput('alphaEstn', 'alpha: Type I error rate', c("0.01","0.025","0.05","0.10"), "0.05",TRUE)
in_alphaEstd = function()selectInput('alphaEstd', 'alpha: Type I error rate', c("0.01","0.025","0.05","0.10"), "0.05",TRUE)
in_alphaCla  = function()selectInput('alphaCla', 'alpha: Type I error rate', c("0.01","0.025","0.05","0.10"), c("0.01","0.05"),TRUE)
in_alphaCom2 = function()selectInput('alphaCom2', 'alpha: Type I error rate', c("0.01","0.025","0.05","0.10"), "0.10",TRUE)
in_alphaCom1 = function()selectInput('alphaCom1', 'alpha: Type I error rate', c("0.01","0.025","0.05","0.10"), "0.05",TRUE)
in_betaCla  = function()selectInput('betaCla', 'beta: Type II error rate', c("0.10","0.20"), c("0.20","0.10"),TRUE)
in_betaCom2 = function()selectInput('betaCom2', 'beta: Type II error rate', c("0.10","0.20"), "0.20",TRUE)
in_betaCom1 = function()selectInput('betaCom1', 'beta: Type II error rate', c("0.10","0.20"), "0.20",TRUE)

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
                in_d(),
                in_alphaEstn()
              ),
              conditionalPanel(
                condition = "input.calc_type == 'Half-width CI'",
                in_n(),
                in_alphaEstd()
              ),
              in_p(),
              in_m(),in_icc(),in_cv(),in_r()
            ),
            # Classification
            tabPanel(
              "Classification",
              condition = "input.study_type == 'Classification'",
              in_P0(),
              in_delta(),
              in_direction(),
              in_alphaCla(),
              in_betaCla(),
              in_m(),in_icc(),in_cv(),in_r()
            ),
            # Comparison
            tabPanel(
              "Comparison",
              condition = "input.study_type == 'Comparison'",
              in_calc_typeCo(),
              conditionalPanel(
                condition = "input.calc_type_Com == '2 Group, 2-Sided'",
                in_P1(),
                in_deltaCo(),
                in_alphaCom2(),
                in_betaCom2(),
                in_ssr()
              ),
              conditionalPanel(
                condition = "input.calc_type_Com == '1 Group, 1-Sided'",
                in_PA(),
                in_PB(),
                in_alphaCom1(),
                in_betaCom1(),
                in_essa()
              ),
            ),
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
