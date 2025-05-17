

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}



ui <- function(request){fluidPage(

  fluidRow( #main
  
  title = "Cluster Survey Sample Size",
   
  shinyFeedback::useShinyFeedback(),

  titlePanel("Cluster Survey Sample Size"),

  sidebarLayout(

    sidebarPanel(p("Select a Study Type:"),tabsetPanel(id="Input_Panel",

	  # Estimation
      tabPanel("Estimation",
        condition = "input.study_type == 'Estimation'",
		    selectInput(inputId = "estimation_n_or_d",label="Solve for:",choices=c("Sample size","Half-width CI")),
		    helpText("Enter comma-separated values, then click 'Update View'"),
    conditionalPanel(
		      condition = "input.estimation_n_or_d == 'Sample size'",
#         actionButton("Btn_Est_SS","Update View", icon("refresh")),
          textInput('d', 'Desired half-width CI', "0.05, 0.10")
		),
		conditionalPanel(
		  condition = "input.estimation_n_or_d == 'Half-width CI'",
#          actionButton("Btn_Est_CI","Update View", icon("refresh")),
          textInput('n', 'Sample size', "300, 900"),
		),
        textInput('p', 'Expected coverage proportion', "0.10, 0.25"),
        textInput('m', 'Target number of respondents per cluster', "5, 15"),
        textInput('icc', 'intracluster correlation coefficient', "1/22, 1/6"),
        textInput('cv', 'Coefficient of variation of sample weights', "0.50"),
        textInput('r', 'Anticipated non-response rate, from 0 to 1', "0.15"),
        selectInput('alpha', 'Type I error rate; usually 0.05 for 95% CI', c("0.025","0.05","0.10"), "0.05",TRUE)

      ),
	  
	  # Classification
      tabPanel("Classification",
        condition = "input.study_type == 'Classification'",
        numericInput("num", "Enter a number:", value = 1)
      ),
	  
	  # Comparison
      tabPanel("Comparison",
        condition = "input.study_type == 'Comparison'",
        textInput("text", "Enter text:")
      ),
	  
    )),

    # Main panel
    mainPanel(tabsetPanel(
      tabPanel("Table",
	      verbatimTextOutput("dev"),
	      # Bookmark feature not currently supported in Shinylive
        #fluidRow(downloadButton("ESSdfDownload","Download"), bookmarkButton()),
	      fluidRow(downloadButton("ESSdfDownload","Download")),
        fluidRow(tableOutput("ESSdf"))
      ),
      tabPanel("Plots",
        plotOutput("plot")#,
#        uiOutput("plotsUI")
      ),
      tabPanel("Statement",
        p("Description of results suitable for a manuscript or report.")
      )
    ))
	
	
  )
  ),# fluidRow main
  fluidRow(
    p("footer")
  )
)}








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
