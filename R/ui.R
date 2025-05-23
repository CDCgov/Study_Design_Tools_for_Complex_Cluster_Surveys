# GitHub: https://github.com/CDCgov/Study_Design_Tools_for_Complex_Cluster_Surveys
# App: https://cdcgov.github.io/Study_Design_Tools_for_Complex_Cluster_Surveys/
#
# UI for Shinylive app
# Contents:
#   UI Diagram
#   Functions to instantiate inputs, other misc. code
#   ui object for app
# 
# UI Diagram:
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
#               conditionalPanel "2 Group, 2-Sided"
#               conditionalPanel "1 Group, 1-Sided"
#         mainPanel "Right"
#           tabsetPanel "Output Type"
#             tabPanel "Table"
#             tabPanel "Plots"
#               conditionalPanel "Sample size"
#               conditionalPanel "Half-width CI"
#             tabPanel "Statement"
#             tabPanel "R"
#     fluidRow "Bottom Row (footer)"


# Workaround for Shinylive Issue 468227
downloadButton = function(...) {
  tag = shiny::downloadButton(...)
  tag$attribs$download = NULL
  tag
}

# Objects to instantiate inputs

buildInputControlFor = function(fun_name){
  meta = eval(parse(text=paste0(".",fun_name,".meta")))
  f = function(x){
    type = x$input_control_type
    args = x$input_control_args
    args$label = paste0(args$inputId,": ",args$label)
    args$inputId = paste0(fun_name,"_",args$inputId)
    aschArgs = as.character(args)
    ii = sapply(args,length)==1 & !sapply(args,is.logical)
    aschArgs[ii] = paste0("'",aschArgs[ii],"'")
    val = paste0(type,paste0("(",paste(paste(names(args),aschArgs,sep="="),collapse=" , "),")"))
    eval(parse(text=val))
  }
  lapply(meta,f)
}

source("ui_input_controls_info.R")
ic_nOutTab = buildInputControlFor("nOutTab")
ic_dOutput = buildInputControlFor("dOutput")
ic_nclOutTab = buildInputControlFor("nclOutTab")
ic_ESS_2Grp_2Sided = buildInputControlFor("ESS_2Grp_2Sided")
ic_ESS_1Grp_1Sided = buildInputControlFor("ESS_1Grp_1Sided")

# selectInput()s for choosing calculation type
in_calc_type = function()selectInput("calc_type","Solve for:",c("Sample size","Half-width CI"))
in_calc_typeCo = function()selectInput("calc_type_Com","Study sub-type:",c("2 Group, 2-Sided","1 Group, 1-Sided"))



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
                ic_nOutTab$d,
                ic_nOutTab$p,
                ic_nOutTab$alpha,
                ic_nOutTab$m,
                ic_nOutTab$icc,
                ic_nOutTab$cv,
                ic_nOutTab$r
              ),
              conditionalPanel(
                condition = "input.calc_type == 'Half-width CI'",
                ic_dOutput$n,
                ic_dOutput$p,
                ic_dOutput$alpha,
                ic_dOutput$m,
                ic_dOutput$icc,
                ic_dOutput$cv,
                ic_dOutput$r
              )
            ),
            # Classification
            tabPanel(
              "Classification",
              condition = "input.study_type == 'Classification'",
              ic_nclOutTab$P0,
              ic_nclOutTab$delta,
              ic_nclOutTab$direction,
              ic_nclOutTab$alpha,
              ic_nclOutTab$beta,
              ic_nclOutTab$m,
              ic_nclOutTab$icc,
              ic_nclOutTab$cv,
              ic_nclOutTab$r
            ),
            # Comparison
            tabPanel(
              "Comparison",
              condition = "input.study_type == 'Comparison'",
              in_calc_typeCo(),
              conditionalPanel(
                condition = "input.calc_type_Com == '2 Group, 2-Sided'",
                ic_ESS_2Grp_2Sided$P1,
                ic_ESS_2Grp_2Sided$Delta,
                ic_ESS_2Grp_2Sided$Alpha,
                ic_ESS_2Grp_2Sided$Beta,
                ic_ESS_2Grp_2Sided$SS_ratio
              ),
              conditionalPanel(
                condition = "input.calc_type_Com == '1 Group, 1-Sided'",
                ic_ESS_1Grp_1Sided$P1,
                ic_ESS_1Grp_1Sided$P2,
                ic_ESS_1Grp_1Sided$Alpha,
                ic_ESS_1Grp_1Sided$Beta,
                ic_ESS_1Grp_1Sided$ESS1
              ),
            )
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
