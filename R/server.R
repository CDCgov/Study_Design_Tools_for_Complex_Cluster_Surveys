# Names of inputs used throughout
nm_Est_n = function()c("d","p","m","icc","cv","r","alphaEstn")
nm_Est_d = function()c("n","p","m","icc","cv","r","alphaEstd")
nm_Cla_1 = function()c("P0","delta","alphaCla","betaCla","m","icc","cv","r")
nm_Com_2 = function()c("P1","deltaCo","ssr","alphaCom2","betaCom2")
nm_Com_1 = function()c("PA","PB","essa","alphaCom1","betaCom1")

# Logical: checks to see if input is:
# a number, i.e. one or more digits [0-9] optionally immediatelly followed by a decimal point and one or more digits 
# OR
# a decimal number without the leading zero
# OR
# a fraction, i.e. a number immediately followed by a slash immediately followed by a number
# Note: currently does not allow numerator or denominator of fraction to be a decimal number unless it has a leading zero
numericInputIsValid = function(x){
  grepl("^\\d+(\\.\\d+)?$", x) | 
  grepl("^\\.\\d+$", x) | 
  grepl("^\\d+(\\.\\d+)?/\\d+(\\.\\d+)?$", x)
}

# Convert input from ui format to numerical format suitable for server code
preparedNumericalInput = function(x){
  if(length(x)>1)x=paste(x,collapse=", ")# handle selectInput values being character vector instead of a character string
  trimws(strsplit(x,",")[[1]])
}

# Logical: check whether numeric input is in correct format
inputIsOk = function(x){
  all(numericInputIsValid(x))
}

# Take the string input, convert to numeric. Handles fractions.
convertNumericInput = function(x,nm){
  x = preparedNumericalInput(x)
  if(inputIsOk(x)){
    val = rep(0,length(x))
    for(i in 1:length(x)){
      if(grepl("/",x[i])){
        val[i] = as.numeric(eval(parse(text=x[i])))
      } else {
        val[i] = as.numeric(x[i])
      }
    }
    val
  } else {
    stop("Please enter only numbers and/or valid fractions separated by commas, e.g., 1/2, 3.5/4, 0.05, 200")
  }
}

# Provide feedback on improper inputs
feedbackOnInput = function(x,nm){
  x = preparedNumericalInput(x)
  shinyFeedback::feedbackDanger(nm, !inputIsOk(x), "invalid input, see error message")
}
giveFeedbackOnInputs = function(x,nm){
  for(k in nm)feedbackOnInput(x[[k]],k)
}

# Return the inputs specified by names as a list of numeric vectors
getNumericInputs = function(names, x=input){
  i = reactiveValues()
  for(k in names)i[[k]]=convertNumericInput(x[[k]],k)
  i
}

getFunArgNames = function(fun_name)names(formals(get(fun_name)))
getICIDforFun = function(fun_name)paste0(fun_name,"_",getFunArgNames(fun_name))

getOutputUsingFun = function(fun_name,input){
  unm = getICIDforFun(fun_name)
  fnm = getFunArgNames(fun_name)
  ii = getNumericInputs(unm, input)  
  vals = reactiveValues()
  for(k in fnm)vals[[k]]=ii[[paste0(fun_name,"_",k)]]
  txt = paste0(fun_name,paste0("(",paste(paste(fnm,reactiveValuesToList(vals),sep="="),collapse=" , "),")"))
#print(paste0(fun_name,paste0("(",paste(paste(fnm,reactiveValuesToList(vals),sep="="),collapse=" , "),")")))
#print(paste(reactiveValuesToList(vals)))
#print(lapply(vals,identity))
#print(ii)
#print(names(ii))
#for(k in names(ii))print(ii[[k]])
#for(k in fnm){vals[[k]]=ii[[paste0(fun_name,"_",k)]];print(vals[[k]])}
#print(vals)
#print(names(vals))
#for(k in names(vals))print(vals[[k]])
#print(isolate(paste(vals)))
  eval(parse(text=txt))
}

# Makes table of sample sizes for Estimation studies
makeOutputTable = function(input){
  if(input$study_type == "Estimation"){
    if(input$calc_type == "Sample size"){
#      i = getNumericInputs(nm_Est_n(), input)
#      val = nOutTab(i$d,i$p,i$m,i$icc,i$cv,i$r,i$alphaEstn)
      val = getOutputUsingFun("nOutTab",input)
    }
    if(input$calc_type == "Half-width CI"){
      i = getNumericInputs(nm_Est_d(), input)
      val = dOutput(i$n,i$p,i$m,i$icc,i$cv,i$r,i$alphaEstd)
    }
  }
  if(input$study_type == "Classification"){
    if(TRUE){
      i = getNumericInputs(nm_Cla_1(), input)
      val = nclOutTab(i$P0,i$delta,i$alphaCla,i$betaCla,input$direction,i$m,i$icc,i$cv,i$r)[[1]]
    }
  }
  if(input$study_type == "Comparison"){
    if(TRUE){
      if(input$calc_type_Com == "2 Group, 2-Sided"){
        i = getNumericInputs(nm_Com_2(), input)
        val = ESS_2Grp_2sided(i$P1,i$deltaCo,i$alphaCom2,i$betaCom2,i$ssr)
      }
      if(input$calc_type_Com == "1 Group, 1-Sided"){
        i = getNumericInputs(nm_Com_1(), input)
        val = ESS_1Grp_1sided(i$PA,i$PB,i$alphaCom1,i$betaCom1,i$essa)
      }
    }
  }
  val
}



source("functions.R")
source("output.R")
source("Sample size 2 group design.R")

server <- function(input, output, session) {
  output$dev <- renderPrint({
    print(input)
    print(input$study_type)
    if(input$study_type=="Estimation")print(input$calc_type)
    if(input$study_type=="Comparison")print(input$calc_type_Com)
    for(k in numeric_input_names())print(paste(k,input[[k]]))
  })
  
#  study_type = reactive({input$study_type})
  
  # Reactive values
  output_table = reactive({makeOutputTable(input)})
  numeric_input_names = reactive({
    switch(
      input$study_type,
        "Estimation"=switch(
          input$calc_type,
            "Sample size"=getICIDforFun("nOutTab"),#nm_Est_n(),
            "Half-width CI"=nm_Est_d()
        ),
        "Classification"=nm_Cla_1(),
        "Comparison"=switch(
          input$calc_type_Com,
            "2 Group, 2-Sided"=nm_Com_2(),
            "1 Group, 1-Sided"=nm_Com_1()
          )
    )
  })

  # Give feedback on inputs
  toListen = reactive({
    i = reactiveValues()
    for(k in numeric_input_names())i[[k]]=input[[k]]
    i
  })
  observeEvent({toListen()},{
    giveFeedbackOnInputs(input,numeric_input_names())
  })

  # Create display table
  output$ESSdf = renderTable({
      output_table()
  }, striped=TRUE
  )


  

  # Set granularity of the plot lines
  x_gran = 10

  output$plot1 = renderPlot({
    i = getNumericInputs(nm_Est_n(), input)
    i$dLst = seq(min(i$d),max(i$d),length.out=x_gran)
    dat = nOutTab(i$dLst,i$p,i$m,i$icc,i$cv,i$r,i$alphaEstn)
    dat$p = as.factor(dat$p)
    ggplot2::ggplot(dat, ggplot2::aes(x = d, y = dat[,"n(ess,deff,inf)"], color = interaction(p,m,icc,cv,r,alpha), group = interaction(p,m,icc,cv,r,alpha))) +
      ggplot2::geom_line(size = 1) + ggplot2::geom_point() + 
      ggplot2::labs(title = "Sample Size as a function of CI Half-Width", y = "Sample Size", x = "CI Half-Width") +
      ggplot2::theme_minimal() + ggplot2::theme(legend.position="bottom")
  })
  output$plot2 = renderPlot({
    i = getNumericInputs(nm_Est_n(), input)
    i$pLst = seq(min(i$p),max(i$p),length.out=x_gran)
    dat = nOutTab(i$d,i$pLst,i$m,i$icc,i$cv,i$r,i$alphaEstn)
    ggplot2::ggplot(dat, ggplot2::aes(x = p, y = dat[,"n(ess,deff,inf)"], color = interaction(d,m,icc,cv,r,alpha), group = interaction(d,m,icc,cv,r,alpha))) +
      ggplot2::geom_line(size = 1) + ggplot2::geom_point() + 
      ggplot2::labs(title = "Sample Size as a function of Expected coverage proportion", y = "Sample Size", x = "Expected coverage proportion") +
      ggplot2::theme_minimal() + ggplot2::theme(legend.position="bottom")
  })
  output$plot = renderPlot({
    i = getNumericInputs(nm_Est_d(), input)
    i$nLst = seq(min(i$n),max(i$n),length.out=x_gran)
    dat = dOutput(i$nLst,i$p,i$m,i$icc,i$cv,i$r,i$alphaEstd)
    dat$p = as.factor(dat$p)
    ggplot2::ggplot(dat, ggplot2::aes(x = n, y = d, color = interaction(p,m,icc,cv,r,alpha), group = interaction(p,m,icc,cv,r,alpha))) +
      ggplot2::geom_line(size = 1) + ggplot2::geom_point() + 
      ggplot2::labs(title = "CI Half-Width as a function of Sample Size", x = "Sample Size", y = "CI Half-Width") +
      ggplot2::theme_minimal() + ggplot2::theme(legend.position="bottom")
  })
  
  
  output$statement_message = renderText({
    if(input$study_type=="Estimation" & input$calc_type == "Sample size"){
      v = output_table()[1,]
      val = paste0("
        Using the first row of the table as an example:\n
        With an expected coverage proportion of ",v$p,",
        a desired half-width CI of ",v$d,", 
        and a Type I error rate of ",v$alpha,", 
        the effective sample size is ",v$ess,".\n
        With an intracluster correlation coefficient of ",v$icc,", 
        a coefficient of variation of sample weights of ",v$cv,",
        and setting the target number of respondents per cluster at ",v$m," 
        results in a design effect of ",v$deff,".\n
        With this effective sample size, design effect and an anticipated non-response rate of ",v$r,"
        we have a required sample size of ",v["n(ess,deff,inf)"],".\n
        Combining sample size cluster size, we see we need ",v$nc," clusters."
      )
    }
    if(input$study_type=="Estimation" & input$calc_type == "Half-width CI"){
      v = output_table()[1,]
      val = paste0("
        Using the first row of the table as an example:\n
        Having an expected coverage proportion of ",v$p,",
        a target number of respondents per cluster of ",v$m,", 
        an intracluster correlation coefficient of ",v$icc,", 
        a coefficient of variation of sample weights of ",v$cv,",
        an anticipated non-response rate of ",v$r,",
        a Type I error rate of ",v$alpha,", 
        and setting the study's sample size at ",v$n," 
        results in a half-width CI of ",v$d
      )
    }
    val
  })
  
  output$R_print = renderText({"hello"})
  
  
  # Download
  EstSSOutTab = reactive({makeOutputTable(input)})
	
  output$ESSdfDownload = downloadHandler(
    filename = "sample_size.csv",
    content = function(file){write.csv(EstSSOutTab(),file,row.names=FALSE)}
  )


  # Bookmark
   observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(updateQueryString)
  
  
}
