# Names of inputs used throughout
nm_Est = c("p","m","icc","cv","r","alpha")
nm_Est_all = c("n","d",nm_Est)
nm_Est_calc_n = c("d",nm_Est)
nm_Est_calc_d = c("n",nm_Est)

nm_Cla_all = c("P0","delta","alphaCL","betaCL","direction","m","icc","cv","r")
nm_Cla = nm_Cla_all[-5]

# Checks to see if input is:
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

# Take the string input, convert to numeric. Handles fractions.
convertNumericInput = function(x,nm){
  if(length(x)>1)x=paste(x,collapse=", ")# handle "alpha" being a character vector instead of a character string
  x = trimws(strsplit(x,",")[[1]])
  input_is_ok = all(numericInputIsValid(x))
  if(input_is_ok){
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
  if(length(x)>1)x=paste(x,collapse=", ")# handle "alpha" being a character vector instead of a character string
  x = trimws(strsplit(x,",")[[1]])
  input_is_ok = all(numericInputIsValid(x))
  shinyFeedback::feedbackDanger(nm, !input_is_ok, "invalid input, see error message")
}
giveFeedbackOnInputs = function(input){
  for(k in nm_Est_calc_n)feedbackOnInput(input[[k]],k)
}

# Return the inputs specified by names as a list of numeric vectors
getNumericInputs = function(names, x=input){
  i = reactiveValues()
  for(k in names)i[[k]]=convertNumericInput(x[[k]],k)
  i
}

# Makes table of sample sizes for Estimation studies
makeOutputTable = function(input){
  if(input$study_type == "Estimation"){
    if(input$calc_type == "Sample size"){
      i = getNumericInputs(nm_Est_calc_n, input)
      val = nOutTab(i$d,i$p,i$m,i$icc,i$cv,i$r,i$alpha)
    }
    if(input$calc_type == "Half-width CI"){
      i = getNumericInputs(nm_Est_calc_d, input)
      val = dOutput(i$n,i$p,i$m,i$icc,i$cv,i$r,i$alpha)
    }
  }
  if(input$study_type == "Classification"){
#    if(TRUE){
      i = getNumericInputs(nm_Cla, input)
      val = nclOutTab(i$P0,i$delta,i$alphaCL,i$betaCL,input$direction,i$m,i$icc,i$cv,i$r)[[1]]
#    }
  }
  if(input$study_type == "Comparison"){
    if(TRUE){
      #i = getNumericInputs(nm_Est_calc_n, input)
      #val = nOutTab(i$d,i$p,i$m,i$icc,i$cv,i$r,i$alpha)
    }
  }
  val
}



source("functions.R")
source("output.R")

server <- function(input, output, session) {
  output$dev <- renderPrint({
    print(input)
#    print(names(input))
    print(input$study_type)
    print(input$calc_type)
    kk=switch(input$study_type,"Estimation"=nm_Est_all,"Classification"=nm_Cla_all)
    print(kk)
    for(k in kk)print(input[[k]])
#    for(k in nm_Est_calc_n)print(x[[k]])
#      i_nm = c("d","p","m","icc","cv","r","alpha")
#      i = reactiveValues()
#      for(k in i_nm)i[[k]]=convertNumericInput(input[[k]],k)
#      print(seq(min(i$d),max(i$d),length.out=x_gran))
#      i$dLst = seq(min(i$d),max(i$d),length.out=x_gran)
#      dat = nOutTab(i$dLst,i$p,i$m,i$icc,i$cv,i$r,i$alpha)
#      print(head(dat,23))
  })
  
  
#  study_type = reactive({input$study_type})
  
  # React when selecting SS or CI
  n_or_d = reactive({input$calc_type})

  # Give feedback on inputs
  toListen = reactive({
    i = reactiveValues()
    for(k in nm_Est_calc_n)i[[k]]=input[[k]]
    i
  })
  observeEvent({toListen()},{
    giveFeedbackOnInputs(input)
  })

  # Create display table
  output$ESSdf = renderTable({
      makeOutputTable(input)
  }, striped=TRUE
  )


  

  # Set granularity of the plot lines
  x_gran = 10

  output$plot1 = renderPlot({
    which_n_or_d = n_or_d()
    if(input$study_type=="Estimation" & which_n_or_d == "Sample size"){
      i = getNumericInputs(nm_Est_calc_n, input)
      i$dLst = seq(min(i$d),max(i$d),length.out=x_gran)
      dat = nOutTab(i$dLst,i$p,i$m,i$icc,i$cv,i$r,i$alpha)
      dat$p = as.factor(dat$p)
      ggplot2::ggplot(dat, ggplot2::aes(x = d, y = dat[,"n(ess,deff,inf)"], color = interaction(p,m,icc,cv,r,alpha), group = interaction(p,m,icc,cv,r,alpha))) +
        ggplot2::geom_line(size = 1) + ggplot2::geom_point() + 
        ggplot2::labs(title = "Sample Size as a function of CI Half-Width", y = "Sample Size", x = "CI Half-Width") +
        ggplot2::theme_minimal() + ggplot2::theme(legend.position="bottom")
    }
  })
  output$plot2 = renderPlot({
    which_n_or_d = n_or_d()
    if(input$study_type=="Estimation" & which_n_or_d == "Sample size"){
      x_gran = 10 # granularity of the plot lines
      i = reactiveValues()
      for(k in nm_Est_calc_n)i[[k]]=convertNumericInput(input[[k]],k)
      i$pLst = seq(min(i$p),max(i$p),length.out=x_gran)
      dat = nOutTab(i$d,i$pLst,i$m,i$icc,i$cv,i$r,i$alpha)
      ggplot2::ggplot(dat, ggplot2::aes(x = p, y = dat[,"n(ess,deff,inf)"], color = interaction(d,m,icc,cv,r,alpha), group = interaction(d,m,icc,cv,r,alpha))) +
        ggplot2::geom_line(size = 1) + ggplot2::geom_point() + 
        ggplot2::labs(title = "Sample Size as a function of Expected coverage proportion", y = "Sample Size", x = "Expected coverage proportion") +
        ggplot2::theme_minimal() + ggplot2::theme(legend.position="bottom")
    }
  })
  
  output$plot = renderPlot({
    which_n_or_d = n_or_d()
    if(input$study_type=="Estimation" & which_n_or_d == "Half-width CI"){
      x_gran = 10 # granularity of the plot lines
      i = reactiveValues()
      for(k in nm_Est_calc_d)i[[k]]=convertNumericInput(input[[k]],k)
      i$nLst = seq(min(i$n),max(i$n),length.out=x_gran)
      dat = dOutput(i$nLst,i$p,i$m,i$icc,i$cv,i$r,i$alpha)
      dat$p = as.factor(dat$p)
      ggplot2::ggplot(dat, ggplot2::aes(x = n, y = d, color = interaction(p,m,icc,cv,r,alpha), group = interaction(p,m,icc,cv,r,alpha))) +
        ggplot2::geom_line(size = 1) + ggplot2::geom_point() + 
        ggplot2::labs(title = "CI Half-Width as a function of Sample Size", x = "Sample Size", y = "CI Half-Width") +
        ggplot2::theme_minimal() + ggplot2::theme(legend.position="bottom")

    }
  })
  
  
  output$statement_message = renderText({
    which_n_or_d = n_or_d()
    if(input$study_type=="Estimation" & which_n_or_d == "Sample size"){
      v = makeOutputTable(input)[1,]
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
    if(input$study_type=="Estimation" & which_n_or_d == "Half-width CI"){
      v = makeOutputTable(input)[1,]
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












#  # Render UI for plots dynamically
#  output$plotsUI <- renderUI({
#    
#    dat = makeOutputTable(input)
#    g = unique(dat[,c("m","icc","cv","r","alpha")])
#    plot_output_list <- lapply(1:nrow(g), function(i) {
#      plotOutput(outputId = paste0("plot", i))
#    })
#    do.call(tagList, plot_output_list)
#  })
#  
#  # Generate plots dynamically
#  observe({
#    dat = makeOutputTable(input)
#    g = unique(dat[,c("m","icc","cv","r","alpha")])
#    x_gran = 10 # granularity of the plot lines
#    for (i in 1:nrow(g)) {
#      local({
#        ii <- i
#        output[[paste0("plot", ii)]] <- renderPlot({
#
#        n = seq(min(dat$n),max(dat$n),length.out=x_gran)
#        d = dOutput(n,unique(dat$p),g$m[ii],g$icc[ii],g$cv[ii],g$r[ii],g$alpha[ii])
#        d$p = as.factor(d$p)
#        tit = paste0("m = ",g$m[ii]," , ","icc = ",g$icc[ii]," , ","cv = ",g$cv[ii]," , ","r = ",g$r[ii]," , ","alpha = ",g$alpha[ii])        
#        ggplot2::ggplot(d, ggplot2::aes(x = n, y = d, color = p, group = p)) +
#          ggplot2::geom_line(size = 1) +
#          ggplot2::labs(title = tit, x = "Sample Size", y = "CI Half-Width") +
#          ggplot2::theme_minimal()
##        plot(d$n,d$d,main=tit)
#
#        })
#      })
#    }
#  })


#  # Create display plots
#  output$plot = renderPlot({
#    which_n_or_d = n_or_d()
#    dat = makeOutputTable(input)
#    if(which_n_or_d == "Sample size"){
#      
#    }
#    if(which_n_or_d == "Half-width CI"){
#
#      x_gran = 10 # granularity of the plot lines
#      g = unique(dat[,c("m","icc","cv","r","alpha")])
#      for(i in 1:nrow(g)){
#        n = seq(min(dat$n),max(dat$n),length.out=x_gran)
#        d = dOutput(n,unique(dat$p),g$m[i],g$icc[i],g$cv[i],g$r[i],g$alpha[i])
#        d$p = as.factor(d$p)
#        tit = paste0("m = ",g$m[i]," , ","icc = ",g$icc[i]," , ","cv = ",g$cv[i]," , ","r = ",g$r[i]," , ","alpha = ",g$alpha[i])        
#        val = ggplot2::ggplot(d, ggplot2::aes(x = n, y = d, color = p, group = p)) +
#          ggplot2::geom_line(size = 1) +
#          ggplot2::labs(title = tit, x = "Sample Size", y = "CI Half-Width") +
#          ggplot2::theme_minimal()
#        print(val)
#      }
#    }
#  })
