# A number, i.e. one or more digits [0-9] optionally immediatelly followed 
#   by a decimal point and one or more digits 
# OR
# A decimal number without the leading zero
# OR
# A fraction, i.e. a number immediately followed by a slash 
#   immediately followed by a number
# Note: currently does not allow numerator or denominator of fraction to be a decimal number unless it has a leading zero
numeric_input_is_valid = function(x){
  grepl("^\\d+(\\.\\d+)?$", x) | 
  grepl("^\\.\\d+$", x) | 
  grepl("^\\d+(\\.\\d+)?/\\d+(\\.\\d+)?$", x)
}

# Take the string input, convert to numeric. Handles fractions.
convert_numeric_input = function(x,nm){
  if(length(x)>1)x=paste(x,collapse=", ")# handle "alpha" being a character vector instead of a character string
  x = trimws(strsplit(x,",")[[1]])
  input_is_ok = all(numeric_input_is_valid(x))
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
feedback_on_input = function(x,nm){
  if(length(x)>1)x=paste(x,collapse=", ")# handle "alpha" being a character vector instead of a character string
  x = trimws(strsplit(x,",")[[1]])
  input_is_ok = all(numeric_input_is_valid(x))
  shinyFeedback::feedbackDanger(nm, !input_is_ok, "invalid input, see error message")
}
give_feedback_on_inputs = function(input){
  i_nm = c("d","p","m","icc","cv","r","alpha")
  for(k in i_nm)feedback_on_input(input[[k]],k)
}

# Makes table of sample sizes for Estimation studies
make_Est_SS_Tab = function(input){
  if(input$estimation_n_or_d == "Sample size"){
    i_nm = c("d","p","m","icc","cv","r","alpha")
    i = reactiveValues()
    for(k in i_nm)i[[k]]=convert_numeric_input(input[[k]],k)
    val = nOutTab(i$d,i$p,i$m,i$icc,i$cv,i$r,i$alpha)
  }
  if(input$estimation_n_or_d == "Half-width CI"){
    i_nm = c("n","p","m","icc","cv","r","alpha")
    i = reactiveValues()
    for(k in i_nm)i[[k]]=convert_numeric_input(input[[k]],k)
    val = dOutput(i$n,i$p,i$m,i$icc,i$cv,i$r,i$alpha)
  }
  val
}



source("functions.R")
source("output.R")

server <- function(input, output, session) {
  output$dev <- renderPrint({
    print(input$estimation_n_or_d)
  })
  
  # React when selecting SS or CI
  n_or_d = reactive({input$estimation_n_or_d})

  # Give feedback on inputs
  observeEvent(1,{give_feedback_on_inputs(input)})

  # Create display table
  output$ESSdf = renderTable({
    which_n_or_d = n_or_d()
    if(which_n_or_d == "Sample size"){
      val = make_Est_SS_Tab(input)
    }
    if(which_n_or_d == "Half-width CI"){
      val = make_Est_SS_Tab(input)
    }
    val
  }, striped=TRUE
  )


#  # Render UI for plots dynamically
#  output$plotsUI <- renderUI({
#    
#    dat = make_Est_SS_Tab(input)
#    g = unique(dat[,c("m","icc","cv","r","alpha")])
#    plot_output_list <- lapply(1:nrow(g), function(i) {
#      plotOutput(outputId = paste0("plot", i))
#    })
#    do.call(tagList, plot_output_list)
#  })
#  
#  # Generate plots dynamically
#  observe({
#    dat = make_Est_SS_Tab(input)
#    g = unique(dat[,c("m","icc","cv","r","alpha")])
#    x.gran = 10 # granularity of the plot lines
#    for (i in 1:nrow(g)) {
#      local({
#        ii <- i
#        output[[paste0("plot", ii)]] <- renderPlot({
#
#        n = seq(min(dat$n),max(dat$n),length.out=x.gran)
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
  
  
  
output$plot = renderPlot({
    which_n_or_d = n_or_d()
    dat = make_Est_SS_Tab(input)
    if(which_n_or_d == "Sample size"){
      
    }
    if(which_n_or_d == "Half-width CI"){

      x.gran = 10 # granularity of the plot lines
      d = dat
      d$p = as.factor(d$p)
      ggplot2::ggplot(d, ggplot2::aes(x = n, y = d, color = interaction(p,m,icc,cv,r,alpha), group = interaction(p,m,icc,cv,r,alpha))) +
        ggplot2::geom_line(size = 1) +
        ggplot2::labs(title = "CI Half-Width as a function of Sample Size", x = "Sample Size", y = "CI Half-Width") +
        ggplot2::theme_minimal() + ggplot2::theme(legend.position="bottom")

    }
  })
  
#  # Create display plots
#  output$plot = renderPlot({
#    which_n_or_d = n_or_d()
#    dat = make_Est_SS_Tab(input)
#    if(which_n_or_d == "Sample size"){
#      
#    }
#    if(which_n_or_d == "Half-width CI"){
#
#      x.gran = 10 # granularity of the plot lines
#      g = unique(dat[,c("m","icc","cv","r","alpha")])
#      for(i in 1:nrow(g)){
#        n = seq(min(dat$n),max(dat$n),length.out=x.gran)
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
  
  # Download
  EstSSOutTab = reactive({make_Est_SS_Tab(input)})
	
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
