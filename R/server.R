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
  x = trimws(strsplit(x,",")[[1]])
  input_is_ok = all(numeric_input_is_valid(x))
  if(input_is_ok){
    val = rep(0,length(x))
    for(i in 1:length(x)){
      if(grepl("/",x[i])){
        val[i] = as.numeric(eval(parse(text=x)))
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
observe_inputs = function(x,nm){
  x = trimws(strsplit(x,",")[[1]])
  input_is_ok = all(numeric_input_is_valid(x))
  shinyFeedback::feedbackDanger(nm, !input_is_ok, "invalid input, see error message")
}

# Makes table of sample sizes for Estimation studies
make_Est_SS_Tab = function(input){
    i_nm = c("d","p","m","icc","cv","r","alpha")
	i = reactiveValues()
	for(k in i_nm)i[[k]]=convert_numeric_input(input[[k]],k)
    nOutTab(i$d,i$p,i$m,i$icc,i$cv,i$r,i$alpha)
}



source("functions.R")
source("output.R")

server <- function(input, output, session) {

  observeEvent(input$Btn_Est_SS,{observe_inputs(input$d,"d")})
  observeEvent(input$Btn_Est_SS,{observe_inputs(input$p,"p")})
  observeEvent(input$Btn_Est_SS,{observe_inputs(input$m,"m")})
  observeEvent(input$Btn_Est_SS,{observe_inputs(input$icc,"icc")})
  observeEvent(input$Btn_Est_SS,{observe_inputs(input$cv,"cv")})
  observeEvent(input$Btn_Est_SS,{observe_inputs(input$r,"r")})
  observeEvent(input$Btn_Est_SS,{observe_inputs(input$alpha,"alpha")})

  # Update table on button click
  updated_data = eventReactive(input$Btn_Est_SS, {
    make_Est_SS_Tab(input)
  })
  output$ESSdf = renderTable({
    updated_data()
  }, striped=TRUE
  )
  
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
