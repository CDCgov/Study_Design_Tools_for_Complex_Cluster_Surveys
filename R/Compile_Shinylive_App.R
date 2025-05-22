

# Required:
#   shiny
#   shinyFeedback
#   bslib
#   ggplot2
#   this.path (only if you're setting wd using source as below)
# Print message if any are not installed:
required_packages = c("shiny","shinyFeedback","bslib","ggplot2","this.path")
have_packages = required_packages %in% installed.packages()
if(!all(have_packages)){
  print("Please install the following package(s):")
  required_packages[!have_packages]
}


# A file with a single line: setwd(path),
# where path is "root_of_repo/R"
source(paste0(this.path::here(),"/","setWD.R"))


# Check that it runs properly on local machine
shiny::runApp()

# Compile into serverless web app
shinylive::export(appdir = getwd(), destdir = "../docs")

# Check that the serverless web app works properly
httpuv::runStaticServer("../docs")












source("output.R")

# Originally thought I would add a "meta" list under each function in the functions.R and output.R files.
# Now I think it's best to have a file input_controls_info.R and source() it in ui.R, then, above the ui
#   object, do something like
#   ic_est_n = buildInputControlFor(nOutTab) # returns a list of objects having shiny.tag class
#   and then inside the ui object place them where desired using, e.g. ic_est_n$d
# Having all the "meta" info in a single file is useful becuase then those that are used several times,
# e.g. m, icc, cv, r, can be merged with the others like .nOutTab.meta = c(.nOutTab.meta,.DE.meta,.INF.meta)

.nOutTab.meta = list(

  d = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "d",
      label = "Desired half-width CI",
      value = "0.05, 0.10"
    )
  )
  ,
  p = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "p",
      label = "Expected coverage proportion",
      value = "0.10, 0.25"
    )
  )
  ,
  alpha = list(
    input_control_type = "selectInput",
    input_control_args = list(
      inputId = "alpha",
      label = "Type I error rate",
      choices = c("0.01","0.025","0.05","0.10"),
      selected = "0.05",
      multiple = TRUE
    )
  )

)



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
ret=buildInputControlFor("nOutTab")
ret$d
ret$alpha
ret$p # can control order this way; each has class shiny.tag



getFunArgNames = function(fun_name)names(formals(get(fun_name)))
getFunArgNames("nOutTab")
getICIDforFun = function(fun_name)paste0(fun_name,"_",getFunArgNames(fun_name))
getICIDforFun("nOutTab")

fun_name="nOutTab"
i = list(c(0.10,0.25),c(0.05,0.20),15,1/6,.5,.15,0.05)
names(i) = getICIDforFun(fun_name)
i
#getOutputUsingFun = function(fun_name,input){
  unm = getICIDforFun(fun_name)
  fnm = getFunArgNames(fun_name)
#  i = getNumericInputs(unm, input)  
  names(i) = sub(paste0(fun_name,"_"),"",names(i))
  i = i[match(fnm,names(i))]
  txt = paste0(fun_name,paste0("(",paste(paste(fnm,i,sep="="),collapse=" , "),")"))
  eval(parse(text=txt))

#}








fun = "nOutTab"
meta = eval(parse(text=paste0(".",fun,".meta")))
x = meta[3]
f = function(x){
#  x=x[[1]]
  type = x$input_control_type
  args = x$input_control_args
  args$inputId = paste0(fun,"_",args$inputId)
  aschArgs = as.character(args)
  ii = sapply(args,length)==1 & !sapply(args,is.logical)
  aschArgs[ii] = paste0("'",aschArgs[ii],"'")
  val = paste0(type,paste0("(",paste(paste(names(args),aschArgs,sep="="),collapse=" , "),")"))
#  val = paste0(type,paste0("(",paste(paste(names(args),paste0("'",as.character(args),"'"),sep="="),collapse=" , "),")"))
  eval(parse(text=val))
}
lapply(meta,f)
#for(i in 1:length(meta))print(f(meta[i]))

textInput('p', 'p: Expected coverage proportion', "0.10, 0.25")
textInput('d', 'd: Desired half-width CI', "0.05, 0.10")
selectInput('alphaEstn', 'alpha: Type I error rate', c("0.01","0.025","0.05","0.10"), "0.05",TRUE)

# in ui.R: buildInputsFor(nOutTab)
# in server.R: useFunction(nOutTab)



x = function()nOutTab
as.list(nOutTab)

fun = "nOutTab"
x = eval(parse(text=fun))
x = str2lang(fun)
x(.1,.2,15)

useFunction = function(fun){
  
}


