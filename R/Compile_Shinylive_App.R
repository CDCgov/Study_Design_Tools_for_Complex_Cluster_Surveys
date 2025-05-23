

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











fun = function(fun_name="nOutTab",x,z=0,...){
  ii = list(d=1,n=1)
  val = x + 1
  aLst = list(...)
#  any(names(aLst) %in% names(formals(fun_name))
    "expandForPlot" %in% names(aLst)
  aLst$expandForPlot
}
fun("nOutTab",2,expandForPlot="d")
names(formals(fun))

