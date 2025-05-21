


# A file with a single line: setwd(path),
# where path is "root_of_repo/R"
source("setWD.R")

# Required:
#   shiny
#   shinyFeedback
#   bslib
#   ggplot2
# Print message if any are not installed:
reqPckgs = c("shiny","shinyFeedback","bslib","ggplot2")
havePckgs = reqPckgs%in%installed.packages()
if(!all(havePckgs)){
  print("Please install the following package(s):")
  reqPckgs[!havePckgs]
}



# Check that it runs properly on local machine
shiny::runApp()

# Compile into serverless web app
shinylive::export(appdir = getwd(), destdir = "../docs")

# Check that the serverless web app works properly
httpuv::runStaticServer("../docs")











