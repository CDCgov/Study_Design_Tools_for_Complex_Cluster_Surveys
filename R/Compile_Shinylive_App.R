

# Required:
#   shiny
#   shinyFeedback
#   bslib
#   ggplot2
#   munsell
#   this.path (only if you're setting wd using source as below)
# Print message if any are not installed:
required_packages = c("shiny","shinyFeedback","bslib","ggplot2","munsell","this.path")
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
shinylive::export(appdir = getwd(), destdir = "../../web_app_placeholder")

# Check that the serverless web app works properly
httpuv::runStaticServer("../../web_app_placeholder")

