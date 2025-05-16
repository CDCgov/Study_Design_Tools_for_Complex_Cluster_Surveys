
setwd("C:/Users/JNN6/OneDrive - CDC/GitHub/cluster_survey_sample_size/R")

# Check that it runs properly on local machine
shiny::runApp()

# Compile into serverless web app
shinylive::export(appdir = getwd(), destdir = "../docs")

# Check that the serverless web app works properly
httpuv::runStaticServer("../docs")
