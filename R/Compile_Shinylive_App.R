
setwd("C:/GitHub/Study_Design_Tools_for_Complex_Cluster_Surveys/R")

# Check that it runs properly on local machine
shiny::runApp()

# Compile into serverless web app
shinylive::export(appdir = getwd(), destdir = "../docs")

# Check that the serverless web app works properly
httpuv::runStaticServer("../docs")

