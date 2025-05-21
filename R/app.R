
# A file with a single line: setwd(path),
# where path is "root_of_repo/R"
source("setWD.R")

library(shiny)
enableBookmarking(store = "url")
shinyAppDir(getwd())#,list(enableBookmarking = "url"))#Bookmarking not currently supported in Shinylive

