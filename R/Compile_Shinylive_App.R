
setwd("C:/GitHub/Study_Design_Tools_for_Complex_Cluster_Surveys/R")

# Check that it runs properly on local machine
shiny::runApp()

# Compile into serverless web app
shinylive::export(appdir = getwd(), destdir = "../docs")

# Check that the serverless web app works properly
httpuv::runStaticServer("../docs")


a = "0.05, 0.10"
b = c("0.05" , "0.025")
paste(b,collapse=", ")


n = c(300,900)
p = c(.1,.25)
m = c(5, 15)
icc = c(1/22, 1/6)
cv = c(0.50)
r = c(0.15)
alpha = 0.05

dat=expand.grid(n,p,m,icc,cv,r,alpha)
colnames(dat) = c("n","p","m","icc","cv","r","alpha")
unique(dat[,3:7])

x.gran = 10 # granularity of the plot lines
g = unique(dat[,c("m","icc","cv","r","alpha")])
plotList = list()
for(i in 1:nrow(g)){
  n = seq(min(dat$n),max(dat$n),length.out=x.gran)
  d = dOutput(n,unique(dat$p),g$m[i],g$icc[i],g$cv[i],g$r[i],g$alpha[i])
  d$p = as.factor(d$p)
  plotList[[i]] = ggplot2::ggplot(d, ggplot2::aes(x = n, y = d, color = p, group = p)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(title = "Line Plot by Group", x = "X-Axis", y = "Y-Axis") +
    ggplot2::theme_minimal()
}













