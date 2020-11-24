# install.packages("remotes")
# remotes::install_github("mfasiolo/volesModel")
library(volesModel)
library(plotly)
library(matrixStats)


data(voles_data)
voles_data$date<-voles_data$year+0.25
voles_data[which(voles_data$season=="autumn"),"date"]<-voles_data[which(voles_data$season=="autumn"),"date"]+0.5


voles_data<-voles_data[order(voles_data$date),]


ax<- list(
  title = "",
  # titlefont = f1,
  showticklabels = TRUE,
  tickangle = 0
  # tickfont = f2,
  # exponentformat = "E"
)

ay<- list(
  title = "Voles Trapping Index",
  titlefont = "Open Sans",
  showticklabels = TRUE,
  tickangle = 270,
  # range = c(0.1, 0.6),
  tickfont = "Times New Roman"
  # exponentformat = "E"
)

p <- plot_ly(data = voles_data, x = ~date, y = ~popDensity,type = 'scatter', mode = 'lines+markers') %>%
  layout(xaxis = ax, yaxis = ay, showlegend = FALSE)
p




#### Model ----------



volesStats  <- function(x, obsData, ...){

  

  if (!is.matrix(x)) x <- matrix(x, 1, length(x))
  tx <- t(x)

  X0 <- t(orderDist(tx, obsData, np=3, diff=1)) ## cubic regs coeff of ordered diffs
  X0 <- cbind(X0, t(nlar(t(x),lag=c(6, 6, 6, 1, 1), power=c(1, 2, 3, 1, 2)))) ## least square coeff
  X0 <- cbind(X0, rowMeans(x)) # mean values of Y, # of 0's
  X0 <- cbind(X0, t(slAcf(tx, max.lag = 5)))  ## autocovariances up to lag 5 (the first element is the variance)
  X0 <- cbind(X0, apply( t( abs( apply( sign( apply(x,1,diff) ), 2, diff ) )), 1, sum)/2) #Number of turning points
  X0 <- cbind(X0, rowMedians(x)) # Mean-median
  
  
  
  r=character()
  for (i in 1:3) r[i]<-paste("cubic_",i,sep = "")
  for (i in 4:8) r[i]<-paste("ols_",i-3,sep = "")
  r[9]<-"mean"
  r[10]<-"variance"
  for (i in 11:15) r[i]<-paste("autocov_",i-10,sep = "")
  r[16]<-"turning"
  r[17]<-"mean_med"
  
  colnames(X0)<-r
  return(X0)
} 

apply( t( abs( apply( sign( apply(t(as.matrix(obsData)),1,diff) ), 2, diff ) )), 1, sum)/2

x=t(matrix(runif(360),90,4)+obsData)
y=data.frame(1:90)
y$obsData<-(voles_data$popDensity)

X1<-volesStats(x,obsData)
