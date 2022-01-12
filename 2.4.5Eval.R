rm(list=ls()) 


det_plot_bounds = function(vecvec){
  maxmax = -Inf
  minmin = Inf
  for(vec in vecvec){
    maxmax= max(max(vec,na.rm = 1), maxmax)
    minmin= min(min(vec,na.rm = 1), minmin)
  }
  return(c(minmin,maxmax))
}

library(data.table)
#File format is:
# V_KOH;pH_KOH;VO_KOH;V_HCL:pH_HCL;V0_HCL
#


#The file needs to be in the Folder Data and needs to be called 2.4.2.csv
data = read.csv("Data/2.4.5_sol3.csv",sep=";")

x_KOH = data$V_KOH-data$V0_KOH[1]
y_KOH = data$pH_KOH
#We adjust the molarity of KOH to match the volume it would have had if it was 0.1 molar
KOH_factor = 0.891 
x_KOH = x_KOH*KOH_factor


x_HCL = -(data$V_HCL-data$V0_HCL[1])
y_HCL = data$pH_HCL
HCL_max = max(x_HCL,na.rm = 1)
HCL_min = min(x_HCL,na.rm = 1)

KOH_max = max(x_KOH,na.rm = 1)
KOH_min = min(x_KOH,na.rm = 1)

tot_max = max(KOH_max,HCL_max)
tot_min = min(KOH_min,HCL_min)

plot(x_KOH,y_KOH,xlab='Vol/mL', ylab = 'pH', pch=4, ylim = c(y_KOH[1]-4,y_KOH[1]+4),col = 'blue',xlim = c(tot_min,tot_max), main = "Buffer capacity of tris buffer", sub = "Volume adjusted to 0.01M acid/base")

points(x_HCL,y_HCL, col = 'tomato')

polygon(c(-100,-100,100,100),c(y_KOH[1]+1, y_KOH[1]-1,y_KOH[1]-1, y_KOH[1]+1), col = rgb(0,0,1,.2),border = NA)

library(Hmisc)
minor.tick(nx=10, ny=2, tick.ratio=.5)
grid()
