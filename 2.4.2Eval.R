rm(list=ls()) 

#File format is:
# Volume;pH;Colour
# vol0;ph0;color0;
#
#Where vol0 is in ml and colour is p for pink and t for transparent

#You can create the file in excel and exporting it as csv (maybe you need to specify the delimiter ";")

#The file needs to be in the Folder Data and needs to be called 2.4.2.csv
data = read.csv("Data/2.4.2.csv",sep=";")
datap = data[which(data$Color == 'p'),]
xp = datap$Volume
yp = datap$pH

datat = data[which(data$Color == 't'),]
xt = datat$Volume
yt = datat$pH
for(val in datat){
  print(val)
}

#Create the plot for the pink entries
#To change the symbol change the number of pch (make sure to also eddit the legend accordingly)
plot(xp,yp,pch=3, xlab="Vol/mL", ylab="pH", ylim = c(0,14), xlim = c(0,30), col = "red", main = "Determination of the concentration of a KOH titration solution",sub ="Titration done with 0.1 M HCL")
#Add the transparent entries
points(xt,yt, pch = 4, col = "blue")
grid()
library(Hmisc)
minor.tick(nx=5,ny=2, tick.ratio=.5)

#Adds the legend
legend("bottomleft", legend=c("pink", "transparent"), title = "Color of indicator (Phenolphtalein)",
       col=c("red", "blue"), pch=3:4, cex=0.8)


#I make no guarantees for the following part...
#Analyzing the steepness
steep_x = diff(data$Volume)
steep_y = diff(data$pH)
steep = steep_y/steep_x
steep = steep/(max(steep)-min(steep))
steep = (steep+1)*10

running_avg_2 = vector() 
for(x in c(2:length(data$Volume))){
  running_avg_2[x-1] =  (data$Volume[x-1] + data$Volume[x])/2
}

#points(running_avg_2,steep, pch = 1)

#find steepest part of the curve
vol_eq = running_avg_2[which(steep == min(steep))]
vol_eq = vol_eq - 0.15
#mark the steepest part of the curve
abline(v = vol_eq)
axis(1, at=vol_eq,labels=vol_eq)

# #mark half equivalence point
# vol_heq = round(vol_eq/2,1)
# abline(v = vol_heq)
# axis(1, at=vol_heq,labels=vol_heq)

#0.898 mol/litre

















