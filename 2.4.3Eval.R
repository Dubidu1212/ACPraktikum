rm(list=ls()) 
library(data.table)
#File format is:
# Volume;pH;Colour
# vol0;ph0;color0;
#
#Where vol0 is in ml and colour is p for pink and t for transparent

#You can create the file in excel and exporting it as csv (maybe you need to specify the delimiter ";")

#The file needs to be in the Folder Data and needs to be called 2.4.2.csv
data = read.csv("Data/2.4.3.csv",sep=";")

x = data$Volume
y = data$pH

plot(x,y,xlab='Vol/mL', ylab = 'pH', pch=4, ylim = c(0,12),col = 'black', main = "Titration of Acetic Acid using ~0.1M KOH", sub = "c(KOH) = 0.0891M")

#Equivalence Point
abline(v = 36)
text(36 +1,0, "EP", cex = .8)
#Half Equivalence Point
abline(v = 36/2,h = 4.6, lty = 2:1)
text(36/2 +1,0, "HEP", cex = .8)

library(Hmisc)
minor.tick(nx=2, ny=2, tick.ratio=.5)
minor.tick(nx=10, ny=10, tick.ratio=.2)
grid()

