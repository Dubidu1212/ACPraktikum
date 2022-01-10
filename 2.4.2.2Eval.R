rm(list=ls()) 
library(data.table)
#File format is:
# Volume;pH;Colour
# vol0;ph0;color0;
#
#Where vol0 is in ml and colour is p for pink and t for transparent

#You can create the file in excel and exporting it as csv (maybe you need to specify the delimiter ";")

#The file needs to be in the Folder Data and needs to be called 2.4.2.csv
data = read.csv("Data/2.4.2.2.csv",sep=";")
data = transpose(data)
colnames(data) = c('Volume','pH')

#first value that is different
change_vol = 25.9
#data before
data_p =data[data$Volume<change_vol,]
#data after
data_a =data[data$Volume>=change_vol,]

xp = data_p$Volume 
yp = data_p$pH
plot(xp,yp,xlab='Vol/mL', ylab = 'pH', pch=4, ylim = c(1,14),col = 'blue', main = "Titration of an unknown HCL solution using an approx. 0.1M KOH solution", sub = "c(KOH) = 0.0891M")


library(Hmisc)
minor.tick(nx=10, ny=2, tick.ratio=.5)
grid()

xa = data_a$Volume
ya = data_a$pH
points(xa,ya,pch=3, col = 'tomato')

legend("topleft", legend=c("pink", "transparent"), title = "Color of indicator (Phenolphtalein)",
       col=c("red", "blue"), pch=3:4, cex=0.8)

# drm(ya ~ xa, data = data, fct = L.3(), type = "continuous")
# library(drc)
# lines(drm)
