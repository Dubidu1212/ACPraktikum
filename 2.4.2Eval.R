data = read.csv("Data/2.4.2",sep=";")
x = data$Volume
y = data$pH
plot(x,y,pch=3, xlab="Vol/mL", ylab="pH")

