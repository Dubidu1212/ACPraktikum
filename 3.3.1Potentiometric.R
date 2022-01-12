rm(list=ls()) 
library(pracma)

data = read.csv("Data/PotentiometricTitration.csv",sep=";")
x = data$Volume
VolKCl = x
y = data$Voltage
Voltage = y
#y=log(y,10)

get_pAg_from_mV = function(mV, E0 = 364.242, s = -0.01585){
  return((mV-E0)*(s)) 
}

Vol0 = 100
VolTot = Vol0 + x
pAg = get_pAg_from_mV(Voltage)
Ag = 10^(-pAg)



#Round the data
pAg = signif(pAg, 3)
Ag = signif(Ag, 3)
tab = data.frame(VolKCl,VolTot,Voltage,pAg,Ag)

plot(x,y,xlab="Vol/ml 0.1M KCl", ylab = expression(Delta * "E/mV"),pch=4, main = "Potentiometric Titration of Ag using Cl", sub = "The titrated soltuion was: 50mL 2M KNO3, 10mL 0.1M HNO3, 2mL 0.1M AgNO3 filled up to 100mL ")

spline = smooth.spline(x, y, spar=0.35)
lines(spline, col = "lightgray")

#Tangent Utils
#returns data frame with a and b where a is the y intercept and b the slope
get_tangent = function(spline, xPos){
  pred0 = predict(spline, x=xPos, deriv=0)
  pred1 = predict(spline, x=xPos, deriv=1)
  
  b = pred1$y 
  a = pred0$y - b*xPos
  return(data.frame(a,b))
}
find_next_point_tangent = function(spline,xPos, slope , direction = 1, step_size = 0.1,accuracy = 0.1, max_dist = 10){
  currPos = xPos
  while(1){
    if(abs(xPos-currPos) > max_dist){
      print("Error max dist elapsed with no tangent")
      return(NaN)
    }
    currPos = currPos + step_size 
    currTan = get_tangent(spline, currPos)
    if(currTan$b > slope-accuracy && currTan$b < slope+accuracy){
      tangent = currTan
      xPos = currPos
      return(data.frame(tangent, xPos))
    } 
  }
}


#Add Tangents
tangent_point = 1.7
tan1 = get_tangent(spline,tangent_point)
abline(a = tan1$a,b = tan1$b, col = "lightblue", lty = 2)
tan2 = find_next_point_tangent(spline, tangent_point, tan1$b, step_size = 0.1, accuracy = 1)
abline(a = tan2$a,b = tan2$b, col = "lightblue", lty = 2)

#Add half-distance point
abline(a = (tan1$a + tan2$a)/2, b = tan1$b, col = "lightblue", lty = 4)

#find intersection
closeness = c()
probe_seq = seq(from = tangent_point, to = tan2$xPos, by = 0.01)
for(s in probe_seq){
    closeness = c(closeness, predict(spline,s)$y - ((tan1$a + tan2$a)/2 + tan1$b*s))
}
closeness = abs(closeness)
xIntersect = probe_seq[which.min(closeness)]
yIntersect = predict(spline, xIntersect)$y
abline(h = yIntersect, lty = 2)
yIntersect
text(0.1,100,paste(signif(yIntersect, 3), "mV"))



#LogPlot
# y = log(y,10)
# plot(x,y,xlab="Vol/ml 0.1M KCl", ylab = expression(log(Delta * "E/mV")),pch=4, main = "Potentiometric Titration of Ag using Cl", sub = "The titrated soltuion was: 50mL 2M KNO3, 10mL 0.1M HNO3, 2mL 0.1M AgNO3 filled up to 100mL ")
# 
# #linear approximations for the two regions
# cutoff= 1.85
# xpre = data$Volume[data$Volume<cutoff]
# ypre = log(data$Voltage[data$Volume<cutoff],10)
# abline(lm(ypre~xpre))
# 
# xpost = data$Volume[data$Volume>=cutoff]
# ypost = log(data$Voltage[data$Volume>=cutoff],10)
# abline(lm(ypost~xpost))
#abline(v = cutoff)

