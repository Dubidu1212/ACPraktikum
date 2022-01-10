rm(list=ls()) 
data = read.csv("Data/PotentiometricTitration.csv",sep=";")
x = data$Volume
y = data$Voltage
#y=log(y,10)

get_pAg_from_mV = function(mV, E0 = 364.242, s = -0.01585){
  return((mV-E0)*(s)) 
}









plot(x,y,xlab="Vol/ml 0.1M KCl", ylab = expression(Delta * "E/mV"),pch=4, main = "Potentiometric Titration of Ag using Cl", sub = "The titrated soltuion was: 50mL 2M KNO3, 10mL 0.1M HNO3, 2mL 0.1M AgNO3 filled up to 100mL ")


y = log(y,10)
plot(x,y,xlab="Vol/ml 0.1M KCl", ylab = expression(log(Delta * "E/mV")),pch=4, main = "Potentiometric Titration of Ag using Cl", sub = "The titrated soltuion was: 50mL 2M KNO3, 10mL 0.1M HNO3, 2mL 0.1M AgNO3 filled up to 100mL ")

#linear approximations for the two regions
cutoff= 1.85
xpre = data$Volume[data$Volume<cutoff]
ypre = log(data$Voltage[data$Volume<cutoff],10)
abline(lm(ypre~xpre))

xpost = data$Volume[data$Volume>=cutoff]
ypost = log(data$Voltage[data$Volume>=cutoff],10)
abline(lm(ypost~xpost))


#abline(v = cutoff)