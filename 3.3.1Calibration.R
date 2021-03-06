#Calibration of Potentiometric Titration
#Data format: 
#V;E;Sol
#Important. The data is formated like this:
#V is the amount of volume added of the solution (a or b)
#Sol is a or b for the solution
#E is the cell potential in mV


data = read.csv("Data/PotentiometricCal.csv",sep=";")
xa = data$V[data$Sol == 'a']
ya = data$E[data$Sol == 'a']


xb = data$V[data$Sol == 'b']
yb = data$E[data$Sol == 'b']



#calculate the required values
caAg = 0.01
cbAg = 0.1

#added Ag in mmol
xa = cumsum(xa) * caAg
xb = cumsum(xb) * cbAg + max(xa)

totV = cumsum(data$V) + 100
x = c(xa,xb)
concentration = x/totV
pAg = -log(concentration,10)
res = data.frame(totV, x, concentration, pAg, data$E)
res


plot(data$E,pAg, ylim = c(0,10), xlim = c(-100,400), xlab = expression(Delta * "E/mV"), ylab = "pAg", main = "Calibration curve of the Ag Electrode")
grid()

y = -log(concentration[concentration != 0],10)
x = data$E[data$V != 0]
abline(lm(y~x))
line = coef(lm(y~x))
xIntercept = solve(line[2], -line[1]);
abline(v= xIntercept, h = 0, lty = 2)

xIntercept
#slope
line[2]
text(200,7,paste("slope y/x:",signif(line[2],3)))
text(200,6.5,paste("slope x/y:",signif(1/line[2],3)))
text(200,6,paste("x intercept:",signif(xIntercept,3), "mV"))

