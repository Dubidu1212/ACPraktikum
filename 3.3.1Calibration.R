#Calibration of Potentiometric Titration
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


plot(y = data$E,x = pAg, ylim = c(0,10), xlim = c(-100,400), xlab = expression(Delta * "E/mV"), ylab = "pAg", main = "Calibration curve of the Ag Electrode")
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
text(200,7,paste("slope y/x:",round(line[2],5)))
text(200,6,paste("x intercept:",round(xIntercept,5), "mV"))

