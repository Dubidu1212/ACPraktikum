rm(list=ls())
#4.4.2.4 CL
data = read.csv("Data/4.4.2.4Cl_A.csv",sep=";")
data$V_AgNO3 = data$V_AgNO3 - data$V0[1]

c_AgNO3 = 0.1
data$n_AgNO3 = data$V_AgNO3 * c_AgNO3

Conductivity = data$Conductivity

plot(data$n_AgNO3,Conductivity, xlab = "Added AgNO3/ mmol",ylab = expression(Lambda/"mS cm"^{-1}) , pch =3, main = "Conductometric Titration of 0.002M Complex A with 0.1M AgNO3")
grid()

#separate data into pre and post EP

EP_cutoff = 0.4
#EP_cutoff = 0.575

#abline(v = EP_cutoff, col = "red", lty = 3)

data_pre = data[data$n_AgNO3 < EP_cutoff,]
data_post = data[data$n_AgNO3 >= EP_cutoff,]

lm_pre = lm(data_pre$Conductivity~ data_pre$n_AgNO3)
lm_post = lm(data_post$Conductivity~ data_post$n_AgNO3)

abline(coef(lm_pre))
abline(coef(lm_post))

lm_to_function = function(linear_model){
  coefs = coef(linear_model)
  return(function(x){
      coefs[1] + coefs[2]*x 
  }) 
}

intersection = uniroot(function(x){lm_to_function(lm_pre)(x) - lm_to_function(lm_post)(x)}, interval = c(min(data$n_AgNO3),max(data$n_AgNO3)))
abline(v = intersection$root, lty = 2, col = "turquoise")
text(intersection$root , 1.5, paste("AgNO3:",signif(intersection$root, 2), "mmol"))


#Calculate mass percent of
M_Cl = 35.453

#Total mass of Cl in sample
m_Cl = M_Cl * intersection$root/1000

#Total mass of sample
m_A = 0.0506
m_B = 0.0496

p_Cl = m_Cl/m_B*100













