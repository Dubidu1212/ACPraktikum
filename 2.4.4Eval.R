rm(list=ls()) 


library(data.table)
#File format is:
# V_1;pH_1;V0_1;m_1;V_2;pH_2;V0_2;m_2

data = read.csv("Data/2.4.4.csv",sep=";")

#We adjust the molarity of KOH to match the volume it would have had if it was 0.1 molar
KOH_factor = 0.891 


#TODO: correct for mass difference
Vol_1 = data$V_1 - data$V0_1[1]
Vol_2 = data$V_2- data$V0_2[1]
Vol_1 = Vol_1#*KOH_factor
Vol_2 = Vol_2#*KOH_factor
Vol_1
Vol_2

plot(Vol_1,data$pH_1, xlab = "Vol/ml", ylab = "pH", pch=3, ylim = c(0,14), xlim = c(0,max(c(Vol_1,Vol_2),na.rm = 1)), main = "Titration of an unknown organic acid with ~0.1M KOH", sub = "Titration factor 0.891")
points(Vol_2 ,data$pH_2, pch = 4,col = "red")

jmp_1 = 4.4
jmp_2 = 6.2

abline(v = jmp_1)
text(jmp_1 + 0.2,0,"EP",cex =.8)
abline(v = jmp_2,col = "red")
text(jmp_2+0.2,0,"EP",col = "red",cex =.8)

abline(v = jmp_1/2, h = 2.9, lty = 2)
text(jmp_1/2+0.2,0,"HEP",cex =.8)

abline(v = jmp_2/2, h = 3,col = "red", lty = 2)
text(jmp_2/2+0.2,0,"HEP",col = "red",cex =.8)

text(0, 3+.2,"pKa",cex =.8)


library(Hmisc)
minor.tick(nx=5, ny=2, tick.ratio=.5)
grid()

legend("topleft",legend = c("1: 0.104g", "2: 0.1489g"), title = "Sample", pch=3:4, col = c("black", "red"),cex = 0.8)


 print(data$m_1[1]/(jmp_1*KOH_factor*0.1)*1000)
 print(data$m_2[1]/(jmp_2*KOH_factor*0.1)*1000)

