rm(list=ls()) 
#Converts pH to H3O+ and OH-
data = read.csv("Data/2.4.1.csv",sep=";")
pH = data$pH
print(10^(-pH))
print(10^(-(14-pH)))