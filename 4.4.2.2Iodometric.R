library(tidyverse)
#Iodometric Cobalt analysis 4.4.2.2

#A
  #in g
  w_A = c(0.101,0.0969,0.104)
  #in mL
  VI_A = c(40.6,39.1,42)
  #in M
  c_thiosulfate = 0.01
  
  #Moles of I-
  n_I_A = VI_A/1000 * c_thiosulfate
  
  M_Co = 58.933
  
  #Grams of cobalt
  m_Co_A = M_Co * n_I_A
  
  #Mass percent
  p_Co_A = 100*m_Co_A/ w_A
  
  #Mol Cobalt per gram substance
  m_g_Co_A = n_I_A/w_A
  
  data_A = data.frame(w_A, VI_A, m_Co_A, p_Co_A, m_g_Co_A)
 
  print("Data A") 
  mean(data_A$p_Co_A)
  sd(data_A$p_Co_A)
  
  mean(data_A$m_g_Co_A)
  sd(data_A$m_g_Co_A)
  
  #export data
  write.table(data_A,file="./Data/Iod_A.csv",sep=";",dec = ".")
#B
  w_B =  c(0.111,0.101,0.102)
  #in mL
  #Note data point 2 is not accurate as CO2 ran out
  VI_B = c(33.5,40.5,35.7)
 
  # w_B = w_B[-2] 
  # VI_B = VI_B[-2] 
  
  #Moles of I-
  n_I_B = VI_B/1000 * c_thiosulfate
  
  M_Co = 58.933
  
  #Grams of cobalt
  m_Co_B = M_Co * n_I_B
  
  #Mass percent
  p_Co_B = 100*m_Co_B/ w_B
  
  #Mol Cobalt per gram substance
  m_g_Co_B = n_I_B/w_B
  
  data_B = data.frame(w_B, VI_B, m_Co_B, p_Co_B, m_g_Co_B)
 
  print("Data B") 
  mean(data_B$p_Co_B)
  sd(data_B$p_Co_B)
  
  mean(data_B$m_g_Co_B)
  sd(data_B$m_g_Co_B)
  
  #export data
  write.table(data_B,file="./Data/Iod_B.csv",sep=";",dec = ".")
  
  #TODO: check if data makes sense by calculating Cl 