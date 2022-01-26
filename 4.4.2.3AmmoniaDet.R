#Ammonia determination

#mass A in g
m_A = 0.1619
m_B = 0.150
m = c(m_A,m_B)

#ml 0.1 M HCL used
V_HCl_A = 33
V_HCl_B = 33.9
V_HCl = c(V_HCl_A,V_HCl_B)

#mol HCl titrated
n_HCl = V_HCl/1000 * 0.1

M_NH3 = 17.031
M_N = 14

#gramms of N/ NH3
m_NH3_tot = n_HCl*M_NH3
m_N_tot = n_HCl*M_N

#Mass percent
p_N = m_N_tot/m * 100
p_NH3 = m_NH3_tot/m * 100

type = c("A","B")

data = data.frame(m, n_HCl, m_NH3_tot, m_N_tot, p_N, p_NH3, type)
data

#mol complex per gram N
n_HCl/(p_N/100*m)



#Addidtional calculations to check for correctness
p_Co = 23.76
p_Cl = 100 - (p_Co + p_NH3)

M_tot = 248.07

#amount of cloride per Complex
M_Cl = M_tot * p_Cl/100
#number of Cl atoms per complex
M_Cl/35

#Molar fraction
n_Co =c( 0.004031117, 0.003509306) 
n_N = n_HCl/m
n_N/n_Co





