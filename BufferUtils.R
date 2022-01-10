#Buffer utils

# A0 = 1
# HA0 = 1
# pKa = 4.5

buffer = c("AC", "PO", "tris")
pH = c(5.75,7.21,7.12)
pKa = c(4.75,7.21,8.12)
#in Mol/L
HA = c(0.009091,0.05,0.09091)
A = c(0.09091,0.05,0.009091)
data = data.frame(buffer, pH,pKa,HA,A, row.names = buffer)


get_pH = function(A,HA,pKa){
  return(pKa + log(A/HA,10))
}
get_A = function(aTot, pH, pKa){
  c = 10^(pH-pKa)
  return(aTot*c/(1+c))
}
get_buffer_region = function(pH0, pKa, A, HA,bufferName = "n", bufferDelta){
  cp = 10^(pH0+bufferDelta-pKa)
  bufferCap = (cp*HA - A)/(cp+1)
  return(data.frame(bufferName, bufferCap))
}
#buffer Regions in delta Mol/L
bufferRegionsAcid = get_buffer_region(data$pH,data$pKa, data$A, data$HA,bufferName = data$buffer,bufferDelta = -1)
bufferRegionsAcid$bufferCap = bufferRegionsAcid$bufferCap * -1

bufferRegionsBase = get_buffer_region(data$pH,data$pKa, data$A, data$HA,bufferName = data$buffer,bufferDelta = 1)

#calculate back to pH
#default
get_pH(data$A,data$HA,data$pKa)
#acidic
get_pH(data$A - bufferRegionsAcid$bufferCap,data$HA + bufferRegionsAcid$bufferCap,data$pKa)
#basic
get_pH(data$A + bufferRegionsBase$bufferCap,data$HA - bufferRegionsBase$bufferCap,data$pKa)

#bufferRegions in delta mmol
sampleSize = 5 #in mL
bufferRegionsAcid$bufferCap = sampleSize *bufferRegionsAcid$bufferCap
bufferRegionsBase$bufferCap = sampleSize *bufferRegionsBase$bufferCap

#Calculate in mL
#concentrations in Mol/L
ConcentrationBase = 0.00891
ConcentrationAcid = 0.01

bufferRegionsAcid$bufferCap = bufferRegionsAcid$bufferCap/ConcentrationAcid
bufferRegionsBase$bufferCap = bufferRegionsBase$bufferCap/ConcentrationBase
bufferRegionsAcid
bufferRegionsBase


