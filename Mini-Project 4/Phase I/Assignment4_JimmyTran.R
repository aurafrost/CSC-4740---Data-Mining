# Jimmy Tran
# 0395
# Data Mining CSC 4740

# install and require necessary libraries
library(arules)
require(arules)
library(arulesViz)
require(arulesViz)

# headers
new_header <- c("ID", "Age:18-24", "Age:25-34", "Age:35-44", "Age:45-54", "Age:55-64", "Age:65+", "Female", "Male", 
                "Before 16", "At 16", "At 17", "At 18", "Some College", "Professional Certificate",
                "University Degree", "Masters Degree", "Doctorate Degree", "Australia", "Canada",
                "New Zealand", "Other", "Republic of Ireland", "UK", "USA", "Asian", "Black",
                "Mixed-Black/Asian", "Mixed-White/Asian", "Mixed-White/Black", "Other", "White", 
                "Nscore Very Low", "Nscore Low", "Nscore High", "Nscore Very High", 
                "Escore Very Low", "Escore Low", "Escore High", "Escore Very High",
                "Oscore Very Low", "Oscore Low", "Oscore High", "Oscore Very High",
                "Ascore Very Low", "Ascore Low", "Ascore High", "Ascore Very High",
                "Cscore Very Low", "Cscore Low", "Cscore High", "Cscore Very High",
                "Impulsive Very Low", "Impulsive Low", "Impulsive High", "Impulsive Very High",
                "SS Very Low", "SS Low", "SS High", "SS Very High",
                "Alcohol CL0", "Alcohol CL1", "Alcohol CL2", "Alcohol CL3", "Alcohol CL4",
                "Alcohol CL5", "Alcohol CL6",
                "Amphet CL0", "Amphet CL1", "Amphet CL2", "Amphet CL3", "Amphet CL4",
                "Amphet CL5", "Amphet CL6",
                "Amyl CL0", "Amyl CL1", "Amyl CL2", "Amyl CL3", "Amyl CL4", "Amyl CL5", "Amyl CL6",
                "Benzos CL0", "Benzos CL1", "Benzos CL2", "Benzos CL3", "Benzos CL4", "Benzos CL5",
                "Benzos CL6",
                "Caff CL0", "Caff CL1", "Caff CL2", "Caff CL3", "Caff CL4", "Caff CL5", "Caff CL6",
                "Cannabis CL0", "Cannabis CL1", "Cannabis CL2", "Cannabis CL3", "Cannabis CL4",
                "Cannabis CL5", "Cannabis CL6",
                "Choc CL0", "Choc CL1", "Choc CL2", "Choc CL3", "Choc CL4", "Choc CL5", "Choc CL6",
                "Coke CL0", "Coke CL1", "Coke CL2", "Coke CL3", "Coke CL4", "Coke CL5", "Coke CL6",
                "Crack CL0", "Crack CL1", "Crack CL2", "Crack CL3", "Crack CL4", "Crack CL5",
                "Crack CL6",
                "Ecstasy CL0", "Ecstasy CL1", "Ecstasy CL2", "Ecstasy CL3", "Ecstasy CL4", 
                "Ecstasy CL5", "Ecstasy CL6",
                "Heroin CL0", "Heroin CL1", "Heroin CL2", "Heroin CL3", "Heroin CL4", 
                "Heroin CL5", "Heroin CL6",
                "Ketamine CL0", "Ketamine CL1", "Ketamine CL2", "Ketamine CL3", "Ketamine CL4", 
                "Ketamine CL5", "Ketamine CL6",
                "Legalh CL0", "Legalh CL1", "Legalh CL2", "Legalh CL3", "Legalh CL4", "Legalh CL5",
                "Legalh CL6",
                "LSD CL0", "LSD CL1", "LSD CL2", "LSD CL3", "LSD CL4", "LSD CL5", "LSD CL6",
                "Meth CL0", "Meth CL1", "Meth CL2", "Meth CL3", "Meth CL4", "Meth CL5", "Meth CL6",
                "Mushrooms CL0", "Mushrooms CL1", "Mushrooms CL2", "Mushrooms CL3", "Mushrooms CL4",
                "Mushrooms CL5", "Mushrooms CL6",
                "Nicotine CL0", "Nicotine CL1", "Nicotine CL2", "Nicotine CL3", "Nicotine CL4", 
                "Nicotine CL5", "Nicotine CL6",
                "Semer CL0", "Semer CL1", "Semer CL2", "Semer CL3", "Semer CL4", "Semer CL5", 
                "Semer CL6",
                "VSA CL0", "VSA CL1", "VSA CL2", "VSA CL3", "VSA CL4", "VSA CL5", "VSA CL6")

# data import
# data is the table for the original data from the file
data<-read.csv("drug_consumption.csv", header=FALSE)

# helper functions
# Score functions
# "s" is added as a prefix to distinguish from the impulsive and ss functions
svlow<-function(value){
  if(value<(-1.73)) return(1)
  else return(0)}
slow<-function(value){
  if(value<0 & value>=-1.73) return(1)
  else return(0)}
shigh<-function(value){
  if(value<1.73 & value>=0) return(1)
  else return(0)}
svhigh<-function(value){
  if(value>=1.73) return(1)
  else return(0)}
# impulsive and ss functions
# "is" is added as a prefix to distinguish from the score functions
isvlow<-function(value){
  if(value<(-1.5)) return(1)
  else return(0)}
islow<-function(value){
  if(value<0 & value>=-1.5) return(1)
  else return(0)}
ishigh<-function(value){
  if(value<1.5 & value>=0) return(1)
  else return(0)}
isvhigh<-function(value){
  if(value>=1.5) return(1)
  else return(0)}

# new table dimensions: 1885 rows x 193 columns
# table is the new table of data
table<-data.frame(matrix(ncol=193, nrow=1885)) # create empty table
colnames(table)<-paste0(new_header) # add new headers

# translate data from "data" to "table"
# comments below are in the format: data column number, data column name
# 1 id
table[[1]]<-data[[1]]
# 2 age
table[[2]]<-as.numeric(data[[2]]=="-0.95197")
table[[3]]<-as.numeric(data[[2]]=="-0.07854")
table[[4]]<-as.numeric(data[[2]]=="0.49788")
table[[5]]<-as.numeric(data[[2]]=="1.09449")
table[[6]]<-as.numeric(data[[2]]=="1.82213")
table[[7]]<-as.numeric(data[[2]]=="2.59171")
# 3 gender
table[[8]]<-as.numeric(data[[3]]=="0.48246")
table[[9]]<-as.numeric(data[[3]]=="-0.48246")
# 4 education
table[[10]]<-as.numeric(data[[4]]=="-2.43591")
table[[11]]<-as.numeric(data[[4]]=="-1.73790")
table[[12]]<-as.numeric(data[[4]]=="-1.43719")
table[[13]]<-as.numeric(data[[4]]=="-1.22751")
table[[14]]<-as.numeric(data[[4]]=="-0.61113")
table[[15]]<-as.numeric(data[[4]]=="-0.05921")
table[[16]]<-as.numeric(data[[4]]=="0.45468")
table[[17]]<-as.numeric(data[[4]]=="1.16365")
table[[18]]<-as.numeric(data[[4]]=="1.98437")
# 5 country
table[[19]]<-as.numeric(data[[5]]=="-0.09765")
table[[20]]<-as.numeric(data[[5]]=="0.24923")
table[[21]]<-as.numeric(data[[5]]=="-0.46841")
table[[22]]<-as.numeric(data[[5]]=="-0.28519")
table[[23]]<-as.numeric(data[[5]]=="0.21128")
table[[24]]<-as.numeric(data[[5]]=="0.96082")
table[[25]]<-as.numeric(data[[5]]=="-0.57009")
# 6 ethnicity
table[[26]]<-as.numeric(data[[6]]=="-0.50212")
table[[27]]<-as.numeric(data[[6]]=="-1.10702")
table[[28]]<-as.numeric(data[[6]]=="1.90725")
table[[29]]<-as.numeric(data[[6]]=="0.12600")
table[[30]]<-as.numeric(data[[6]]=="-0.22166")
table[[31]]<-as.numeric(data[[6]]=="0.11440")
table[[32]]<-as.numeric(data[[6]]=="-0.31685")
# 7 Nscore
for(i in 1:1885){table[i,33]<-svlow(data[i,7])}
for(i in 1:1885){table[i,34]<-slow(data[i,7])}
for(i in 1:1885){table[i,35]<-shigh(data[i,7])}
for(i in 1:1885){table[i,36]<-svhigh(data[i,7])}
# 8 Escore
for(i in 1:1885){table[i,37]<-svlow(data[i,8])}
for(i in 1:1885){table[i,38]<-slow(data[i,8])}
for(i in 1:1885){table[i,39]<-shigh(data[i,8])}
for(i in 1:1885){table[i,40]<-svhigh(data[i,8])}
# 9 Oscore
for(i in 1:1885){table[i,41]<-svlow(data[i,9])}
for(i in 1:1885){table[i,42]<-slow(data[i,9])}
for(i in 1:1885){table[i,43]<-shigh(data[i,9])}
for(i in 1:1885){table[i,44]<-svhigh(data[i,9])}
# 10 Ascore
for(i in 1:1885){table[i,45]<-svlow(data[i,10])}
for(i in 1:1885){table[i,46]<-slow(data[i,10])}
for(i in 1:1885){table[i,47]<-shigh(data[i,10])}
for(i in 1:1885){table[i,48]<-svhigh(data[i,10])}
# 11 Cscore
for(i in 1:1885){table[i,49]<-svlow(data[i,11])}
for(i in 1:1885){table[i,50]<-slow(data[i,11])}
for(i in 1:1885){table[i,51]<-shigh(data[i,11])}
for(i in 1:1885){table[i,52]<-svhigh(data[i,11])}
# 12 Impulsive
for(i in 1:1885){table[i,53]<-isvlow(data[i,12])}
for(i in 1:1885){table[i,54]<-islow(data[i,12])}
for(i in 1:1885){table[i,55]<-ishigh(data[i,12])}
for(i in 1:1885){table[i,56]<-isvhigh(data[i,12])}
# 13 SS
for(i in 1:1885){table[i,57]<-isvlow(data[i,13])}
for(i in 1:1885){table[i,58]<-islow(data[i,13])}
for(i in 1:1885){table[i,59]<-ishigh(data[i,13])}
for(i in 1:1885){table[i,60]<-isvhigh(data[i,13])}
# 14 Alcohol
table[[61]]<-as.numeric(data[[14]]=="CL0")
table[[62]]<-as.numeric(data[[14]]=="CL1")
table[[63]]<-as.numeric(data[[14]]=="CL2")
table[[64]]<-as.numeric(data[[14]]=="CL3")
table[[65]]<-as.numeric(data[[14]]=="CL4")
table[[66]]<-as.numeric(data[[14]]=="CL5")
table[[67]]<-as.numeric(data[[14]]=="CL6")
# 15 Amphet
table[[68]]<-as.numeric(data[[15]]=="CL0")
table[[69]]<-as.numeric(data[[15]]=="CL1")
table[[70]]<-as.numeric(data[[15]]=="CL2")
table[[71]]<-as.numeric(data[[15]]=="CL3")
table[[72]]<-as.numeric(data[[15]]=="CL4")
table[[73]]<-as.numeric(data[[15]]=="CL5")
table[[74]]<-as.numeric(data[[15]]=="CL6")
# 16 Amyl
table[[75]]<-as.numeric(data[[16]]=="CL0")
table[[76]]<-as.numeric(data[[16]]=="CL1")
table[[77]]<-as.numeric(data[[16]]=="CL2")
table[[78]]<-as.numeric(data[[16]]=="CL3")
table[[79]]<-as.numeric(data[[16]]=="CL4")
table[[80]]<-as.numeric(data[[16]]=="CL5")
table[[81]]<-as.numeric(data[[16]]=="CL6")
# 17 Benzos
table[[82]]<-as.numeric(data[[17]]=="CL0")
table[[83]]<-as.numeric(data[[17]]=="CL1")
table[[84]]<-as.numeric(data[[17]]=="CL2")
table[[85]]<-as.numeric(data[[17]]=="CL3")
table[[86]]<-as.numeric(data[[17]]=="CL4")
table[[87]]<-as.numeric(data[[17]]=="CL5")
table[[88]]<-as.numeric(data[[17]]=="CL6")
# 18 Caff
table[[89]]<-as.numeric(data[[18]]=="CL0")
table[[90]]<-as.numeric(data[[18]]=="CL1")
table[[91]]<-as.numeric(data[[18]]=="CL2")
table[[92]]<-as.numeric(data[[18]]=="CL3")
table[[93]]<-as.numeric(data[[18]]=="CL4")
table[[94]]<-as.numeric(data[[18]]=="CL5")
table[[95]]<-as.numeric(data[[18]]=="CL6")
# 19 Cannabis
table[[96]]<-as.numeric(data[[19]]=="CL0")
table[[97]]<-as.numeric(data[[19]]=="CL1")
table[[98]]<-as.numeric(data[[19]]=="CL2")
table[[99]]<-as.numeric(data[[19]]=="CL3")
table[[100]]<-as.numeric(data[[19]]=="CL4")
table[[101]]<-as.numeric(data[[19]]=="CL5")
table[[102]]<-as.numeric(data[[19]]=="CL6")
# 20 Choc
table[[103]]<-as.numeric(data[[20]]=="CL0")
table[[104]]<-as.numeric(data[[20]]=="CL1")
table[[105]]<-as.numeric(data[[20]]=="CL2")
table[[106]]<-as.numeric(data[[20]]=="CL3")
table[[107]]<-as.numeric(data[[20]]=="CL4")
table[[108]]<-as.numeric(data[[20]]=="CL5")
table[[109]]<-as.numeric(data[[20]]=="CL6")
# 21 Coke
table[[110]]<-as.numeric(data[[21]]=="CL0")
table[[111]]<-as.numeric(data[[21]]=="CL1")
table[[112]]<-as.numeric(data[[21]]=="CL2")
table[[113]]<-as.numeric(data[[21]]=="CL3")
table[[114]]<-as.numeric(data[[21]]=="CL4")
table[[115]]<-as.numeric(data[[21]]=="CL5")
table[[116]]<-as.numeric(data[[21]]=="CL6")
# 22 Crack
table[[117]]<-as.numeric(data[[22]]=="CL0")
table[[118]]<-as.numeric(data[[22]]=="CL1")
table[[119]]<-as.numeric(data[[22]]=="CL2")
table[[120]]<-as.numeric(data[[22]]=="CL3")
table[[121]]<-as.numeric(data[[22]]=="CL4")
table[[122]]<-as.numeric(data[[22]]=="CL5")
table[[123]]<-as.numeric(data[[22]]=="CL6")
# 23 Ecstasy
table[[124]]<-as.numeric(data[[23]]=="CL0")
table[[125]]<-as.numeric(data[[23]]=="CL1")
table[[126]]<-as.numeric(data[[23]]=="CL2")
table[[127]]<-as.numeric(data[[23]]=="CL3")
table[[128]]<-as.numeric(data[[23]]=="CL4")
table[[129]]<-as.numeric(data[[23]]=="CL5")
table[[130]]<-as.numeric(data[[23]]=="CL6")
# 24 Heroin
table[[131]]<-as.numeric(data[[24]]=="CL0")
table[[132]]<-as.numeric(data[[24]]=="CL1")
table[[133]]<-as.numeric(data[[24]]=="CL2")
table[[134]]<-as.numeric(data[[24]]=="CL3")
table[[135]]<-as.numeric(data[[24]]=="CL4")
table[[136]]<-as.numeric(data[[24]]=="CL5")
table[[137]]<-as.numeric(data[[24]]=="CL6")
# 25 Ketamine
table[[138]]<-as.numeric(data[[25]]=="CL0")
table[[139]]<-as.numeric(data[[25]]=="CL1")
table[[140]]<-as.numeric(data[[25]]=="CL2")
table[[141]]<-as.numeric(data[[25]]=="CL3")
table[[142]]<-as.numeric(data[[25]]=="CL4")
table[[143]]<-as.numeric(data[[25]]=="CL5")
table[[144]]<-as.numeric(data[[25]]=="CL6")
# 26 Legalh
table[[145]]<-as.numeric(data[[26]]=="CL0")
table[[146]]<-as.numeric(data[[26]]=="CL1")
table[[147]]<-as.numeric(data[[26]]=="CL2")
table[[148]]<-as.numeric(data[[26]]=="CL3")
table[[149]]<-as.numeric(data[[26]]=="CL4")
table[[150]]<-as.numeric(data[[26]]=="CL5")
table[[151]]<-as.numeric(data[[26]]=="CL6")
# 27 LSD
table[[152]]<-as.numeric(data[[27]]=="CL0")
table[[153]]<-as.numeric(data[[27]]=="CL1")
table[[154]]<-as.numeric(data[[27]]=="CL2")
table[[155]]<-as.numeric(data[[27]]=="CL3")
table[[156]]<-as.numeric(data[[27]]=="CL4")
table[[157]]<-as.numeric(data[[27]]=="CL5")
table[[158]]<-as.numeric(data[[27]]=="CL6")
# 28 Meth
table[[159]]<-as.numeric(data[[28]]=="CL0")
table[[160]]<-as.numeric(data[[28]]=="CL1")
table[[161]]<-as.numeric(data[[28]]=="CL2")
table[[162]]<-as.numeric(data[[28]]=="CL3")
table[[163]]<-as.numeric(data[[28]]=="CL4")
table[[164]]<-as.numeric(data[[28]]=="CL5")
table[[165]]<-as.numeric(data[[28]]=="CL6")
# 29 Mushrooms
table[[166]]<-as.numeric(data[[29]]=="CL0")
table[[167]]<-as.numeric(data[[29]]=="CL1")
table[[168]]<-as.numeric(data[[29]]=="CL2")
table[[169]]<-as.numeric(data[[29]]=="CL3")
table[[170]]<-as.numeric(data[[29]]=="CL4")
table[[171]]<-as.numeric(data[[29]]=="CL5")
table[[172]]<-as.numeric(data[[29]]=="CL6")
# 30 Nicotine
table[[173]]<-as.numeric(data[[30]]=="CL0")
table[[174]]<-as.numeric(data[[30]]=="CL1")
table[[175]]<-as.numeric(data[[30]]=="CL2")
table[[176]]<-as.numeric(data[[30]]=="CL3")
table[[177]]<-as.numeric(data[[30]]=="CL4")
table[[178]]<-as.numeric(data[[30]]=="CL5")
table[[179]]<-as.numeric(data[[30]]=="CL6")
# 31 Semer
table[[180]]<-as.numeric(data[[31]]=="CL0")
table[[181]]<-as.numeric(data[[31]]=="CL1")
table[[182]]<-as.numeric(data[[31]]=="CL2")
table[[183]]<-as.numeric(data[[31]]=="CL3")
table[[184]]<-as.numeric(data[[31]]=="CL4")
table[[185]]<-as.numeric(data[[31]]=="CL5")
table[[186]]<-as.numeric(data[[31]]=="CL6")
# 32 VSA
table[[187]]<-as.numeric(data[[32]]=="CL0")
table[[188]]<-as.numeric(data[[32]]=="CL1")
table[[189]]<-as.numeric(data[[32]]=="CL2")
table[[190]]<-as.numeric(data[[32]]=="CL3")
table[[191]]<-as.numeric(data[[32]]=="CL4")
table[[192]]<-as.numeric(data[[32]]=="CL5")
table[[193]]<-as.numeric(data[[32]]=="CL6")