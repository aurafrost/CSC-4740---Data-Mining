# Jimmy Tran
# 0395
# Data Mining CSC 4740

# install and require necessary libraries
library(arules)
require(arules)
library(arulesViz)
require(arulesViz)

# read csv data and set class as factor before converting to data frame
data<-read.csv("mini_project_4.csv", colClasses="factor")
data<-data.frame(data)

# Apriori. maxlen is set to 2 to complete within reasonable time
rules<-apriori(data,parameter=list(maxlen=2)) # results in 39673 rules

# trimming rules to be >10 and <20. Resulting subrules has 11 rules.
# Result of subrules will be at end of script in comments
subrules<-rules[quality(rules)$lift>2.2]

# create scatterplots for rules (not subrules) based on support, confidence, lift
scatterplot<-plot(rules, measure=c("support","confidence","lift"))

# create new set of subrules based on lift
liftrules<-head(sort(rules, by="lift"), 10)

# plot of top 10 lift rules in vertices form (directed graph)
directedgraph<-plot(liftrules, method="graph")

# parallel coordinate plot (commented out due to error)
# pcplot<-plot(liftrules, method="paracoord") 
# This returns an error for some reason. Strange as I had no issues with the directed graph.
# Couldn't fix it. As such, have no plot for it either. 
# Possibly an issue with the library? Error message mentions a method I don't use directly.
# Error message follows:
# Warning message:
#   In cbind(pl, pr) :
#   number of rows of result is not a multiple of vector length (arg 2)

# Results in subrules after trimming. 
# The pdf said to have top 10 but also said 10-20 earlier. 
# So I just stuck with 11. The top 10 lift rules are the same anyway bar rule 8 below.
#
# lhs                 rhs               support   confidence lift     count
# [1]  {Legalh.CL2=1} => {Legalh.CL0=0} 0.1050398 1          2.383059 198  
# [2]  {VSA.CL1=1}    => {VSA.CL0=0}    0.1061008 1          4.383721 200  
# [3]  {Amyl.CL1=1}   => {Amyl.CL0=0}   0.1114058 1          3.250000 210  
# [4]  {LSD.CL3=1}    => {LSD.CL0=0}    0.1135279 1          2.310049 214  
# [5]  {Amyl.CL2=1}   => {Amyl.CL0=0}   0.1257294 1          3.250000 237  
# [6]  {Coke.CL3=1}   => {Coke.CL0=0}   0.1368700 1          2.225502 258  
# [7]  {LSD.CL1=1}    => {LSD.CL0=0}    0.1374005 1          2.310049 259  
# [8]  {Coke.CL2=1}   => {Coke.CL0=0}   0.1432361 1          2.225502 270  
# [9]  {Caff.CL5=1}   => {Caff.CL6=0}   0.1448276 1          3.770000 273  
# [10] {Legalh.CL3=1} => {Legalh.CL0=0} 0.1713528 1          2.383059 323  
# [11] {USA=1}        => {UK=0}         0.2954907 1          2.241379 557 
# 
# Analysis: Lift seemed like a better measure for trimming the rules down as many rules
# hit the ceiling of 1 for support and confidence. Seems like a lot of the rules
# with high lift have rather low support. I guess support is a rather poor measure
# in that case in regards to data mining. Not like confidence is much better since all
# of these rules have values of 1.