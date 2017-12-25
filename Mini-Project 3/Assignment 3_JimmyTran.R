# Jimmy Tran
# 0395
# Data Mining CSC 4740

# import and require factoextra library
library("factoextra")
require("factoextra")
options("scipen"=100, digits = 4) #remove scientific notation
# data import
# ignore column 1 as that isn't a principal component
# add headers so that row 1 isn't mistaken as a header row
data<-read.csv("Wine.csv", header=FALSE, 
               colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
               col.names=c
               ("NULL","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium",
                 "Total phenols","Flavanoids","Nonflavanoid phenols",
                 "Proanthocyanins","Color intensity","Hue",
                 "OD280/OD315 of diluted wines","Proline"))
data.pca<-prcomp(data, scale=TRUE) # principal component analysis
var<-get_pca_var(data.pca) # extract variable results

# table of pcs (Principal components) vs attributes
# Borrowed helper function to get correlation between variables and pcs
var_cor_func <- function(var.loadings, comp.sdev){
  var.loadings*comp.sdev
}
loadings <- data.pca$rotation
sdev <- data.pca$sdev
var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))
var.cos2 <- var.coord^2
comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
table<-head(var.contrib[]) 

# get eigenvalues
eigenData<-(data.pca$sdev)^2

# scree plot for the 13 pcs
sPlot<-fviz_screeplot(data.pca, ncp=13, choice="eigenvalue")
sPlot<-sPlot + labs(title = "Eigenvalues for the Principal Components",
                    x = "Principal Components", y = "Eigenvalue")

# Outputs (I removed scientific notation. Not sure if we were supposed to do that,
# but it looks better in my opinion.)

# PCs vs Attributes Table 
# (I get the feeling this is wrong since my labels are on the other axis...)
#                          PC1      PC2     PC3      PC4    PC5     PC6      PC7
# Alcohol            2.0830974 23.39188  4.3008  0.03188  7.058  4.5599  0.31805
# Malic.acid         6.0116950  5.05939  0.7923 28.82512  0.124 28.8169 17.68404
# Ash                0.0004207  9.98995 39.2156  4.58712  2.046  2.3862  2.22519
# Alcalinity.of.ash  5.7274256  0.01122 37.4642  0.37039  0.437  1.0166  8.23513
# Magnesium          2.0161740  8.97805  1.7097 12.37608 52.860  0.1455 10.42536
# Total.phenols     15.5757183  0.42301  2.1368  3.92311  2.230  0.7077  0.07798
#                       PC8     PC9     PC10    PC11   PC12     PC13
# Alcohol           15.6926 25.8693  4.47766  5.1038 7.0908  0.02241
# Malic.acid         0.4333  0.5668  9.55304  0.5850 1.4810  0.06741
# Ash                2.8988  9.4676  0.07358 24.8693 0.2462  1.99425
# Alcalinity.of.ash 18.3158  4.0180  0.27878 22.9742 0.3107  0.84057
# Magnesium          2.4449  7.3659  0.46064  0.5082 0.3871  0.32233
# Total.phenols     16.4782  8.1816 10.24841  9.2624 9.2345 21.52106

# Eigenvalues
# [1] 4.7059 2.4970 1.4461 0.9190 0.8532 0.6417 0.5510 0.3485 0.2889 0.2509 0.2258
# [12] 0.1688 0.1034

# Answer for Part 9
# Based off the data, I would remove the first 3 principal components (columns)
# since they have eigenvalues above 1.
# I would keep the remaining ones as they are within the 0 to 1 range and 
# accurately represent the variance of each respective principal component.