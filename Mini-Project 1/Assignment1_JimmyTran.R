#Jimmy Tran
#CSC4740
#Script assumes workspace is the same folder as .csv file location
data<-read.csv("Distribution_of_population_by_sex.csv")
attach(data)
year<-Year
female<-Female
male<-Male
total<-Total

plot(female, type="o", col="red", axes=FALSE, ann=FALSE)
lines(male, type="o", pch=22, col="blue")

title(main="Distribution of Population by Sex", xlab="Year", ylab="Population")
box()
options("scipen"=100, digits = 4)

axis(1, at=1:61, lab=year)
pop_range<-c(20,30,40,50,60,70,80,90,100,110)
axis(2)
legend(1, 90000000, c("Female","Male"), cex=0.8, col=c("red","blue"), pch=21:22, lty=1:2)
