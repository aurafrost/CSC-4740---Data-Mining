# import data
data<-read.csv('poe_stats.csv')
data<-data.frame(data)

# set id column to null - effectively removing it
colnames(data)[colnames(data)=="id"] <- "NULL"

# replace null values with no account
data$twitch[data$twitch == "null"] <- "No account"