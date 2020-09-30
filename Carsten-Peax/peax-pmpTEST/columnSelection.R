data <- read.table('data/short.csv', header=TRUE, na.strings = "NA", sep=",", skipNul = FALSE)
d_subset = data[,c(1:80)]
write.csv(d_subset, file="data/shortColumn.csv",row.names=FALSE,quote=FALSE)
