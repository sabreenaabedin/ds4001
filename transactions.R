install.packages("arules")
library(arules)

groceries<-read.transactions("C:/Users/Sabreena/Dropbox/DS/groceries.csv", sep = ",")

inspect(groceries[1:5])
itemFrequency(groceries[,1:3])
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

image(sample(groceries,100))

myrules <-apriori(data = groceries, parameter = list(support =0.005, confidence = 0.25, minlen = 1))
inspect(myrules)
inspect(sort(myrules, by = "lift") [1:5])

berryrules <- subset(myrules, items %in% "berries")
inspect(berryrules)

write(myrules, file = "groceryrules.csv", sep = ",", quote = TRUE, row.names=FALSE )
