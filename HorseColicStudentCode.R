#HorseColic <- read.table("~/Desktop/Course Archive/DS 5559/HorseColic.txt")
HC <- HorseColicOriginalData
HC[HC == "?"] <- NA
colnames(HC) <- c('surgery?','Age', 'Hospital.Number', 'Rectal.Temp', 'Pulse', 'Resp.Rate', 'Temp.of.Extremities', 'Peripheral.Pulse','Mucous.Membranes', 'Cap.Refill.Time', 'Pain', 'Peristalsis', 'Abdominal.Distension','Nasogastric.Tube', 'Nasogastric.Reflux', 'Nasogastric.Reflux.Ph', 'Rectal.Exam.Feces', 'Abdomen','Packed.Cell.Volume', 'Total.Protein', 'Abdominocentesis.Appearance', 'Abdominocentesis.total.protein', 'Outcome','Surgical.Lesion?','Type.of.Lesion','Type.of.Lesion2','Type.of.Lesion3','cp_data')
summary(HC)
install.packages("DMwR")
library(DMwR)
anyNA(HC,0.2) #looks for rows that has 20% or more of its data missing
HC <- HC[-manyNAs(HC),] #get rid of the rows that have too much missing data
nrow(HC)

summary(HC) #Nasgoastric Ph doesn't have 50 usable rows, nullify it
View(HC)
HC$Nasogastric.Reflux.Ph <- NULL

library(ggplot2)
#pairs(HC)
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(HC,"Nasogastric.Reflux.Ph",which(as.numeric(HC$Nasogastric.Reflux.Ph) > 1000),NA)



#manyNAs(HC,0.2)
#HC <- HC[-manyNAs(HC),]
#nrow(HC)
#colnames(HC)
#colnames(HC[27])
