#HorseColic <- read.table("~/Downloads/Data Science/Data/HorseColicOrginialData.txt")
HC <- HorseColicOriginalData
View(HC)
#R knows how to deal with NA
HC[HC == "?"] <- NA 
colnames(HC) <- c('surgery?','Age', 'Hospital.Number', 'Rectal.Temp', 'Pulse', 'Resp.Rate', 'Temp.of.Extremities', 'Peripheral.Pulse','Mucous.Membranes', 'Cap.Refill.Time', 'Pain', 'Peristalsis', 'Abdominal.Distension','Nasogastric.Tube', 'Nasogastric.Reflux', 'Nasogastric.Reflux.Ph', 'Rectal.Exam.Feces', 'Abdomen','Packed.Cell.Volume', 'Total.Protein', 'Abdominocentesis.Appearance', 'Abdominocentesis.total.protein', 'Outcome','Surgical.Lesion?','Type.of.Lesion','Type.of.Lesion2','Type.of.Lesion3','cp_data')
View(HC)
summary(HC)
#we're missing more than 60% of the data, so remove the attribute (in this class she'll accept 50%)
HC$Nasogastric.Reflux.Ph <- NULL
HC[!!rowSums(!is.na(HC)),] #didn't go over in class
hist.default(as.numeric(HC$V27)) #didn't go over in class

#we named the function "outlierReplace"
outlierReplace = function(dataframe, cols, rows, newValue = NA){
  if(any(rows)){
    set(dataframe, rows, cols, newValue)
  }
}
outlierReplace(HC,"Type.of.Lesion3", which(HC$Type.of.Lesion3 > 1000), NA)