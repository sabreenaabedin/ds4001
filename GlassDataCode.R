library(DMwR)

glassdata <- read.csv("~/Desktop/DS 5559/glassdata.txt", header=FALSE, stringsAsFactors=FALSE)
View(glassdata)
colnames(glassdata) <- c('instance.number','RI','Na','Mg','Al','Si','K','Ca','Ba','Fe','Type')
View(glassdata)
manyNAs(glassdata,0.2)
glassdata <- glassdata[-manyNAs(glassdata),]
nrow(glassdata)
View(glassdata)
sum(is.na(glassdata))

## Let's explore correlation

symnum(cor(glassdata[,2:10], use = "complete.obs"))

## Now that we know there's a correlation, can we use it?
which(is.na(glassdata$Ca)) #find an NA value of Calcium

lm(Ca~RI,data=glassdata) #linear model
glassdata[13,"Ca"]<- -571.1+382.0*glassdata[13,"RI"]

which(is.na(glassdata$RI)) # no more values to replace


## Next, let's try kNN imputation
glassdata <- knnImputation(glassdata, k=10)
sum(is.na(glassdata))

## Finally, let's try mean imputation:

glassdata$Na[is.na(glassdata$Na)] <- mean(glassdata$Na, na.rm=TRUE)

## Now replace all missing data by the means for the given attribute.

for (j in 1:length(glassdata[1,]))
{
  
  if (is.numeric(glassdata[,j]))
  {
    for(k in 1:length(glassdata[,1]))
    {
      if(is.na(glassdata[k,j]))
      {
        glassdata[k,j] <- mean(glassdata[,j],na.rm=T)
      }
    }
  }
}


write.csv(glassdata, file = "glassdata.csv")
