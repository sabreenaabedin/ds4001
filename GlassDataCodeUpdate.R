library(DMwR)

glassdata <- read.csv("~/Desktop/DS 4559 2016/glassdata.txt", header=FALSE, stringsAsFactors=FALSE)
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
which(is.na(glassdata$Ca))

lm(Ca~RI,data=glassdata)
glassdata[13,"Ca"]<- -571.1+382*glassdata[13,"RI"]
sum(is.na(glassdata))

## Next, let's try kNN imputation
glassdata <- knnImputation(glassdata, k=10)
sum(is.na(glassdata))
which(is.na(glassdata$RI))
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

sum(is.na(glassdata))
##For use in analysis, data should be as Gaussian in distribution as possible.  We use the log transform
##to minimize skewness.  This is called a transformation.

hist(glassdata$Si)
glassdata$Si <- log(glassdata$Si)
hist(glassdata$Si)


##Depending on the method we will be using for analysis, it may be important to normalize our data.
##This is shown below.
##Normalized Data (make data range from 0 to 1).  Clearly, this is only done with numeric data.
NewRI <- (glassdata$RI-min(glassdata$RI))/(max(glassdata$RI)-min(glassdata$RI))
hist(NewRI)


write.csv(glassdata, file = "glassdata.csv")
