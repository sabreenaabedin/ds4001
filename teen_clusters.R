teens <- read.csv("/var/folders/xr/7vbfz5cd7f368gps51mwfj8r0000gn/T//Rtmp1evH7Z/datae6f597dd779")
str(teens)
table(teens$gender)
table(teens$gender, useNA = "ifany")
summary(teens$age)
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age)

## Dummy coding to allow for extra gender value, so as not to lose too much data.
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender),1,0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

## Test that our dummy coding worked

table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

mean(teens$age)

mean(teens$age, na.rm = TRUE)

aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm=TRUE))

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)

## Analysis
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))
teen_clusters <- kmeans(interests_z, 5)
teen_clusters$size
teen_clusters$centers
