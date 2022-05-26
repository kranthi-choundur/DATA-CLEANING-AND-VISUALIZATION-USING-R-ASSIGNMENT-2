library(robotstxt)
library(rvest)

#dataprocessing

library(dplyr)

#importing dataset

housedata=read.csv("House_Data.csv")
View(housedata)


bengdata=housedata
View(bengdata)

#cleaning for special characters

bengdata$size = gsub("BHK","",bengdata$size) #removed brackets or symbol
View(bengdata)
bengdata$size = gsub("Bedroom","",bengdata$size) #removed brackets or symbol
View(bengdata)

#checking the datatypes

str(bengdata)

bengdata$size=as.integer(bengdata$size)
bengdata$total_sqft=as.numeric(bengdata$total_sqft)
View(bengdata)

str(bengdata)

#class(bengdata$size)

#adding or removing columns in datasheet

set.seed(125)

bengdata=subset(bengdata,select= -(society))
bengdata=subset(bengdata,select= -(location))
bengdata=subset(bengdata,select= -(availability))
View(bengdata)


#dealing with missing values

#cheching na values
is.na(bengdata)

#checking na values
summary(is.na(bengdata))

#replacing the na values


bengdata$size=ifelse(is.na(bengdata$size),
                     ave(bengdata$size,FUN=function(x) 
                       mean(x,na.rm=TRUE)),bengdata$size)



bengdata$total_sqft=ifelse(is.na(bengdata$total_sqft),
                     ave(bengdata$total_sqft,FUN=function(x) 
                       mean(x,na.rm=TRUE)),bengdata$total_sqft)


bengdata$bath=ifelse(is.na(bengdata$bath),
                     ave(bengdata$bath,FUN=function(x) 
                       mean(x,na.rm=TRUE)),bengdata$bath)



bengdata$balcony=ifelse(is.na(bengdata$balcony),
                     ave(bengdata$balcony,FUN=function(x) 
                       mean(x,na.rm=TRUE)),bengdata$balcony)


summary(is.na(bengdata))

View(bengdata)

#dealing with categorical variables or data

str(bengdata)

bengdata$area_type=as.factor(bengdata$area_type)

str(bengdata)
View((bengdata))


bengdata$area_type=factor(bengdata$area_type,levels=c("Super built-up  Area","Plot  Area","Built-up  Area","Carpet  Area"),labels=c(1,2,3,4))
View(bengdata)


#CLEANED DATASET
write.csv(bengdata,"bengdata.csv")


summary(is.na(bengdata))

beng2=bengdata


##training set and testing
library(caTools)

split=sample.split(beng2$price,SplitRatio = 0.8)
View(split)


training_set=subset(beng2,split==TRUE)
testing_set=subset(beng2,split==FALSE)

View(training_set)
View(testing_set)


#outlier detection

library(outliers)
test=grubbs.test(beng2$price)
test

#feature scaling(standardization)

training_set$price=scale(training_set[,6])
View(training_set)

testing_set$price=scale(testing_set[,6])
View(testing_set)


#data visualization
library(ggplot2)

View(beng2)


sqft=beng2[ ,3]
View(sqft)

cost=beng2[,6]
View(cost)

sqftprice=data.frame(sqft,cost)
View(sqftprice)

sqftprice["areatypes"]=beng2$area_type



#line plot for price for sqft

ggplot(data=sqftprice,aes(x=sqft,y=cost))+
  geom_line(color="red",size=2)+         #size= thickness of line
  labs(x="sqft",y="price",title="price per sqft")

#scatter plot for sqft and price


ggplot(data=sqftprice,aes(x=sqft,y=cost))+
  geom_point(color="red",size=2,shape=sqftprice$areatypes)+         #size= thickness of line
  labs(x="sqft",y="price",title="sqft vs price graph")


#histogram of medical expenses

ggplot(data=sqftprice,aes(x=cost))+
  geom_histogram(binwidth =30,color="black")+         #size= thickness of line
  labs(x="sqft",y="price",title="sqft vs price")

#box plot
ggplot(sqftprice, aes(x =sqft, y = cost)) + theme_bw() + 
  geom_boxplot(alpha=1) + 
  labs(y = "price", x = "sqft", 
       title = "sqft vs price")



ggplot(beng2, aes(x =total_sqft,y=price),shape=area_type) + theme_light() + 
  facet_grid(~area_type) + geom_point() + 
  labs(x="sqft",y="price",title="price per sqft")








