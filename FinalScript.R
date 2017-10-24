# Reading train.csv and test.csv
train = read.csv("Data/train.csv", header = T)

# ID represents Date and Hour. We need to seperate them in Year, Month, Day and Hour
train$Year = substr(train$ID, 1, 4)
train$Month = substr(train$ID, 5, 6)
train$Day = substr(train$ID, 7, 8)
train$Hour = substr(train$ID, 9, 10)

# Features needed to be converted into proper datatype
train$Year = as.factor(train$Year)
train$Month = as.factor(train$Month)
train$Day = as.factor(train$Day)
train$Hour = as.factor(train$Hour)

# New features are created: Day of Year(YearDay) and Name of the Day(DayName)
train$Date <- as.Date(with(train, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
weekend=as.POSIXlt(train$Date)
train$YearDay=weekend$yday
#train$weekend=ifelse(weekend$wday==0|weekend$wday==6, 1, 0)
train$DayName=ifelse(weekend$wday==1,"Monday",ifelse(weekend$wday==2,"Tuesday", ifelse(weekend$wday==3, "Wedmesday", ifelse(weekend$wday==4,"Thursday", ifelse(weekend$wday==5, "Friday", ifelse(weekend$wday==6, "Saturday", "Sunday"))))))
train$DayName=as.factor(train$DayName)
train$Date=NULL


# test dataset is also prepared the same way.

test = read.csv("Data/test.csv", header = T)

test$Year = substr(test$ID, 1, 4)
test$Month = substr(test$ID, 5, 6)
test$Day = substr(test$ID, 7, 8)
test$Hour = substr(test$ID, 9, 10)

test$Year = as.factor(test$Year)
test$Month = as.factor(test$Month)
test$Day = as.factor(test$Day)
test$Hour = as.factor(test$Hour)

test$Date <- as.Date(with(test, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
weekend=as.POSIXlt(test$Date)
test$YearDay=weekend$yday
#test$weekend=ifelse(weekend$wday==0|weekend$wday==6, 1, 0)
test$DayName=ifelse(weekend$wday==1,"Monday",ifelse(weekend$wday==2,"Tuesday", ifelse(weekend$wday==3, "Wedmesday", ifelse(weekend$wday==4,"Thursday", ifelse(weekend$wday==5, "Friday", ifelse(weekend$wday==6, "Saturday", "Sunday"))))))
test$DayName=as.factor(test$DayName)
test$Date=NULL

# As it is a count data, Poisson and Negative Binomial Models are build.
# A number of models were built.
# Interaction terms were found very useful, improving the score.
# These terms hepls in explaining working hour of weekdays, working hour for different months(summers will have early starting hour than winters) and holidays or off-days.

# Poisson Models
p.model1=glm(Count ~ Year + Month + Day + Hour, data = train, family = poisson)
p.model2=glm(Count ~ Year + Month + Day + Hour + DayName + YearDay, data = train, family = poisson)
p.model3=glm(Count ~ Year + Month + Day + Hour + DayName, data = train, family = poisson)
p.model4=glm(Count ~ Year + Month + Day + Hour*DayName, data = train, family = poisson)
p.model5=glm(Count ~ Year + Hour*DayName + Month*Day, data = train, family = poisson)
p.model6=glm(Count ~ Year + Month*Hour+ Hour*DayName + Month*Day, data = train, family = poisson)
p.model7=glm(Count ~ Year + Month*Hour+ Hour*DayName + Month*Day + Month*DayName, data = train, family = poisson)

# Poisson model comparision based on RMSE score
PoissonComp=data.frame(Actual=train$Count, Model1=round(p.model1$fitted.values), Model2=round(p.model2$fitted.values), Model3=round(p.model3$fitted.values), Model4=round(p.model4$fitted.values), Model5=round(p.model5$fitted.values), Model6=round(p.model6$fitted.values), Model7=round(p.model7$fitted.values))

library(Metrics)
rmse(PoissonComp$Actual,PoissonComp$Model1)   # 72.87186
rmse(PoissonComp$Actual,PoissonComp$Model2)   # 49.67913
rmse(PoissonComp$Actual,PoissonComp$Model3)   # 49.95335
rmse(PoissonComp$Actual,PoissonComp$Model4)   # 45.40249
rmse(PoissonComp$Actual,PoissonComp$Model5)   # 38.08427
rmse(PoissonComp$Actual,PoissonComp$Model6)   # 35.70049
rmse(PoissonComp$Actual,PoissonComp$Model7)   # 35.38155

# Predicting on test dataset
p1=round(predict(p.model1, newdata = test, type = "response"))
p2=round(predict(p.model2, newdata = test, type = "response"))
p3=round(predict(p.model3, newdata = test, type = "response"))
p4=round(predict(p.model4, newdata = test, type = "response"))
p5=round(predict(p.model5, newdata = test, type = "response"))
p6=round(predict(p.model6, newdata = test, type = "response"))
p7=round(predict(p.model7, newdata = test, type = "response"))

# Saving the results.
p.result1=data.frame(ID=test$ID, Count=p1)
p.result2=data.frame(ID=test$ID, Count=p2)
p.result3=data.frame(ID=test$ID, Count=p3)
p.result4=data.frame(ID=test$ID, Count=p4)
p.result5=data.frame(ID=test$ID, Count=p5)
p.result6=data.frame(ID=test$ID, Count=p6)
p.result7=data.frame(ID=test$ID, Count=p7)

# Dumping the submission files.
write.csv(p.result1, "submission1.csv", row.names = F)
write.csv(p.result2, "submission2.csv", row.names = F)
write.csv(p.result3, "submission3.csv", row.names = F)
write.csv(p.result4, "submission4.csv", row.names = F)
write.csv(p.result5, "submission5.csv", row.names = F)
write.csv(p.result6, "submission6.csv", row.names = F)
write.csv(p.result7, "submission13.csv", row.names = F)

# Negative Binomial Models
library(MASS)

nb.model1=glm.nb(Count ~ Year + Month + Day + Hour, data = train)
nb.model2=glm.nb(Count ~ Year + Month + Day + Hour + DayName + YearDay, data = train)
nb.model3=glm.nb(Count ~ Year + Month + Day + Hour + DayName, data = train)
nb.model4=glm.nb(Count ~ Year + Month + Day + Hour*DayName, data = train)
nb.model5=glm.nb(Count ~ Year + Hour*DayName + Month*Day, data = train)
nb.model6=glm.nb(Count ~ Year + Month*Hour+ Hour*DayName + Month*Day, data = train)
nb.model7=glm.nb(Count ~ Year + Month*Hour+ Hour*DayName + Month*Day + Month*DayName, data = train)

# NB model comparision based on RMSE score
NBComp=data.frame(Actual=train$Count, Model1=round(nb.model1$fitted.values), Model2=round(nb.model2$fitted.values), Model3=round(nb.model3$fitted.values), Model4=round(nb.model4$fitted.values), Model5=round(nb.model5$fitted.values), Model6=round(nb.model6$fitted.values), Model7=round(nb.model7$fitted.values))

#library(Metrics)
rmse(NBComp$Actual,NBComp$Model1)   # 73.86755
rmse(NBComp$Actual,NBComp$Model2)   # 50.29052
rmse(NBComp$Actual,NBComp$Model3)   # 50.65039
rmse(NBComp$Actual,NBComp$Model4)   # 46.22607
rmse(NBComp$Actual,NBComp$Model5)   # 39.51571
rmse(NBComp$Actual,NBComp$Model6)   # 37.18435
rmse(NBComp$Actual,NBComp$Model7)   # 36.97216

# Predicting on test dataset
p1=round(predict(nb.model1, newdata = test, type = "response"))
p2=round(predict(nb.model2, newdata = test, type = "response"))
p3=round(predict(nb.model3, newdata = test, type = "response"))
p4=round(predict(nb.model4, newdata = test, type = "response"))
p5=round(predict(nb.model5, newdata = test, type = "response"))
p6=round(predict(nb.model6, newdata = test, type = "response"))
p7=round(predict(nb.model7, newdata = test, type = "response"))

# Saving the results.
nb.result1=data.frame(ID=test$ID, Count=p1)
nb.result2=data.frame(ID=test$ID, Count=p2)
nb.result3=data.frame(ID=test$ID, Count=p3)
nb.result4=data.frame(ID=test$ID, Count=p4)
nb.result5=data.frame(ID=test$ID, Count=p5)
nb.result6=data.frame(ID=test$ID, Count=p6)
nb.result7=data.frame(ID=test$ID, Count=p7)

# Dumping the submission files.
write.csv(nb.result1, "submission7.csv", row.names = F)
write.csv(nb.result2, "submission8.csv", row.names = F)
write.csv(nb.result3, "submission9.csv", row.names = F)
write.csv(nb.result4, "submission10.csv", row.names = F)
write.csv(nb.result5, "submission11.csv", row.names = F)
write.csv(nb.result6, "submission12.csv", row.names = F)
write.csv(nb.result7, "submission14.csv", row.names = F)


# Finally submission12.csv gave the best result on public leader board.






