#Nicholas Smith, Jaden Jackson, Weilun Mai
install.packages('lubridate')
install.packages('sqldf')
library(lubridate)
library(sqldf)

train = read.csv("training.csv")

convert = function(basedata){
  
  basedata$time<-ymd_hms(basedata$contactStart, tz = "America/Denver")
  basedata$timerounded<-floor_date(basedata$time, unit = "30 min")
  
  intervals<-sqldf('SELECT timerounded, COUNT(timerounded) as "calls" FROM basedata GROUP BY timerounded')
  timeNumeric=as.numeric(hm(format(intervals$timerounded, format="%H:%M")))/3600
  workHour=ifelse(timeNumeric<19,ifelse(timeNumeric>6.5,timeNumeric-7,0),0)
  day=wday(intervals$timerounded, label = TRUE)
  week=isoweek(intervals$timerounded) - 17

  df= data.frame(intervals, 
                 timeNumeric,
                 workHour,
                 day,
                 week)

  return(df)
}

df=convert(train)

plot(df$timeNumeric,df$calls,
     xlab="Time of Day (hour)",
     ylab="Avg Call Volume (calls/30min)",
     main="Call Volume vs. Time of Day")

convertToDaily=function(data){
  df=sqldf('SELECT day, SUM(calls) as "callVolume" FROM data GROUP BY day, week')
  return(df)
}

daily = convertToDaily(df)
boxplot(daily$callVolume ~ daily$day, 
        xlab = "Day of Week", 
        ylab = 'Number of Calls (per day)',
        main = "Total Calls vs. Day of Week")
  
x = format(df$timerounded, format="%D")
df$holiday=ifelse(x=="05/28/18",1,ifelse(x=="07/04/18",1,0))
df$mon=ifelse(df$day=='Mon',ifelse(df$holiday==0,1,0),0)
df$tue=ifelse(df$day=='Tue',1,0)
df$wed=ifelse(df$day=='Wed',ifelse(df$holiday==0,1,0),0)
df$weekend=ifelse(df$day=='Sat',1,ifelse(df$day=='Sun',1,0))
df$eow=ifelse(df$wed==1,1,ifelse(df$day=='Thu',1,ifelse(df$day=='Fri',1,0)))
df$on=ifelse(df$timeNumeric<19,ifelse(df$timeNumeric>6.5,1,0),0)

#variable selection
library(leaps)
regfit.bwd = regsubsets(calls~(workHour+I(workHour^2)+holiday:on+mon:on+tue:on+eow:on+weekend:on)^2,
                        data=df, method = 'backward',nvmax=19)
reg.summary=summary(regfit.bwd)
plot(reg.summary$adjr2,
     xlab = 'Number of Predictors', 
     ylab='Adjusted R^2',type = 'l',
     main='Backward Selection for CVM')
#looks like 6 variables is good 
points(6, reg.summary$adjr2[6], 
       col='red',cex = 2, pch = 20)
#what are the 6 selected variables
print(reg.summary)
#workHour:on:mon,workHour:on:tue,workHour:on:eow, 
#I(workHour^2):on:mon,I(workHour^2):on:tue,I(workHour^2):on:eow,

#random training index
set.seed(1)
trainIndex = sample(c(1:dim(df)[1]), .7*dim(df)[1], replace = FALSE)

#fit selected model with appropriate intercepts
fit=lm(calls~on:mon+on:tue+on:eow+workHour:on:mon+workHour:on:tue+workHour:on:eow+ 
       I(workHour^2):on:mon+I(workHour^2):on:tue+I(workHour^2):on:eow,data=df,subset=trainIndex)
testing=df[-trainIndex,"calls"]
yhat=predict(fit,df[-trainIndex,])
error=mean((yhat-testing)^2)
error#1893.57
sqrt(error)#~45

#final model 
CVM=lm(calls~on:mon+on:tue+on:eow+workHour:on:mon+workHour:on:tue+workHour:on:eow+ 
         I(workHour^2):on:mon+I(workHour^2):on:tue+I(workHour^2):on:eow,data=df)

#visualize model
x = seq(7,18.5,0.5)

#fit mon
y = 8.2635 + 213.996*(x-6.5) - 16.8979*((x-6.5)^2) 
plot(df[which(mon==1),'timeNumeric'],df[which(mon==1),'calls'],
     cex=.5,col="darkgrey",
     xlab="Hour of Day (MST)",
     ylab="Avg Call Volume (calls/30min)",
     main="Model Fit for Monday")
lines(x,y,col="red",lwd=2)

#fit tue
ytue = 8.2635 + 174.5085*(x-6.5) - 13.6978*((x-6.5)^2) 
plot(df[which(tue==1),'timeNumeric'],df[which(tue==1),'calls'],
     cex=.5,col="darkgrey",
     xlab="Hour of Day (MST)",
     ylab="Avg Call Volume (calls/30min)",
     main="Model Fit for Tuesday")
lines(x,ytue,col="blue",lwd=2)

#fit end of week
yend = 8.2635 + 152.7183*(x-6.5) - 12.0564*((x-6.5)^2) 
plot(df[which(eow==1),'timeNumeric'],df[which(eow==1),'calls'],
     cex=.5,col="darkgrey",
     xlab="Hour of Day (MST)",
     ylab="Avg Call Volume (calls/30min)",
     main="Model Fit for End of Week")
lines(x,yend,col="dark green",lwd=2)

#fit all data
plot(df$timeNumeric,df$calls,
     cex=.5,col="darkgrey",
     xlab="Time of Day (hour)",
     ylab="Avg Call Volume (calls/30min)",
     main="Call Volume Model")
lines(x,y,col="red",lwd=2)
lines(x,ytue,col="blue",lwd=2)
lines(x,yend,col="dark green",lwd=2)
xbase=seq(0,23.5,0.5)
baseline=rep(3,length(xbase))
lines(xbase,baseline,lwd=2)
legend("topleft",legend=c("Monday", "Tuesday", "End of Week", "Baseline"),
       col=c("red","blue","dark green","black"),lty=1,lwd=2,cex=.8,bty = "n")

##############################################################
#final predictions
#CVM=lm(calls~on:mon+on:tue+on:eow+workHour:on:mon+workHour:on:tue+workHour:on:eow+ 
#             I(workHour^2):on:mon+I(workHour^2):on:tue+I(workHour^2):on:eow,data=df)
answer.calls = read.csv("answers_volume-1.csv")

testCVM=data.frame(answer.calls)
testCVM$time<-ymd_hms(testCVM$date, tz = "America/Denver")
testCVM$timeNumeric=as.numeric(hm(format(testCVM$time, format="%H:%M")))/3600
testCVM$workHour=ifelse(testCVM$timeNumeric<19,ifelse(testCVM$timeNumeric>6.5,testCVM$timeNumeric-7,0),0)
testCVM$day=wday(testCVM$time, label = TRUE)
x = format(testCVM$time, format="%D")
testCVM$holiday=ifelse(x=="05/28/18",1,ifelse(x=="07/04/18",1,0))
testCVM$mon=ifelse(testCVM$day=='Mon',ifelse(testCVM$holiday==0,1,0),0)
testCVM$tue=ifelse(testCVM$day=='Tue',1,0)
testCVM$wed=ifelse(testCVM$day=='Wed',ifelse(testCVM$holiday==0,1,0),0)
testCVM$eow=ifelse(testCVM$wed==1,1,ifelse(testCVM$day=='Thu',1,ifelse(testCVM$day=='Fri',1,0)))
testCVM$on=ifelse(testCVM$timeNumeric<19,ifelse(testCVM$timeNumeric>6.5,1,0),0)

#predict calls for testCVM
testCVM$calls=predict(CVM,testCVM)

answer.calls$calls=testCVM$calls
write.csv(answer.calls, "answers_volume.csv")


