#Nicholas Smith, Jaden Jackson, Weilun Mai
install.packages('lubridate')
install.packages('sqldf')
library(lubridate)
library(sqldf)

train = read.csv("training.csv")

convert = function(basedata){
  basedata = basedata[which(basedata$agentSeconds >0),]
  basedata$time<-ymd_hms(basedata$contactStart, tz = "America/Denver")
  basedata$timerounded<-floor_date(basedata$time, unit = "30 min")
  
  calls<-sqldf('SELECT timerounded, AVG(agentSeconds) as "handletime" FROM basedata GROUP BY timerounded')
  timeNumeric=as.numeric(hm(format(calls$timerounded, format="%H:%M")))/3600
  workHour=timeNumeric-7
  day=wday(calls$timerounded, label = TRUE)
  week=isoweek(calls$timerounded)-18
  
  df=data.frame(calls,timeNumeric,workHour,day,week)
  df$mon=ifelse(df$day=='Mon',1,0)
  df$tue=ifelse(df$day=='Tue',1,0)
  df$wed=ifelse(df$day=='Wed',1,0)
  df$thu=ifelse(df$day=='Thu',1,0)
  df$fri=ifelse(df$day=='Fri',1,0)
  return(df)
}

#converted data for 30min intervals
df=convert(train)
plot(df$timeNumeric,df$handletime,
     xlab="Time of Day (hour)",
     ylab="Avg Handle Time (seconds/30min)",
     main="Handle Time Data")

#split data into grind and crunch time
grind=sqldf("SELECT * FROM df WHERE timeNumeric<18")
plot(grind$timeNumeric,grind$handletime,
     xlab="Time of Day (hour)",
     ylab="Avg Handle Time (seconds/30min)",
     main="\"The Grind\"")
df$crunchTime=(df$timeNumeric-18)
crunch=sqldf("SELECT * FROM df WHERE timeNumeric>=18")
plot(crunch$timeNumeric,crunch$handletime,
     xlab="Time of Day (hour)",
     ylab="Avg Handle Time (seconds/30min)",
     main="\"Crunch Time\"")
df$g=ifelse(df$timeNumeric<18,1,0)
df$c=ifelse(df$timeNumeric>=18,1,0)

#variable selection
library(leaps)
regfit.bwd = regsubsets(handletime~(workHour+I(workHour^2)+I(workHour^3)+I(workHour^4)
                                    +mon+tue+wed+thu+fri+week)^2,
                        data=grind, method = 'backward',nvmax=12)
reg.summary=summary(regfit.bwd)
plot(reg.summary$adjr2,
     xlab = 'Number of Variables', 
     ylab='Adjusted R^2',type = 'l',
     main='Backward Selection for HTM')
#looks like 6 variables is good 
points(6, reg.summary$adjr2[6], 
       col='red',cex = 2, pch = 20)
#what are the 6 selected variables
print(reg.summary)

#does the day really not matter? 
boxplot(grind$handletime~grind$day,
        xlab="Day of Week",
        ylab="Avg Handle Time (seconds/30min)",
        main="Avg Handle Time by Day")

#best subset over backward selection
regfit.best = regsubsets(handletime~workHour+I(workHour^2)+I(workHour^3)+I(workHour^4)+I(workHour^5)
                         +I(workHour^6)+I(workHour^7),data=grind)
reg.summary = summary(regfit.best)
plot(reg.summary$adjr2,
     xlab = 'Number of Predictors', 
     ylab='Adjusted R^2',
     main='Best Subset for HTM',
     type = 'l')
points(4, reg.summary$adjr2[4], 
       col='red',cex = 2, pch = 20)
print(reg.summary)

#random training index
set.seed(1)
trainIndex = sample(c(1:dim(grind)[1]), .7*dim(grind)[1], replace = FALSE)

#fit best subset model
fit=lm(handletime~workHour+I(workHour^2)+I(workHour^3)+I(workHour^4),data=grind,subset=trainIndex)
testing=grind[-trainIndex,"handletime"]
yhat=predict(fit,grind[-trainIndex,])
error=mean((yhat-testing)^2)
error #1967.13 
sqrt(error)#~45

#final model includes grind and crunch and no common intercept
set.seed(1)
trainIndex = sample(c(1:dim(df)[1]), .7*dim(df)[1], replace = FALSE)
htm=lm(handletime~0+g+workHour:g+I(workHour^2):g+I(workHour^3):g+I(workHour^4):g
         +c+crunchTime:c,data=df,subset=trainIndex)
testing=df[-trainIndex,"handletime"]
yhat=predict(htm,df[-trainIndex,])
error=mean((yhat-testing)^2)
error #27,500
sqrt(error)#~166

#check correct coefficients
HTM=lm(handletime~0+g+workHour:g+I(workHour^2):g+I(workHour^3):g+I(workHour^4):g
       +c+crunchTime:c,data=df)
fit.grind=lm(handletime~workHour+I(workHour^2)+I(workHour^3)+I(workHour^4),data=grind)
fit.crunch=lm(handletime~crunchTime,data=crunch)
summary(HTM)
summary(fit.grind)
summary(HTM)
summary(fit.crunch)

#visualize model
timelims=range(df$workHour)
time.grid=seq(from=timelims[1],to=timelims[2])
handlelims=range(grind$handletime)
handle.grid=seq(from=handlelims[1],to=handlelims[2])
plot(df$workHour,df$handletime,xlim=timelims, ylim=handlelims,
     cex=.5,col="darkgrey",
     xlab="Hour of Day",
     ylab="Avg Handle Time (seconds/30 min)",
     main="Handle Time Model")
x = seq(0,10.5,0.5)
summary(fit.grind)
y = 495.92 + 129.99*x - 35.29*(x^2) + 3.84995*(x^3) - 0.14404*(x^4) 
lines(x,y,col="blue",lwd=2)
xc = seq(10.5,12.5,0.5)
summary(fit.crunch)
yc=619.29-102.82*(xc-11)
lines(xc,yc,col="red",lwd=2)
legend("topleft",legend=c("The Grind","Crunch Time"),
       col=c("blue","red"),lty=1,lwd=2,cex=.8,bty = "n")

#########################################################3
#test answers for handletime predictions
#HTM=lm(handletime~0+g+workHour:g+I(workHour^2):g+I(workHour^3):g+I(workHour^4):g
#                 +c+crunchTime:c,data=df)
answer.handletime = read.csv("answers_handletime-1.csv")
testHTM=data.frame(answer.handletime)

testHTM$hour=as.numeric(testHTM$hour)
testHTM$workHour=testHTM$hour-7
testHTM$crunchTime=(testHTM$hour-18)
testHTM$g=ifelse(testHTM$hour<18,1,0)
testHTM$c=ifelse(testHTM$hour>=18,1,0)

testHTM$handletime=predict(HTM,testHTM)

answer.handletime$handletime=testHTM$handletime
write.csv(answer.handletime,"answers_handletime.csv")
