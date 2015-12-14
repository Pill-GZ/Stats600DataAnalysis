setwd("~/Coursework/Stats600/HW/Capstone/")
library(foreign)
library(data.table)
library(MASS)

######################### Pre-processing #########################

## Merge with age and gender, and body measurement data, BMI ##

# First read in demographics, take age and gender
demographics = read.xport('DEMO_D.XPT')
demographics = data.table(demographics)
attach(demographics)
agegender <- cbind(SEQN, 'gender'=RIAGENDR, 'age'=RIDAGEEX)
detach(demographics)

# Then read in bodymeasures, take BMI's
bodymeasures = read.xport('BMX_D.XPT')
bodymeasures = data.table(bodymeasures)
#bodymeasures = bodymeasures[bodymeasures$BMXBMI<60]
attach(bodymeasures)
BMI <- cbind(SEQN, BMXBMI)
detach(bodymeasures)
colnames(BMI) <- c('SEQN','BMI')
BMI <- data.frame(BMI)

# Merge age, gender, BMI by sequence numbers
merged <- merge(agegender, BMI, by = "SEQN")

# Now read in activity readings
data <- read.csv("activity.csv")
# head(data, n = 40)

# Now merge activity readings with personal info by sequence number
merged <- merge(merged, data, by = "SEQN")
names(merged) <- c("seqn","gender","age","bmi","day","hour","activity")
merged <- merged[complete.cases(merged),]
merged <- merged[merged$bmi<60,]
merged[,'day'] <- factor(merged[,'day'])
merged[,'hour'] <- factor(merged[,'hour'])
merged[,'gender'] <- factor(merged[,'gender'])

# Take a small sample for illustration pueposes
sample <- merged[sample(x = dim(merged)[1],size = 10000,replace = F),]

##### Some Exploratory Plots #####

attach(merged)
# attach(sample)
plot(jitter(as.numeric(day)),jitter(activity)) # plot(riagendr, bpxsy1)
weekday.average = mean(activity[(as.numeric(day)<7)&(as.numeric(day)>1)])
abline(h=weekday.average)
weekday.q95 = quantile(activity[(as.numeric(day)<7)&(as.numeric(day)>1)],probs = 0.95)
abline(h=weekday.q95)
weekend.average = mean(activity[(as.numeric(day)>6)|(as.numeric(day)<2)])
abline(h=weekend.average,col='red')
weekend.q95 = quantile(activity[(as.numeric(day)>6)|(as.numeric(day)<2)],probs = 0.95)
abline(h=weekend.q95,col='red')

# define some indicators
weekend.indicator <- (day==1)|(day==7)
sunday <- (day==1)
saturday <- (day==7)
weekends <- factor(saturday+2*sunday)

# Plot activity against day of the week and time of the day
test<-aggregate(activity,by=list(day,seqn),sum)
par(mar=c(4,2,0,0)+0.1)
boxplot(test$x~test$Group.1,ylim=c(0,13000),outline=F,xlab='Activity level', 
        axes = FALSE, ann = FALSE, horizontal=TRUE)
axis(2, at = 1:7, labels = c('Sun','Mon','Tue','Wed','Thu','Fri','Sat'), cex.axis = 1)
axis(1, cex.axis = 1)

par(mar=c(2,4,0,0)+0.1)
par(mfrow=c(2,1))
boxplot(activity~weekend.indicator*hour,ylim=c(0,1100),outline=F,
        col=(c("gold","green")),ylab='Activity level', axes = FALSE, ann = FALSE)
axis(1, at = 0:24*2+0.5, cex.axis = 1,labels=F)
axis(1, at = 1:24*2-0.5, labels = 1:24, cex.axis = 1,tick = F)
axis(2, cex.axis = 1)
legend("topleft", inset = .05,col = c("gold","green"),
       legend = c("Weekdays","Weekends"),
       cex = 1, pch = 15, merge=FALSE, bty = 'n')


boxplot(activity~weekends*hour,outline=F,col=(c("gold","lightgreen","lightblue")),
        ylab='Activity level', axes = FALSE, ann = FALSE)
axis(1, at = 0:24*3+0.5, cex.axis = 1,labels=F)
axis(1, at = 1:24*3-1, labels = 1:24, cex.axis = 1,tick = F)
axis(2, cex.axis = 1)
legend("topleft", inset = .05,col = c("gold","lightgreen","lightblue"),
       legend = c("Weekdays","Saturday","Sunday"),
       cex = 1, pch = 15, merge=FALSE, bty = 'n' )
par(mfrow=c(1,1))

# Do a Box-Cox transformation to stablize variance
#test <- boxcox(activity~weekends*hour)
#lambda <- test$x[which(test$y==max(test$y))]
lambda <- 0.2222222

# Check the boxplots again
#boxplot(activity^lambda~day,outline=F)
#boxplot(activity^lambda~weekend.indicator*hour,outline=F,col=(c("gold","green")))
#boxplot(activity^lambda~weekends*hour,outline=F,col=(c("gold","green","lightblue")))
boxplot(activity^lambda~weekends*hour,outline=F,col=(c("gold","lightgreen","lightblue")),
        ylab='Box-Cox Transformed activity level', axes = FALSE, ann = FALSE)
axis(1, at = 0:24*3+0.5, cex.axis = 1,labels=F)
axis(1, at = 1:24*3-1, labels = 1:24, cex.axis = 1,tick = F)
axis(2, cex.axis = 1)
legend("topleft", inset = .05,col = c("gold","lightgreen","lightblue"),
       legend = c("Weekdays","Saturday","Sunday"),
       cex = 1, pch = 15, merge=FALSE, bty = 'n' )


# Finalize the transformation
activity <- activity^lambda

# plot(bmi,activity)
# plot(gender,activity)
# plot(age,activity)
# plot(hour,activity)

# Plot activity against age and gender
plot(age[gender==1]/12,activity[gender==1],col='green',cex=0.4,
     xlab = 'age (years)',ylab = 'Activity Index',ylim = c(0,2000))
points(age[gender==2]/12,activity[gender==2],col='orange',cex=0.4)

# detach(merged)
# detach(sample)

################### Model fitting #########################

##### day-of-the-week #####

modelfit.weekdays <- lm(activity~day)
summary(modelfit.weekdays)
BIC(modelfit.weekdays)

modelfit.weekend <- lm(activity~weekend.indicator)
summary(modelfit.weekend)
BIC(modelfit.weekend)

levels(weekends) <- c("weekdays","Saturday","Sunday")
modelfit.weekends <- lm(activity~weekends)
summary(modelfit.weekends)
BIC(modelfit.weekends)

# We prefer the more parsimonious 'weekends' model, and will build upon that.

##### time-of-the-day #####

modelfit.day.hour <- lm(activity~weekends+hour)
summary(modelfit.day.hour)
BIC(modelfit.day.hour)

modelfit.dayhour <- lm(activity~weekends*hour)
summary(modelfit.dayhour)
beta<-modelfit.dayhour$coefficients
BIC(modelfit.dayhour)


# Check fitted values
# weekdays
for(i in 1:24) points(3*(i-1)+1,beta[1]+beta[3+i],pch=16,col='darkorange')
i=1:24
lines(3*(i-1)+1,beta[1]+beta[3+i],pch=16,col='darkorange',lwd=3)
# Saturdays
i=1
points(2,beta[1]+beta[3+i]+beta[2],pch=16,col='darkgreen')
for(i in 1:23) points(3*i+2,beta[1]+beta[3+i]+beta[2]+beta[25+i*2],pch=16,col='darkgreen')
i=1
lines(c(2,5),c(beta[1]+beta[3+i]+beta[2],beta[1]+beta[3+i]+beta[2]+beta[25+i*2]),pch=16,col='darkgreen',lwd=3)
i=1:23
lines(3*i+2,beta[1]+beta[3+i]+beta[2]+beta[25+i*2],pch=16,col='darkgreen',lwd=3)
# Sundays
i=1
points(3,beta[1]+beta[3+i]+beta[3],pch=16,col='blue')
for(i in 1:23) points(3*i+3,beta[1]+beta[3+i]+beta[3]+beta[26+i*2],pch=16,col='blue')
i=1
lines(c(3,6),c(beta[1]+beta[3+i]+beta[3],beta[1]+beta[3+i]+beta[3]+beta[25+i*2]),pch=16,col='darkgreen',lwd=3)
i=1:23
lines(3*i+3,beta[1]+beta[3+i]+beta[3]+beta[26+i*2],pch=16,col='blue',lwd=3)
mtext(side = 1,text = 'Hours',line = 2)


##### Selection of BMI and gender-age model #####

BICtable = matrix(c(0),7,7)
AdjR2 = matrix(c(0),7,7)
for(i in 1:7){
  for(j in 1:7){
    modelfit <- lm(activity~saturday+sunday+poly(bmi,i,raw = T)
                              +poly(age,j,raw = T)*gender)
    BICtable[i,j] = BIC(modelfit)
    AdjR2[i,j] = summary(modelfit)$adj.r.squared
  }
}
min(BICtable)
max(AdjR2)
summary(modelfit)

#modelfit <- lm(activity~weekends*hour+bmi*hour+age*gender)

par(mfrow=c(2,1),mar=c(2,3,0,0)+0.5)
plot(bmi,modelfit$residuals,pch='.',ann=F,ylab='Residuals')
mtext(side = 1,text = 'BMI',line = 1.5)
mtext(side = 2,text = 'Residuals',line = 2)
plot(age/12,modelfit$residuals,pch='.',ann=F,ylab='Residuals')
mtext(side = 1,text = 'Age',line = 1.5)
mtext(side = 2,text = 'Residuals',line = 2)

par(mar=c(3,4,0,0)+0.5)
plot(hour,modelfit$residuals,pch='.',ann=F,ylab='Residuals',axes=F)
axis(1, at = 1:24, labels = 1:24, cex.axis = 1)
axis(2, cex.axis = 1)
mtext(side = 1,text = 'hours',line = 2)


##### Final Regression Model #####

BICtable2 = matrix(c(0),7,10)
for(i in 3:7){
  for(j in 7:10){
    modelfit <- lm(activity~saturday+sunday+poly(bmi,i,raw = T)
                   +poly(age,j,raw = T)+gender)
    BICtable2[i,j] = BIC(modelfit)
  }
}
min(BICtable2[3:7,7:10])

modelfit <- lm(activity~saturday+sunday+poly(bmi,5,raw = T)
               +poly(age,9,raw = T)+gender)
summary(modelfit)
BIC(modelfit)

detach(merged)
