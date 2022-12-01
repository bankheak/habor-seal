#Retrieve data
setwd("C:/Users/bankh/OneDrive/Documents/Homework/!!!SEALS RESEARCH")
w.data<-read.csv("w.data.csv")
w.tide<-read.csv("w.tide.csv")

#Get packages
require(lme4)
require(ggplot2)
require(performance) #for check_collinearity()
require(AICcmodavg) #I've never seen this package... I use AIC() from the stats package, would recommend that route
library(MASS) #for glm.nb()

#Check distribution
ggplot(w.data, aes(x=seals)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density()+stat_density(alpha=.2,adjust = 1, fill="#FF6666")+xlab("Number of Seals Hauled-out")+ylab("Density")+theme(panel.background = element_blank())

#Check Residuals
#Noise
res2 <- resid(wbm3)
plot(w.data$noise, res2)
abline(0,0)
#Julian date
plot(w.data$month, res2)
abline(0,0)
#Location
plot(w.data$location, res2)
abline(0,0)
#Tide
plot(w.data$tide, res2)
abline(0,0)
#Time
plot(m.data$time, res1)
abline(0,0)

#Check fit
plot(w.data$seals ~ w.data$noise)

#Check for collinarity
plot(w.data$j.date ~ w.data$tide)
check_collinearity(wbm5) #doesn't exist yet 
#Not an issue

#Format y-variable
head(w.data)
head(w.tide)
colnames(w.data)[1] = "seals"
colnames(w.tide)[1] = "seals"

#GLMM Building Models
wbm1 <- glm.nb(seals ~ noise*j.date+tide+time+location,data = w.data)
summary(wbm1) 
# in this case, the interaction is not sig so we'll start by removing that. 
wbm2 <- glm.nb(seals ~ noise + j.date + tide + time + location, data = w.data)
summary(wbm2) #noise is insig and should be dropped
wbm3 <- glm.nb(seals ~ j.date+ tide + time + location, data = w.data)
summary(wbm3) #everything is significant... check some interactions as they make sense
wbm4 <- glm.nb(seals ~ j.date + tide*location + time, data = w.data)#I'm interested in how the tide might affect number of seals at the two different locations within the waterfront
summary(wbm4) #not significant
wbm5 <- glm.nb(seals ~ j.date*tide + location + time, data = w.data)#It's possible the effect of tide depends on the date
summary(wbm5) #this is significant but only marginally so, that said, in a small dataset like this, that's pretty good

#Running AIC
AIC(wbm2,wbm3,wbm5)
# model 3 and 5 are the same but model 5 is more complicated (has that multiplicative factor). 
summary(wbm5) #this the best model 
plot(wbm5)

#sealvnoise graph
ggplot(w.data, aes(noise,seals)) + geom_point() + geom_smooth(method = "gam", formula =seals~s(noise))+
  geom_point()+xlab("Average Noise Level (dB)")+ylab("Number of Seals Hauled-out")+coord_trans(x = "log10")+theme(panel.background = element_blank())

#Monthvseal graph
p<-ggplot(w.data, aes(x=month,y =seals,group=month))
p+geom_boxplot(fill="grey", alpha=0.2)+scale_x_discrete(limits=c("1", "2","3","4","5","6","7","8","9","10","11","12"))+xlab("Month")+ylab("Number of Seals")+theme(panel.background = element_blank())

#Tidevseals
b<-ggplot(w.tide, aes(x=tide.l,y =seals,group=tide.l))
b+geom_boxplot(fill="grey")+scale_x_discrete(limits=c("Below 0", "0-0.75m","0.75-1.5m","1.5m+"))+xlab("Tide")+ylab("Number of Seals")+theme(panel.background = element_blank())

#tidevmonth
ggplot(w.tide, aes(month, seals, colour = tide.l)) + 
  geom_point()
