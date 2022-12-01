# Anthropogenic noise disturbance on harbor seals

###########################################################################
# Marina GLM analysis
###########################################################################

# Set working directory here
setwd("C:/Users/bankh/My_Repos/harbor-seal/data")

# Retrieve data
m.data<-read.csv("m.data.csv")
m.tide<-read.csv("m.tide.csv")

# Load packages
require(lme4)
require(ggplot2)
require(performance) #for check_collinearity()
require(MuMIn)

###########################################################################
# PART 1: Gambit of the group ---------------------------------------------

#Format y-variable
head(m.data)
colnames(m.data)[1] = "seals"
colnames(m.tide)[1] = "seals"

#Check distribution
ggplot(m.data, aes(x=seals)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density()+stat_density(alpha=.2,adjust = 1, fill="#FF6666")+xlab("Number of Seals Hauled-out")+ylab("Density")+theme(panel.background = element_blank())

#Check if good for Poisson
x<-m.data$seals
dispersion_test <- function(x) 
{
  res <- 1-2 * abs((1 - pchisq((sum((x - mean(x))^2)/mean(x)), length(x) - 1))-0.5)
  
  cat("Dispersion test of count data:\n",
      length(x), " data points.\n",
      "Mean: ",mean(x),"\n",
      "Variance: ",var(x),"\n",
      "Probability of being drawn from Poisson distribution: ", 
      round(res, 3),"\n", sep = "")
  
  invisible(res)
}
#Not good, try quasipoisson to correct for overdispersion

#Check Residuals
#Noise
res1 <- resid(mbm3)
plot(m.data$noise, res1)
abline(0,0)
#Julian date
plot(m.data$j.date, res1)
abline(0,0)
#Tide
plot(m.data$tide, res1)
abline(0,0)
#Time
plot(m.data$time, res1)
abline(0,0)

#Check fit
plot(m.data$seals ~ m.data$noise)

#Check for collinarity
plot(m.data$j.date ~ m.data$tide)
check_collinearity(mbm3) #doesn't exist yet 
#Not an issue

#GLMM Building Models
mbm1 <- glm(seals ~ noise+scale(j.date)+tide+time,data = m.data,family = quasipoisson(link='log'))
summary(mbm1) #take out j.date,tide and time
mbm2 <- glm(seals ~ noise, data = m.data,family = quasipoisson(link='log'))
summary(mbm2) #everything is significant... this is the best model

#Running AIC
AIC(mbm1,mbm2)
# model 3 
summary(mbm2) #this the best model 
plot(mbm2)

#sealvnoise graph
ggplot(m.data, aes(noise,seals))+geom_point()+geom_smooth(method = "gam", formula =seals~s(noise))+
  stat_smooth(method="gam",colour="black")+xlab("Average Noise Level (dB)")+ylab("Number of Seals Hauled-out")+
  coord_trans(x = "log10")+theme(panel.background = element_blank())

#Monthvseal graph
p<-ggplot(m.data, aes(x=month,y =seals,group=month))
p+geom_boxplot(fill="black", alpha=0.2)+scale_x_discrete(limits=c("1", "2","3","4","5","6","7","8","9","10","11","12"))+xlab("Month")+ylab("Number of Seals")+theme(panel.background = element_blank())

#Tidevseals
b2<-ggplot(m.tide, aes(x=tide.l,y =seals,group=tide.l))
b2+geom_boxplot(fill="grey")+scale_x_discrete(limits=c("Low", "Med","High"))+xlab("Tide")+ylab("Number of Seals")+theme(panel.background = element_blank())

#tidevmonth
ggplot(m.tide, aes(month, seals, colour = tide.l)) + 
  geom_point()