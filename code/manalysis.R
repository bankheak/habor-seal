# Anthropogenic noise disturbance on harbor seals

###########################################################################
# Waterfront-Marina GLM analysis
###########################################################################

# Set working directory here
setwd("C:/Users/bankh/My_Repos/habor-seal/data")

# Retrieve data
m.data<-read.csv("m.data.csv")
w.data<-read.csv("w.data.csv")

# Load packages
require(lme4)
require(ggplot2)
require(performance) #for check_collinearity()
require(MuMIn)
require(MASS) #for glm.nb()

###########################################################################
# PART 1: Check Location Significance at Waterfront ---------------------------------------------

head(w.data)

# Check t-test assumptions
## Is noise normal?
hist(w.data$noise[w.data$location == 1])
hist(w.data$noise[w.data$location == 2])
### Both are normal

## Equal variance?
var(w.data$noise[w.data$location == 1])
var(w.data$noise[w.data$location == 2])
### Equal variance

# Run t-test
t.test(w.data$noise[w.data$location == 1], w.data$noise[w.data$location == 2])
## Not significantly different
## Move on-to merging data sets

###########################################################################
# PART 2: Combine Waterfront and Marina Models ---------------------------------------------

# Merge data
full.data<-merge(w.data,m.data,all = T)

# Check distribution
ggplot(full.data, aes(x=seals)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density()+stat_density(alpha=.2,adjust = 1, fill="#FF6666")+xlab("Number of Seals Hauled-out")+ylab("Density")+theme(panel.background = element_blank())

# Check GLM
model1 <- glm.nb(seals ~ scale(j.date) + tide + time + site*noise + month, data = full.data)
summary(model1) #take out j.date,tide and time
model2 <- glm.nb(seals ~ scale(j.date) + site*noise + month, data = full.data)
summary(model2) #everything is significant... this is the best model












#GLM Building Models
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