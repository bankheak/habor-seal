# Anthropogenic noise disturbance on harbor seals

###########################################################################
# Waterfront-Marina GLM analysis
###########################################################################

# Set working directory here
setwd("C:/Users/bankh/My_Repos/habor-seal/data")

# Retrieve data
m.data<-read.csv("m.data.csv")
new.w.data<-read.csv("new.w.data.csv")

# Load packages
require(ggplot2)

###########################################################################
# PART 1: Distribution---------------------------------------------

ggplot(full.data, aes(x=seals)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density()+stat_density(alpha=.2,adjust = 1, fill="#FF6666")+xlab("Number of Seals Hauled-out")+ylab("Density")+theme(panel.background = element_blank())
## Negative binomial or poisson would be the best fit

###########################################################################
# PART 2: Month vs seal graph---------------------------------------------

## Waterfront
p<-ggplot(new.w.data, aes(x=month,y =seals,group=month))
a<- p+geom_boxplot(fill="black", alpha=0.2) +
  xlab("Month")+ylab("Number of Seals Hauled-out")+
  theme(panel.background = element_blank())
a + scale_x_discrete(limits = c(6,7,8,9,10,11),
                     labels=c("Jun", "Jul", "Aug", "Sept", "Oct", "Nov"))

## Marina
j<-ggplot(m.data, aes(x=month,y =seals,group=month))
k<- j+geom_boxplot(fill="black", alpha=0.2)+
  xlab("Month")+ylab("Number of Seals Hauled-out")+
  theme(panel.background = element_blank())
k + scale_x_discrete(limits = c(6,7,8,9,10,11),
                     labels=c("Jun", "Jul", "Aug", "Sept", "Oct", "Nov"))


###########################################################################
# PART 3: Noise graph---------------------------------------------



###########################################################################
# PART 4: Seal vs noise graph---------------------------------------------

## Waterfront
ggplot(new.w.data, aes(noise,seals))+geom_point()+geom_smooth(method = "gam", formula =seals~s(noise))+
  stat_smooth(method="gam",colour="black")+xlab("Average Noise Level (dB)")+ylab("Number of Seals Hauled-out")+
  coord_trans(x = "log10")+theme(panel.background = element_blank())

## Marina
ggplot(m.data, aes(noise,seals))+geom_point()+geom_smooth(method = "gam", formula =seals~s(noise))+
  stat_smooth(method="gam",colour="black")+xlab("Average Noise Level (dB)")+ylab("Number of Seals Hauled-out")+
  coord_trans(x = "log10")+theme(panel.background = element_blank())

