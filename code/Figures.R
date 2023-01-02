# Anthropogenic noise disturbance on harbor seals

###########################################################################
# Waterfront-Marina GLM analysis
###########################################################################

# Set working directory here
setwd("C:/Users/bankh/My_Repos/habor-seal/data")

# Retrieve data
m.data<-read.csv("m.data.csv")
new.w.data<-read.csv("new.w.data.csv")
full.data<-read.csv("full.data.csv")
newdata2<- read.csv("newdata2")

# Load packages
require(ggplot2)
require(MASS)

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

x.order <- c('Waterfront', 'Marina')
ggplot(full.data, aes(x = site, y = noise)) +
  geom_violin(fill = "grey") + geom_boxplot(width = .2) +
  xlab("Site")+ylab("Average Noise Level (dB)") +
  scale_x_discrete(limit = c("waterfront", "marina"),labels = c("Waterfront","Marina")) +
  theme(panel.background = element_blank())


###########################################################################
# PART 4: Seal vs noise graph---------------------------------------------

ggplot(newdata2, aes(noise, seals)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = site), alpha = .25) +
  geom_line(aes(colour = site, linetype = site), linewidth = 1) +
  labs(x = "Noise Level (dB)", y = "Predicted Number of Seals Hauled-out") +
  theme(panel.background = element_blank())

