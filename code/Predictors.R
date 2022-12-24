# Anthropogenic noise disturbance on harbor seals

###########################################################################
# Number of Seals Hauled-out Compared to Canidate Model Predictors
###########################################################################

# Set working directory here
setwd("C:/Users/bankh/My_Repos/habor-seal/data")

# Retrieve data
m.data<-read.csv("m.data.csv")
new.w.data<-read.csv("new.w.data.csv")

# Load packages
require(ggplot2)
require(MASS) #for glm.nb()


###########################################################################
# PART 1: Check Month Significance on Seals ---------------------------------------------

## Waterfront
length(new.w.data$seals[new.w.data$month == 7])
summary(new.w.data$seals[new.w.data$month == 7])
IQR(new.w.data$seals[new.w.data$month == 7])

## Marina
length(m.data$seals[m.data$month == 8])
summary(m.data$seals[m.data$month == 8])
IQR(m.data$seals[m.data$month == 8])

###########################################################################
# PART 2: Check Time of Day Significance on Seals ---------------------------------------------

# Waterfront
p<-ggplot(new.w.data, aes(x = time, y = seals, group = time))
p+geom_boxplot(fill="black", alpha=0.2)+
  xlab("Time of Day")+ylab("Number of Seals")+theme(panel.background = element_blank())
## IQR
length(new.w.data$seals[new.w.data$time == 16])
summary(new.w.data$seals[new.w.data$time == 16])
IQR(new.w.data$seals[new.w.data$time == 16])

# Marina
j<-ggplot(m.data, aes(x = time, y = seals, group = time))
j+geom_boxplot(fill="black", alpha=0.2)+
  xlab("Time of Day")+ylab("Number of Seals")+theme(panel.background = element_blank())
## IQR
length(m.data$seals[m.data$time == 15])
summary(m.data$seals[m.data$time == 15])
IQR(m.data$seals[m.data$time == 15])