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
# PART 5: Graphs---------------------------------------------

# Seal vs noise graph
## Waterfront
ggplot(new.w.data, aes(noise,seals))+geom_point()+geom_smooth(method = "gam", formula =seals~s(noise))+
  stat_smooth(method="gam",colour="black")+xlab("Average Noise Level (dB)")+ylab("Number of Seals Hauled-out")+
  coord_trans(x = "log10")+theme(panel.background = element_blank())

## Marina
ggplot(m.data, aes(noise,seals))+geom_point()+geom_smooth(method = "gam", formula =seals~s(noise))+
  stat_smooth(method="gam",colour="black")+xlab("Average Noise Level (dB)")+ylab("Number of Seals Hauled-out")+
  coord_trans(x = "log10")+theme(panel.background = element_blank())


# Month vs seal graph
## Waterfront
p<-ggplot(new.w.data, aes(x=month,y =seals,group=month))
p+geom_boxplot(fill="black", alpha=0.2)+
  scale_x_discrete(limits=c("1","2","3","4","5","6"))+
  xlab("Month")+ylab("Number of Seals")+theme(panel.background = element_blank())

## Marina
p<-ggplot(m.data, aes(x=month,y =seals,group=month))
p+geom_boxplot(fill="black", alpha=0.2)+
  scale_x_discrete(limits=c("Jun","Jul","Aug","Sept","Oct","Nov"))+
  xlab("Month")+ylab("Number of Seals")+theme(panel.background = element_blank())

