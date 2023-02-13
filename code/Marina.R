# Anthropogenic noise disturbance on harbor seals

###########################################################################
# Marina analysis
###########################################################################

# Set working directory here
setwd("C:/Users/bankh/My_Repos/habor-seal/data")

# Retrieve data
full.mdata<-read.csv("full.mdata.csv")

# Load packages
require(ggplot2)
require(performance) # for overdispersion
require(AICcmodavg) #for AICc
require(MASS) # for glm
require(DHARMa) # auto cor and zero-inflation

###########################################################################
# PART 1: Find distribution---------------------------------------------

# Run pairwise cor between all independent variables
## Cut-off is +/- 0.7
cor.matrix<-cor(full.mdata[,c(2:4,6:7)]) 
# Keep only large correlations in the same model
cor.matrix[abs(cor.matrix)< 0.7]<-NA
cor.matrix
## No colilinearity

# Check distribution
ggplot(full.mdata, aes(x=seals)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density()+stat_density(alpha=.2,adjust = 1, fill="#FF6666")+xlab("Number of Seals Hauled-out")+ylab("Density")+theme(panel.background = element_blank())
## Poisson or NB would be the best fit

## Does the mean equal the variance?
var(full.mdata$seals)
mean(full.mdata$seals)
### Variance is way higher than the mean

## Check if overdispersion is detected with full model
check_overdispersion(mod) 

### Check if data is zero inflated 
testZeroInflation(simulationOutput) 
### Data is zero-inflated

###########################################################################
# PART 2: Run GLM and AICc ---------------------------------------------
# Check GLM
m.mod1<- glm(seals ~ 1, data = full.mdata, family = quasipoisson)
summary(m.mod1) 
m.mod2<- glm(seals ~ noise + month + tide + time, data = full.mdata, family = quasipoisson)
summary(m.mod2) 
m.mod3<- glm(seals ~ noise + month + time, data = full.mdata, family = quasipoisson)
summary(m.mod3) 
m.mod4<- glm(seals ~ noise + month, data = full.mdata, family = quasipoisson)
summary(m.mod4) 

# Create list
m.mods<-list(m.mod4, m.mod4, m.mod4, m.mod4)

## Find the AICc function
source("../code/Functions.R")

## Calculate AICc with glm of models
AICc(m.mods) # Looks like site*noise + month + time are the best predictors

summary(model3)