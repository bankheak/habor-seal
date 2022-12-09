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
require(MuMIn) # For dredge function
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
# PART 2: Combine Waterfront and Marina Models and find distribution---------------------------------------------

# Combine location avg noise and total seals within dates
new.w.data<-read.csv("new.w.data.csv")

# Fix date
new.w.data$date<-as.Date(as.character(new.w.data$date),format = "%m/%d/%Y")
m.data$date<-as.Date(as.character(m.data$date),format = "%m/%d/%Y")

# Merge data
full.data<-merge(new.w.data,m.data,all = T)
summary(full.data)
full.data$time<-as.numeric(full.data$time)
full.data$month<-as.numeric(full.data$month)

# Run pairwise cor between all independent variables
## Cut-off is +/- 0.7
cor.matrix<-cor(full.data[,c(2:4,6:7)]) 
# Keep only large correlations in the same model
cor.matrix[abs(cor.matrix)< 0.7]<-NA
cor.matrix
## No colilinearity

# Check distribution
ggplot(full.data, aes(x=seals)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density()+stat_density(alpha=.2,adjust = 1, fill="#FF6666")+xlab("Number of Seals Hauled-out")+ylab("Density")+theme(panel.background = element_blank())
## Negative binomial would be the best fit

###########################################################################
# PART 3: Run GLM and AICc---------------------------------------------

# Check GLM
model1<- glm.nb(seals ~ 1, data = full.data)
summary(model1) 
model2<- glm.nb(seals ~ site*noise + month + tide + time, data = full.data)
summary(model2) 
model3<- glm.nb(seals ~ site*noise + month + time, data = full.data)
summary(model3) 
model4<- glm.nb(seals ~ site*noise + month, data = full.data)
summary(model4) 

models<-list(model1, model2, model3, model4)

## Calculate AICc with glm of models
n = length(models[[1]]$fitted)
AIC.table = c() # Make a place for summary table
for(i in 1:4) {
  AIC.table<-
      rbind(AIC.table, c(n, models[[i]]$rank + 1, models[[i]]$aic + (2*K*(K+1))/(n-K-1)))
}
colnames(AIC.table)<- c("N","df","AICc")
rownames(AIC.table)<- c("seals ~ 1",
                        "seals ~ site*noise + month + tide + time",
                        "seals ~ site*noise + month + time",
                        "seals ~ site*noise + month")

### Model 1
aic = model1$aic
n = length(model1$fitted)
K = model1$rank + 1
AIC.c <- aic + (2*K*(K+1))/(n-K-1)
mod1.sel = c(n,K,AIC.c)

### Model 2
aic = model2$aic
n = length(model2$fitted)
K = model2$rank + 1
AIC.c <- aic + (2*K*(K+1))/(n-K-1)
mod2.sel = c(n,K,AIC.c)

### Model 3
aic = model3$aic
n = length(model3$fitted)
K = model3$rank + 1
AIC.c <- aic + (2*K*(K+1))/(n-K-1)
mod3.sel = c(n,K,AIC.c)

### Model 4
aic = model4$aic
n = length(model4$fitted)
K = model4$rank + 1
AIC.c <- aic + (2*K*(K+1))/(n-K-1)
mod4.sel = c(n,K,AIC.c)

## Combine AICc for models
AIC.table<-rbind(mod1.sel,mod2.sel,mod3.sel,mod4.sel)
colnames(AIC.table)<- c("N","df","AICc")
rownames(AIC.table)<- c("seals ~ 1",
                        "seals ~ site*noise + month + tide + time",
                        "seals ~ site*noise + month + time",
                        "seals ~ site*noise + month")

###########################################################################
# PART 4: Run Diagnostics---------------------------------------------

# Plot residuals by predicted values
plot(model$resid~model$fitted) # Plot residual vs. predicted values
plot(model$resid~full.data$noise)
plot(model$resid~full.data$month)
## All values are zero inflated

