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
require(ggplot2)
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
## Create csv
write.csv(full.data,"full.data")
full.data<- read.csv("full.data.csv")

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
## Negative binomial or poisson would be the best fit

## Check which one would fit better 
m1<- glm.nb(seals ~ 1, data = full.data)
m3<- glm(seals ~ 1, data = full.data, family = "poisson")
pchisq(2 * (logLik(m1) - logLik(m3)), df = 1, lower.tail = FALSE)
### Negative binomial would be the best fit

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

# Create list
models<-list(model1, model2, model3, model4)

## Find the AICc function
source("../code/Functions.R")

## Calculate AICc with glm of models
AICc(models) # Looks like site*noise + month + time are the best predictors

summary(model3)

# What does the interaction between site and noise look like?
interaction.plot(x.factor = full.data$noise, #x-axis variable
                 trace.factor = full.data$site, #variable for lines
                 response = full.data$seals, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Number of Seals Hauled-out",
                 xlab = "Noise Level (dB)",
                 col = c("pink", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Site")

###########################################################################
# PART 4: Run Diagnostics---------------------------------------------

# Use model to predict the response variable 
newdata <- data.frame(noise = mean(full.data$noise), 
                       site = factor(1:2, levels = 1:2, 
                                      labels = unique(full.data$site)))
m3<- glm.nb(seals ~ site * noise, data = full.data)
newdata$phat <- predict.glm(m3, newdata, type = "response")
newdata

# Plot on top of raw data
effect_plot(model3, pred = noise, interval = TRUE, partial.residuals = TRUE,
            jitter = c(0.1, 0), x.label = "Average Noise Level (dB)", 
            y.label = "Number of Seals Hauled-out")

# Plot residuals by predicted values
plot(model3$resid~model3$fitted)
## Values are zero inflated
