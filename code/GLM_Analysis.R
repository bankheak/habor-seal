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
require(performance)
require(AICcmodavg) #for AICc
require(glmmTMB) # for glmm
require(MASS) # for glm
require(mgcv) # for GAMM
require(remotes)


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
new.w.data$j.date<- as.integer(new.w.data$j.date)
m.data$j.date<- as.integer(.data$j.date)

# Add sample ID
new.w.data$id<- seq_along(new.w.data[,1])
m.data$id<- seq_along(m.data[,1])

# Merge data
full.data<-merge(new.w.data,m.data,all = T)
full.data$site<- as.numeric(full.data$site=="waterfront")
summary(full.data)

## Create csv
write.csv(full.data,"full.data")
full.data<- read.csv("full.data.csv")

# Run pairwise cor between all independent variables
## Cut-off is +/- 0.7
cor.matrix<-cor(full.data[,c(2:4,6:7,9)]) 
# Keep only large correlations in the same model
cor.matrix[abs(cor.matrix)< 0.7]<-NA
cor.matrix
## No colilinearity

# Check distribution
ggplot(full.data, aes(x=seals)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density()+stat_density(alpha=.2,adjust = 1, fill="#FF6666")+xlab("Number of Seals Hauled-out")+ylab("Density")+theme(panel.background = element_blank())

## Does the mean equal the variance?
var(full.data$seals)
mean(full.data$seals)
### Variance is way higher than the mean

## Negative binomial or poisson would be the best fit
mod<- glm.nb(seals ~ site*noise + month, data = full.data)
summary(mod)

## Plot lines
attach(full.data)
plot(noise[site==1],seals[site==1],type="p",pch=paste(1), col="blue", xlim=c(min(noise),max(noise)), ylim=c(min(seals),max(seals)), xlab="Noise",ylab="Seals") 
abline(glm.nb(seals[site==1]~noise[site==1]),col="blue") # add regression line for polymer 1
points(noise[site==0],seals[site==0],type="p",pch=paste(2), col="red") # add points corresponding to polymer 2
abline(glm.nb(seals[site==0]~noise[site==0]),col="red") # add regression line for polymer 2

## Check if overdispersion is detected with full model
check_overdispersion(mod) 

### Check if data is zero inflated 
check_zeroinflation(mod, tolerance = 0.05)
# Plot residuals by predicted values
plot(mod$resid~mod$fitted)
## Values are zero inflated

# Check Temporal Autocorrelation
## Organize data by id
full.data<- full.data[order(full.data$id), ]
## Run acf for both direct and indirect effects
acf(full.data$seals[full.data$site == "waterfront"]) # Highly autocorrelated at lag of 1
acf(full.data$seals[full.data$site == "marina"]) # Autocorrelated at lag of 7


###########################################################################
# PART 3: Run GAMs and QIC---------------------------------------------
  
# Run GAMs
model1<- zinb(list(seals ~ noise * site,
                  ~ 1), data = full.data, family = ziplss())
summary(model1) 
model2<- gam(seals ~ s(noise, by = site) + s(month) + s(tide) + s(time), 
                data = full.data, family = nbinom2(), method = "REML")
summary(model2) 
model3<- gam(seals ~ s(noise, by = site) + s(month) + s(time), 
             data = full.data, family = ziplss, method = "REML")
summary(model3) 
model4<- gam(seals ~ s(noise, by = site) + s(month), 
             data = full.data, family = ziplss, method = "REML")
summary(model4) 

# Create list
models<-list(model1, model2, model3, model4)

## Calculate AICc with glm of models
Modnames<- c("seals ~ 1",
             "seals ~ site*noise + month + tide + time",
             "seals ~ site*noise + month + time",
             "seals ~ site*noise + month")

qictable<- QIC(model1, model2, model3, model4)
aictable<- aictab(models, modnames = Modnames,
                  second.ord = TRUE, 
                  nobs = NULL, c.hat = 1) # Looks like site*noise + month + time are the best predictors

# Find significant predictors and summary of model3
summary(model3)


###########################################################################
# PART 4: Run Predict Function---------------------------------------------

# Use model to predict the response variable 
newdata <- data.frame(noise = mean(full.data$noise), 
                       site = factor(1:2, levels = 1:2, 
                                      labels = unique(full.data$site)))
m3<- glm.nb(seals ~ site * noise, data = full.data)
newdata$phat <- predict.glm(m3, newdata, type = "response")
newdata

# Plot 
newdata2 <- data.frame(
  noise = rep(seq(from = min(full.data$noise), to = max(full.data$noise), length.out = 100), 2),
  site = factor(rep(1:2, each = 100), levels = 1:2, labels =
                  unique(full.data$site)))

newdata2 <- cbind(newdata2, predict(m3, newdata2, type = "link", se.fit=TRUE))
newdata2 <- within(newdata2, {
  seals <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

write.csv(newdata2, "newdata2.csv")

ggplot(newdata2, aes(noise, seals)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = site), alpha = .25) +
  geom_line(aes(colour = site), linewidth = 2) +
  labs(x = "Noise Level (dB)", y = "Predicted Number of Seals Hauled-out")

