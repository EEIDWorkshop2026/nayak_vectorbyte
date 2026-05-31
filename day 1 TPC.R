### VECTORBYTE TRAINING
## Fitting Thermal Performance Curves via Non-Linear Least Squares
# Arpita Nayak

#Install libraries
library(tidyverse)
library(rgl)

# Put some data in here
x <- c(1,2,3,4)
y <- c(9.5, 11, 19.6, 20)

# Plot it - looks kind of like a line
plot(x, y)

#Let's look through a few parameter values for the slope and see what the result looks like

# Make a search grid of parameter values
beta0 <- seq(0,10,0.1)
beta1 <- seq(0,10,0.1)
betas <- expand.grid(beta0 = beta0, 
                     beta1 = beta1)

# Get SSR values for each beta set
for (i in 1:nrow(betas)){
  
  beta0 <- betas$beta0[i]
  beta1 <- betas$beta1[i]
  SSR <- sum((y - (beta0 + beta1*x))^2)
  
  betas$SSR[i] <- SSR
  
}

# Plot the SSR for each pair of values
plot3d( 
  x=betas$beta0, y=betas$beta1, z=betas$SSR, 
  type = 's', 
  radius = 5)
rglwidget()

# Surfaces are difficult to visualize, so let's break it down into   # the components for ease
betas_0 <- betas %>%group_by(beta0) %>% summarize(SSRmin = min(SSR))
betas_1 <- betas %>%group_by(beta1) %>% summarize(SSRmin = min(SSR))

# Notice that the minimum SSR happens when beta0 = 5
plot(betas_0$beta0, betas_0$SSRmin)

# Notice that the minimum SSR happens when beta1 = 4
plot(betas_1$beta1, betas_1$SSRmin)

# We get the same result using the lm() function that uses OLS to 
# find the parameter values
summary(lm(y ~ x))




# Run the API file to load in all the functions we need
source("C:/VecTraits Practice/VecTraits_Dataset_Access.R")

mosquito_df <- getDataset(578) #this returns a list of data frames in case we ask for several data sets.
df <- mosquito_df[[1]] #we can extract our data frame like this

# Make a data set of the aggregated mean values
development_rate_mean <- df %>% 
  filter(SecondStressorValue == 165) %>%
  group_by(Interactor1Temp) %>%
  summarise(Trait = mean(1 / OriginalTraitValue), .groups = "drop") %>%
  mutate(curve_ID = factor(1), Temp = Interactor1Temp)

# Make a dataset of the individual-level values
development_rate_individuals <- df %>%
  filter(SecondStressorValue == 165) %>%
  mutate(curve_ID = factor(2),
         Temp = Interactor1Temp,
         Trait = 1 / OriginalTraitValue)

# Now we can examine both the individual-level and mean data. 
# The means seem to be fairly central to the data clusters, 
# so that might lead us to believe that modeling the mean values may not be so bad after all.
ggplot() +
  geom_jitter(data = development_rate_individuals,
              aes(Temp, Trait),
              size = 2, shape = 21, fill = "black", col = "white",
              width = 0.12) +
  geom_point(data = development_rate_mean,
             aes(Temp, Trait),
             size = 3, shape = 22, colour = "black", fill = "red") +
  theme_bw()

# If we don't provide starting values, the nls() function often
# has trouble finding optimal parameter values.
briere <- nls(Trait ~ a*Temp*(Temp-tmin)*(tmax-Temp)^(1/2),
              data = development_rate_individuals)

# Our temperature range in this data set is ~22-35C so let’s try those for tmin and tmax. 
# For a, we will just start at 1.
briere <- nls(Trait ~ a*Temp*(Temp-tmin)*(tmax-Temp)^(1/2),
              start = list(a = 1, tmin = 22, tmax = 35),
              data = development_rate_individuals)

# Full summary
summary(briere)

# Just the parameters
coef(briere)

# So how do we know we’ve found the best possible values? 
# We can test out more starting values and compare the results!

# Does not converge
briere2 <- nls(Trait ~ a*Temp*(Temp-tmin)*(tmax-Temp)^(1/2),
               start = list(a = 5, tmin = 10, tmax = 60),
               data = development_rate_individuals)

# Converges and gives almost identical result
briere3 <- nls(Trait ~ a*Temp*(Temp-tmin)*(tmax-Temp)^(1/2),
               start = list(a = 0.01, tmin = 30, tmax = 40),
               data = development_rate_individuals)
coef(briere)

coef(briere3)

# Does not converge - looks like the tmax starting value is too high.
briere4 <- nls(Trait ~ a*Temp*(Temp-tmin)*(tmax-Temp)^(1/2),
               start = list(a = 0.01, tmin = 15, tmax = 60),
               data = development_rate_individuals)

# Let’s check the residual plots to see if our assumptions are well met.
resBriere <- residuals(briere)
hist(resBriere) #Mostly normal around 0 - good!
plot(resBriere, predict(briere)) #No clear pattern - good!

# Generate predicted values to graph
tempDat <- data.frame(Temp = 
                        seq(min(development_rate_individuals$Temp),
                            max(development_rate_individuals$Temp),
                            length.out = 100))

d_preds <- predict(briere, newdata = tempDat)
tempDat$preds <- d_preds

# Graph briere model with NLLS fitting (estimates model parameters) against data
ggplot() +
  geom_jitter(data = development_rate_individuals,
              aes(Temp, Trait),
              size = 2, shape = 21, fill = "black", col = "white",
              width = 0.12) +
  geom_point(data = development_rate_mean,
             aes(Temp, Trait),
             size = 3, shape = 22, colour = "black", fill = "red") +
  geom_line(data = tempDat,
            aes(x = Temp, y = preds), color = "blue")+
  theme_bw()

# Extract confidence intervals
confint(briere)

# may want to be able to visualize the uncertainty across the whole TPC
# Bootstrapping involved re-sampling the data with replacement and re-fitting the model 
# to the samples to be able to generate multiple predictions for each data point 
# (one from each of the re-fitted models with their unique parameter estimates)
library("car") #houses the Boot() function for regression models

# Re-sample the data set 1000 times and generate 1000 sets of 
# parameter estimates (R = 1000)
boot1 <- Boot(briere, method = 'case', R = 100)

# Let's check out the data set of parameter values
head(boot1$t)

# We can plot the distributions using hist()
hist(boot1, c(2,2))

# We successfully re-sampled our data and generated new parameters for each sample. 
# Now, let’s see what they look like around our original curve.

# Now we'll generate a data set that has all the simulated prediction
# values.
temperature <- development_rate_individuals$Temp

boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>% # Add a column for the iteration number
  group_by_all() %>% # Group by the iteration number
  do(data.frame(temp = seq(min(temperature),
                           max(temperature),
                           length.out = 100))) %>% # For each                                   # iteration number, make a sequence of                               #  temperatures to predict across
  ungroup() %>% # Release a really long data frame
  mutate(pred = a*temp*(temp-tmin)*(tmax-temp)^(1/2))


# calculate bootstrapped confidence intervals for plotting
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs - we will save this for the next activity.
(ind <- ggplot() +
    geom_line(aes(Temp, preds), tempDat, col = 'blue') +
    geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
    geom_jitter(aes(Temp, Trait), 
                development_rate_individuals, size = 2, 
                width = 0.12, shape = 21, alpha = 0.5,
                fill = "black", color = "white") +
    geom_point(data = development_rate_mean,
               aes(Temp, Trait),
               size = 3, shape = 22, colour = "black", fill = "red") +
    theme_bw(base_size = 12) +
    labs(x = 'Temperature (ºC)',
         y = 'Growth rate',
         title = 'Growth rate across temperatures'))