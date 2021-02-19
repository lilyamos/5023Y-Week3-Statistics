library(tidyverse)
library(skimr)

### import and check data
wood <- read.csv("Data/wood_density.csv")
skim(wood)

### plots linear model of data with 95% Cl.
wood %>% 
  ggplot(aes(x=Density,
             y=Hardness))+
  geom_point()+
  geom_smooth(method="lm")

### generates linear model stats to make predictions about the change in hardness
### for every unit change in wood density.
density_model <- lm(Hardness~Density, data=wood)
density_model
### so Hardness=−1160.5+57.51(Density)

### calculates predicted hardness for density of 24.7
-1160.5+(24.7*57.507)

### uses the coefficients of the model directly to calculate predicted hardness 
### for density of 24.7
coef(density_model)[1]+
  coef(density_model)[2]*
  24.7

### estimates the dependent variable based on each independent value.
fitted(density_model)

### calculate residuals: difference between model-fitted values and observed values
484-259.9152
427-265.6658
413-409.4325

### add the models predictions and residuals onto original dataframe
wood2 <- wood %>% 
         mutate(predicted= fitted(density_model)) %>% 
         mutate(residuals=Hardness-predicted)

### plots the observed Timber Hardness (p1), predicted Hardness value (p2) and 
### the difference between these two values (p3)
p1 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_line()+
  ggtitle("Full Data")

p2 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=predictions))+
  geom_line()+
  ggtitle("Linear trend")

p3 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=residuals))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining pattern")

p1+p2+p3
### A perfect model would produce flat residual line. Any patterns in our 
### residuals are what is left-over after taking away the pattern explained by
### the linear model.

### Constructs one-row summary of the model. This typically contains values such
### as R^2, adjusted R^2, F values, df and P
broom::glance(density_model)

### Constructs a small tibble with most of the models summary data in it
broom::tidy(density_model, conf.int=TRUE)

### Takes computations from model fit and adds them back onto original dataframe
broom::augment(density_model, wood, interval="confidence") 

plot1 <- broom::augment(density_model, wood, interval="confidence") %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_line(aes(x=Density, y=.fitted))+
  geom_line(aes(x=Density, y=.upper), linetype="dashed")+
  geom_line(aes(x=Density, y=.lower), linetype="dashed")+
  geom_point()+
  ggtitle("Manually fitting linear model \n and confidence intervals")

plot2 <- wood %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_smooth(method=lm)+
  geom_point()+
  ggtitle("Geom smooth method to plotting \n a linear model")

plot1+plot2

### write up
### as wood density increases by one pound per cubic foot, the hardness increases
### by 57.5 units in the janka scale (F1,34=637, P<0.001, R^2=0.95) 