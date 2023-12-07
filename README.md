# Question 1
#### Included below are the **completed scripts with detailed explanatory annotation** (#) of the three sections (i, ii, iii)
### i) Plotting the bacterial growth data

The data used here is based off a lab culture where an _Ecoli_ bacterial population was mixed with a growth medium (experiment1). Modelling the bacterial growth dynamics generates logistic growth ( with a sigmoidal curve as seen in plot1); a lag phase, exponential phase, and ultimately a stationary phase. The final growth phase exemplifies density dependence as the population growth rate is inhibited by depletion of growth medium resource.

(note: no unit was specified in the csv file for time. Bremer (1982) found a distribution of E.coli generation time between 20 and 60 minutes with an avegerage of 40 minutes in a range of laboratory conditions. As such, I have assumed the time is in minutes)
```R 
## Set up
sink(file = "package-versions.txt")
sessionInfo()
sink()

#Script to plot the logistic growth data

  # Analysis of the csv results file entitled experiment1: the data concerns time in minutes (t) with the cell count (N); this is bacterial growth

growth_data <- read.csv("experiment1.csv") 
  # reads in the data

#install.packages("ggplot2")
library(ggplot2)
  # loads the package necessary to generate plots of the data

#plot 1
 plot1 <- ggplot(aes(t,N), data = growth_data) +  
  geom_point() + 
    # plots the data as a scatter plot  
  xlab("time (minutes)") +
  ylab("bacterial cell number") +
    # adds axes labels  
  ggtitle("Plot showing logistic growth of bacterial cells") +    
   # adds a title   
  theme_bw()  
    # sets the theme of the plot
 plot1
  #prints the plot

#plot 2 (log transform to linearise)
 
plot2 <- ggplot(aes(t,N), data = growth_data) +
  geom_point() +
  xlab("time (minutes)") +
  ylab("log(bacterial cell number)") +
  scale_y_continuous(trans='log10') +
  ggtitle("Plot of log(cell number) over time") +
  theme_bw() 
plot2

  # plot2 gives the initial population size and rate of growth, r
  # there is a horizontal line on tapering after approximately t>1800
    # this line has slope zero (r =0) 
    # y intercept of the horizontal section gives K (the carrying capacity)
      # in a model of logistic growth, after a period of exponential growth, the population growth curve will begin to asymptote towards k
  
  # hence we can use two linear regressions to generate 3 parameters of a linear model; 1) N0, 2) r, 3) k 
```
### ii) Subsetting the data and fitting linear models
  The linear regressions in the script below return the following parameters:
  - log(N0) = 6.927845
    - To back tranform log(N0) to N0, N = exp(6.927845) = 1020.293. 
  -  r = 0.00994
  -  k =  5.999e+10
```R 
#Script to estimate the model parameters using a linear approximation

#install.packages("dplyr")
library(dplyr)
  #loads the necessary package
    #package contains functions used here such as filter() and mutate()

growth_data <- read.csv("experiment1.csv")
  #reads in the data from the csv file 

#Case 1. K >> N0, t is small

data_subset1 <- growth_data %>% filter(t<1700) %>% mutate(N_log = log(N))
  # filters the data, while the growth phase of the cells is exponential
  # (i.e. the  positive linear segment plot2 before the growth curve starts to plateau)

  # N_log will be a new column in the subset of the data
    # the values in this column with be the log transformation of the cell counts in the column N

 # Fit a linear model of the subset data 
model1 <- lm(N_log ~ t, data_subset1)
summary(model1)

model1 #calls the lm() function and prints the key coefficents

# We have the first two parameters (N0 and r)
  # initial population size = 6.927845, rate of growth over t = 0.00994

#Case 2. N(t) = K

data_subset2 <- growth_data %>% filter(t>2300)
  # filters the data, while the growth phase of the cells is stationary
  # # (i.e. the horizontal segment plot2 after the curve has plateaued)

# Fit a linear model of the subset data 
model2 <- lm(N ~ 1, data_subset2)
summary(model2)

model2

# We have the third parameter (the carrying capacity K)
  # k = y intercept = 5.999e+10
```
### iii) Plotting both the data and the model
The final output plot is included below. The close fit of the model of the data illustrates the suitability and high precision of the model.  
```R 
#Script to plot data and model

growth_data <- read.csv("experiment1.csv")
  #reads in the data

logistic_fun <- function(t) {  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t)) 
  return(N) 
}
  # code defines a logistic growth function to carry out calculations with the modeled logistic growth formula of population size N at time t

N0 <- exp(6.927845) # back transformation from log(N0) in model1  
r <- 0.00994 # parameter from model1  
K <- 5.999e+10 # parameter from model 1

  # the above estimates parameters will be input into the formula

plot3 <- ggplot(aes(t,N), data = growth_data) +
  geom_function(fun=logistic_fun, colour="red") +
  labs(x = "time (t) (minutes)", y = "bacterial cell number (N)") +
  ggtitle("Population growth model and experimental data points") +
  geom_point() 
    # the above code plots the results 
    # geom_function() is used to plot the logistic function definition
 plot3 
 
   # Plot3 shows a red curve overlaid with black data points
    # Generated modeled logistic growth population dynamics = red curve
    # Data points for N at time t (from experiment1) = black points
```
#### Graph (plot3) output of script 
<img width="785" alt="image" src="https://github.com/PenguinsAssignment/logistic_growth/assets/150163891/9708da41-eaf7-455b-b4a2-740737858f8f">

# Question 2

### Calculating population size at t  = 4980 min
The code below shows the following at t = 4980: 
- Exponential population size = 3.21235e+24. This is greater than my estimated parameter for K for logistic growth.
- Logistic population size = 5.999e+10. This is equal to my estimate for K (to 2dp on R).

In the logistic growth model, density dependence suppresses N on its approach to K such that it can never surpass K; K is the maximum value of N possible. In the exponential growth model, assumptions of density dependence and finite resource depletion are relaxed, and N, no longer bounded by a carrying capacity, is free to surpass K. 

```R 
# My parameter estimates 
N0 <- 1020.293 
r <- 0.00994
K <- 5.999e+10 #needed for logistic_fun
  
# Define a function of exponential growth 
exp_fun <- function(t){
  (N = N0*exp(r*t)) 
  return(N)
}

exp_N4980 <- exp_fun(4980)
exp_N4980 # = 3.21235e+24

# Recall the definition of the function of logistic growth 
logistic_fun <- function(t) {  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t)) 
  return(N) 
}

logistic_N4980 <- logistic_fun(4980)
logistic_N4980 # = 5.999e+10

exp_N4980 > K #TRUE

logistic_N4980 == K #FALSE
round(logistic_N4980, 2) == round(K, 2) #TRUE (= when rounded to 2dp)
```

# Question 3
### Comparing the exponential and logistic growth curves

#### Link to script in repository
https://github.com/PenguinsAssignment/logistic_growth/blob/main/compare_curves.R

#### Graph output of script 
<img width="785" alt="image" src="https://github.com/PenguinsAssignment/logistic_growth/assets/150163891/72b0e626-eb94-44de-b162-a371c7f64ea8">

(Note: as in script - so as not to mask the scale of the output from the exponential growth function, I applied a log transformation to the y-axis as opposed to reducing the axis to a set limit)

# References
1) Bremer, H. “Variation of Generation Times in Escherichia Coli Populations: Its Cause and Implications.” Microbiology, vol. 128, no. 12, 1 Dec. 1982, pp. 2865–2876, https://doi.org/10.1099/00221287-128-12-2865. Accessed 7 Dec. 2019.
