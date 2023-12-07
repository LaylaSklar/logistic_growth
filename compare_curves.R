# Script contains code to make a graph comparing the exponential and logistic growth curves 

#install.packages("tidyverse")
library(tidyverse)
  # loads package required here
  # tidyverse includes ggplot2 and dplyr

#setwd("your_path_file_to_experiment1.csv")
#getwd()
  #ensures working directory is set to desired path 

growth_data <- read.csv("experiment1.csv")
  #reads in the data and calls it growth_data

# My parameter estimates 
N0 <- 1020.293
r <- 0.00994
K <- 5.999e+10 #needed for logistic_fun

# Define the function of exponential growth 
exp_fun <- function(t){
  (N = N0*exp(r*t)) 
  return(N)
}

# Define the function of logistic growth 
logistic_fun <- function(t) {  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t)) 
  return(N) 
}

# Plot the functions
plot_unscaled <- ggplot(aes(t,N), data = growth_data) +
  
  geom_function(fun = exp_fun, colour = "skyblue3") +
  
  geom_function(fun = logistic_fun, colour = "red3")

plot_unscaled #prints plot

  # Scales of magnitude differ significantly on the y axis  (cell population N)
    # Result: cannot accurately compare curves; logistic curve appears a horizontal line against the x axis 
    # Solution: log transform the the y axis 
    # note: A limited y-axis would show non-transformed patterns of the data including the typical cuvres for exponential growth and sigmoidal logistic growth.
    # ... However this would also hide a degree of the model output of the exponential growth function beyond the set threshold.

plot_scaled <- plot_unscaled + scale_y_continuous(trans = 'log10')


plot_scaled_final <- plot_scaled + 
  
  labs(x = "time, t (minutes)" , y =" log(Population count, N (number of cells))") +
  
  ggtitle("Plot comparison of exponential and logistic growth curves with log transformation") +
  
  theme_bw() 
  
  
 plot_scaled_final
  # Output: Graph comparing two growth curves 
    #red: logistic growth curve 
    # blue: exponential growth curve
  
  

