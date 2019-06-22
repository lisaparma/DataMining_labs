# Controllare in che working directory sono
getwd()
# Settarla (oppure su Session > Set working directory)
setwd("~/Scrivania/DataMining_labs")

# Installare una libreriaq
install.packages("ggplot2")
# Utlizzarla nello script
library(ggplot2)

# Assegnazione variabile 
pigreco = 2*pi
pigreco <- 2*pi

# Rimozione variabile
rm(pigreco)

# Rimozione di tutti gli oggetti 
rm(list=ls())

library(MASS)
data(Boston)
## GRAFICI
# Istogramma (1)
hist(Boston$medv,
     xlab = 'Median Value', # label asse x
     main = 'Histogram') # Titolo

# Boxplot (1)
boxplot(Boston$medv,
     xlab = 'Median Value', # label asse x
     main = 'Boxplot') # Titolo

# Scatterplot (2)
plot(Boston$medv, 
     Boston$lstat, # Y
     xlab = '',
     ylab = 'Median value',
     main = 'Dispersion plot', # Titolo
     cex=0.5, # dimensione punto
     pch= 19
)
