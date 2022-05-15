---
title: "Irene Hsueh's BS 806 Homework 5"
author: "Irene Hsueh"
date: "10/8/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(car)
```

### Preparing Dataset
```{r}
pulse <- read.csv("C:/Irene Hsueh's Documents/MS Applied Biostatistics/BS 806 - Multivariate Analysis for Biostatisticians/Class 5 - Regression Diagnostics/Homework 5/pulsedata.csv")
head(pulse)
```



### Multiple Linear Regression
```{r}
linear_model <- lm(Pulse1 ~ Height + Weight + Age + factor(Sex) + factor(Smokes) + factor(Alcohol) + factor(Exercise), data=pulse)
summary(linear_model)

#Residual Plots
plot(fitted(linear_model), residuals(linear_model), 
     xlab="Fitted", 
     ylab="Residuals", 
     pch=20, 
     col="hotpink")
plot(linear_model, col="hotpink")
residualPlots(linear_model)
influenceIndexPlot(linear_model)
```



### Normality Assumption 
```{r}
#Q-Q Plot
qqnorm(residuals(linear_model), 
       ylab="Residuals", 
       pch=20, 
       col="hotpink")
qqline(residuals(linear_model), 
       lwd = 3, 
       col="mediumpurple1")

#Q-Q Plot with 95% Confidence Interval
qqPlot(residuals(linear_model), 
       pch=20, 
       col="hotpink")

#Histogram
hist(linear_model$residuals, col="hotpink")


#Hypothesis Test for Normality Assumption
shapiro.test(linear_model$residuals)
```



### Outliers
```{r}
#Threshold for Rejection 
jackknife <- rstudent(linear_model)
n <- length(jackknife)
p_prime <- 8
qt(0.05/(n*2), df = n-p_prime-1 , lower.tail=FALSE)  

#Checking Points Greater Than Threshold 
outliers <- abs(jackknife) > abs(qt(0.05/(n*2), df = n-p_prime-1, lower.tail=FALSE))
as.numeric(outliers)
which(as.numeric(outliers)==1)

#Maximum Studentized Residuals
jackknife[which.max(abs(jackknife))]
pulse[47,]
```



### Influential Points 
```{r}
#Cook's Distance
cooks_distance <- cooks.distance(linear_model)

#Rule of Thumb
cook_ruleofthumb <- cooks_distance[cooks_distance > 4/n]
sort(cook_ruleofthumb, decreasing=TRUE) [1:10]  

#Check Di > 0.5
cooks_distance[cooks_distance>0.5]

#Outliers Percentile Value
cooks_distance[(pf(cooks_distance, p_prime, n-p_prime)>0.5)] 

pulse[76,]
```


### Question 5 - Leverage Points
```{r}
leverage <- hatvalues(linear_model)
leverage_ruleofthumb <- leverage[leverage > 2*p_prime/n]
sort(leverage_ruleofthumb, decreasing=TRUE) [1:10]  

pulse[76,]
pulse[62,]
pulse[56,]
pulse[80,]
pulse[17,]
```



