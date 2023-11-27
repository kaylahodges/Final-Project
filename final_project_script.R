## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################

# EXAMINE QUANT_VAR1
table(data$Tempo)
mean(data$Tempo)
sd(data$Tempo)
summary(data$Tempo)

# EXAMINE QUANT_VAR2
table(data$Song_Length)
mean(data$Song_Length)
sd(data$Song_Length)
summary(data$Song_Length)

# EXAMINE QUAL_VAR1
table(data$`Love_Type `)

# EXAMINE QUAL_VAR2
table(data$`Emotional_Tone `)


##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################

table(data$`Love_Type `, data$`Emotional_Tone `)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(table(data$`Love_Type ` , data$`Emotional_Tone `))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
# Perform ANOVA
anova_adapted <- aov(Tempo ~ `Emotional_Tone `, data = data)
# Summarize ANOVA results
summary(anova_adapted)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
cor(data$Tempo,data$`Emotional_Tone `)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(Tempo ~ Song_Length, data = data)
summary(linear_relationship)
##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
plot(Tempo~ Song_Length, data = data)
abline(linear_relationship, col = "red")
abline(h=118.1951, col = 'purple')
abline(v=3.124634, col = 'pink')
###############################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Song_Length, residuals(linear_relationship))
abline(h= 0, col = 'red')