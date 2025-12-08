setwd("C:/Users/karin/Downloads/CA1_DataPreparation/dataset_mathematics_in_higher_education")

#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("mosaic") 
#install.packages("fastDummies")
#install.packages("factoextra")
#install.packages("reshape2")

library(mosaic)
library(fastDummies)
library(tidyverse)
library(dplyr)
library(factoextra)
library(reshape2)

# Check if my dataset is in the working directory
list.files() 

# Use 'read_delim()' for more controle when using "," as decimal and "." as grouping mark to load the dataset
math_data <- read_delim("Math_dataset.csv", delim = ";", trim_ws = TRUE)

# Preview data, variables names, and dimensions
head(math_data)
colnames(math_data)
dim(math_data)

# Data type - Convert text columns to factors (categorical)
math_data$`Student Country` <- as.factor(math_data$`Student Country`)
math_data$`Question Level`  <- as.factor(math_data$`Question Level`)
math_data$Topic             <- as.factor(math_data$Topic)
math_data$Subtopic          <- as.factor(math_data$Subtopic)
math_data$Keywords          <- as.factor(math_data$Keywords)

# Structure of the dataset.
str(math_data)

# Check missing values in each column.
colSums(is.na(math_data))

# No missing values. If missing values exist, I could replace numeric values with mean or median.

# Descriptive Statistics
#General summary of all columns
summary(math_data)

# Using frequency tables to show how many times each category appears.
table(math_data$`Student Country`)
table(math_data$`Question Level`)
table(math_data$`Topic`)

# Inspecting numeric Variables to understand range, scale and distribution
summary(math_data$`Student ID`)
summary(math_data$`Question ID`)
summary(math_data$`Type of Answer`)

# To make the analysis clear, I created a new variable called AnswerCorrectness
# Create a variable (1= correct, 0= incorrect)
math_data$AnswerCorrectness <- math_data$`Type of Answer`

# Inspection performance distribution
summary(math_data$AnswerCorrectness)
table(math_data$AnswerCorrectness)