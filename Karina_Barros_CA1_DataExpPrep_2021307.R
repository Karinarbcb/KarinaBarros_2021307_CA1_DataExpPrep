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

# Country-level performnce (AnswerCorrectness by Student Country)
perf_country <- math_data %>%
  group_by(`Student Country`) %>%
  summarise(
    n_responses      = n(),
    mean_correctness = mean(AnswerCorrectness, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_correctness))

perf_country

# Mean correctness by country and topic (for Heatmap)
perf_topic_country <- math_data %>%
  group_by(`Student Country`, Topic) %>%
  summarise(
    n_responses      = n(),
    mean_correctness = mean(AnswerCorrectness, na.rm = TRUE),
    .groups = "drop"
  )

# Global mean correctness (for colour midpoint)
global_mean <- mean(math_data$AnswerCorrectness, na.rm = TRUE)

# Heatmap of country x topic
ggplot(perf_topic_country,
       aes(x = Topic, y = `Student Country`, fill = mean_correctness)) +
  geom_tile(color = "grey30") +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "darkgreen",
    midpoint = global_mean,
    name = "Correctness"
  ) +
  labs(
    title = "Heatmap of Average AnswerCorrectness by Country and Topic",
    x = "Topic",
    y = "Student Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)
  )


# Country colors for plots
country_colors <- c(
  "Portugal"           = "#6A0DAD",  # Purple
  "Lithuania"          = "#228B22",  # Dark green
  "Italy"              = "#7CFC00",  # Light green
  "Slovenia"           = "#FF4500",  # Orange
  "Ireland"            = "#FFD700",  # Yellow
  "Romania"            = "#DC143C",  # Red
  "Russian Federation" = "#1E90FF",  # Blue
  "Spain"              = "#FF69B4"   # Pink
)

# Topic-level difficulty: from lowest mean (hardest) to highest
# Mean AnswerCorrectness per Topic
perf_topic <- math_data %>%
  group_by(Topic) %>%
  summarise(
    n_responses =n(),
    mean_correctness = mean(AnswerCorrectness, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_correctness) 

perf_topic

# Bar plot of topic difficulty (from hardest to easiest)
ggplot(perf_topic,
       aes(x = reorder(Topic, mean_correctness),
           y = mean_correctness)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Topic Difficulty Based on Average AnswerCorrectness",
    x = "Topic (ordered from hardest to easiest)",
    y = "Correct Answers (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9),
    text        = element_text(size = 12)
  )

# Another way to show topic difficulty but now using Lollipop chart.

ggplot(perf_topic,
       aes(x = reorder(Topic, mean_correctness),
           y = mean_correctness)) +
  geom_segment(aes(x = reorder(Topic, mean_correctness),
                   xend = reorder(Topic, mean_correctness),
                   y = 0, yend = mean_correctness),
               color = "grey60") +
  geom_point(size = 3, color = "navy") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Topic Difficulty Ranking (Lollipop Plot)",
    x = "Topic (ordered from hardest to easiest)",
    y = "Correct Answers (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9),
    text        = element_text(size = 12)
  )

# Mean correctness by country and topic
perf_topic_country <- math_data %>%
  group_by(`Student Country`, Topic) %>%
  summarise(
    n_responses      = n(),
    mean_correctness = mean(AnswerCorrectness, na.rm = TRUE),
    .groups = "drop"
  )

perf_topic_country   

# Identify the globally hardest topics (lowest mean correctness)

perf_topic <- math_data %>%
  group_by(Topic) %>%
  summarise(
    n_responses      = n(),
    mean_correctness = mean(AnswerCorrectness, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_correctness)

# Select the first seven hardest topic
hard_topics <- perf_topic %>%
  slice(1:7) %>%        
  pull(Topic)

hard_topics 

# Filter the dataset to include only the hardest topics
perf_tc_hard <- perf_topic_country %>%
  filter(Topic %in% hard_topics)

perf_tc_hard   

# Dot plot by country for the 7 hardest topics
ggplot(perf_tc_hard,
       aes(x = mean_correctness,
           y = reorder(Topic, mean_correctness))) +
  geom_point(aes(color = `Student Country`), size = 3) +
  facet_wrap(~ `Student Country`, ncol = 3) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = country_colors, name = "Country") +
  labs(
    title = "Variation of Topic Difficulty Across Countries",
    subtitle = "Seven Globally Hardest Topics",
    x = "Correct Answers (%)",
    y = "Topic"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(size = 9),
    axis.text.y     = element_text(size = 9),
    strip.text      = element_text(size = 12, face = "bold")
  )

# Enconding Scheme (Dummy variables)
pca_data <- math_data %>%
  select(
    `Student Country`,
    `Question Level`,
    Topic
  )

head(pca_data)

# One-Hot encoding of categorical variables
pca_encoded <- fastDummies::dummy_cols(
  pca_data,
  select_columns = c("Student Country", "Question Level", "Topic"),
  remove_selected_columns = TRUE
)

pca_matrix <- as.matrix(pca_encoded)
pca_matrix_scaled <- scale(pca_matrix)

# PCA on the scaled dummy matrix
pca_model <- prcomp(
  pca_matrix_scaled,
  center = TRUE,
  scale. = TRUE
)
# View general PCA summary
summary(pca_model)

# Scree Plot - variance explained by components
fviz_eig(pca_model,
         addlabels = TRUE,
         barfill = "skyblue",
         barcolor = "black") +
  labs(title = "Scree Plot of PCA Components")

# Variables PCA plot 
fviz_pca_var(
  pca_model,
  col.var = "contrib",
  gradient.cols = c("blue", "orange", "red"),
  top_n = 10,
  repel = TRUE
) +
  labs(title = "PCA – Top 10 Variable Contributions")

# PCA Biplot
fviz_pca_biplot(pca_model,
                geom.ind = "point",
                col.ind  = "gray50",
                alpha.ind = 0.4,
                col.var  = "red",
                repel = TRUE) +
  labs(title = "PCA Biplot – Individuals and Variables")
