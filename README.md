---
title: "ST 558 Project 2"
author: "Stefanee Tillman & Kaitlyn Bayley"
date: "10/19/2021"
output: html_document
---

```{r}
rmarkdown::render("C:/Users/Stefa/OneDrive/Documents/GitHub/Project2/Project2.Rmd",
 output_format = "github_document",
 output_file = "docs/",
 output_options = list(
 toc = FALSE,
 toc_depth = 3,
 number_sections = FALSE,
 df_print = "default",
keep_html = FALSE
))
```

# Required Packages 

```{r, message= FALSE}
require(readxl)
require(dplyr)
#install.packages("randomForest")
library(randomForest)
require(tidyr)
require(caret)
require(gbm)
require(randomForest)
```
# Introduction Section

# Data

```{r}
#Reading in Data
NewsData<- read_excel("C:/Users/Stefa/Downloads/OnlineNewsPopularity.xlsx")

#Subsetting Data
NewsData <- NewsData %>% mutate(channel = case_when(data_channel_is_bus == 1 ~ "Business",data_channel_is_entertainment == 1 ~ "Entertainment",data_channel_is_lifestyle == 1 ~ "Lifestyle",data_channel_is_socmed == 1 ~ "SocialMedia",data_channel_is_tech == 1 ~ "Tech", data_channel_is_world == 1 ~ "World"))

#Some of the output is NA, replace with other value to produce better results
NewsData$channel <- replace_na(NewsData$channel, "Other")
NewsData$channel <- as.factor(NewsData$channel) 
print(NewsData)

NewsData <- NewsData %>% select(-url, -timedelta)

```

# Summarizations
```{r}

plotImages <- ggplot(data = NewsData, aes(x=num_imgs, y=shares))
plotImages + geom_jitter() +
  labs(x= "Number of Images", y = "Shares")


plotSubject <- ggplot(data = NewsData, aes(x=global_subjectivity, y=shares))
plotSubject + geom_jitter() +
  labs(x= "Global Subjectivity", y= "Shares")

plotNumberWords <- ggplot(data = NewsData, aes(x=n_tokens_title, y=shares))
plotNumberWords + geom_jitter() +
  labs(x = "Number of Words in Title", y = "Shares")

means <- c(mean(NewsData$num_imgs), mean(NewsData$global_subjectivity), mean(NewsData$n_tokens_title))
SDs <- c(sd(NewsData$num_imgs), sd(NewsData$global_subjectivity), sd(NewsData$n_tokens_title))

PlotSummaryStats <- tbl_df(cbind(means, SDs))

tableChannelDay <- table(NewsData$data_channel_is_entertainment, NewsData$weekday_is_wednesday)
tableChannelDay

```

# Modeling
```{r}
#Split data into training and test set 70:30
set.seed(1)
dt = sort(sample(nrow(NewsData), nrow(NewsData) * .7))
train<- NewsData[dt,]
test<- NewsData[-dt,]
```

```{r}
#Fit Linear Regression model1- Main Effect
model1 <- lm(shares~., data=train)
summary(model1)
```

```{r}
#Fit Linear Regression model1- Main Effect
model2 <- lm(shares~ n_tokens_title + num_hrefs + num_imgs + average_token_length + data_channel_is_entertainment + kw_min_min + kw_max_min + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares + weekday_is_wednesday + global_subjectivity + title_sentiment_polarity, data=train)

summary(model2)
```

```{r}
#Fit Linear Regression model3 - Main Effect

model3 <- lm(shares~ n_tokens_content + num_self_hrefs + num_videos + data_channel_is_socmed + weekday_is_friday + title_subjectivity, data=train)

summary(model3)

```

```{r}
#Random Forest Model using Cross-Validation
rfmodel <- train(shares~ n_tokens_title + num_hrefs + num_imgs + average_token_length + data_channel_is_entertainment + kw_min_min + kw_max_min + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares + weekday_is_wednesday + global_subjectivity + title_sentiment_polarity, 
data = train, method = "rf", trControl = trainControl(method= "repeatedcv", number = 5, repeats = 3), preProcess = c("center", "scale"))



rfmodel

confusionMatrix(rfmodel, newdata= test)
```

```{r}
#Boosted Tree

gbmGrid <-  expand.grid(interaction.depth = 1:4, 
                        n.trees = c(25, 50, 100, 150, 200), 
                        shrinkage = 0.1,
                        n.minobsinnode = 10)

nrow(gbmGrid)

fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

boostFit <- train(shares~ n_tokens_title + num_hrefs + num_imgs + average_token_length + data_channel_is_entertainment + kw_min_min + kw_max_min + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares + weekday_is_wednesday + global_subjectivity + title_sentiment_polarity, 
                data = train,
                preProcess = c("center", "scale"),
                trControl = fitControl,
                method = "gbm",
                tuneGrid = gbmGrid)

boostPred <- predict(boostFit, newdata = test)

boostRMSE <- sqrt(mean((boostPred-test$shares)^2))
boostRMSE

confusionMatrix(boostPred, test$shares)

```

# Explanation of of the idea of a linear regression model-Stefanee Tillman
A linear regression model is a models that displays the relationship between two or more variables. Here we are demonstrating the relationship between Social Media and Shares. With Social Media being the predictor and shares being the response we are attempting to see the relationship between the two. Does social media have a significant effect on the amount of shares? Is there a correlation between the two variables.



