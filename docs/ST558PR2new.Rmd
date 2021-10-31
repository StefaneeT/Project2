---
title: "ST 558 Project 2"
author: "Stefanee Tillman & Kaitlyn Bayley"
date: "10/19/2021"
output: github_document
params:
        channel: "Lifestyle"
---

```{r, eval=FALSE}
rmarkdown::render("C:/Users/Stefa/OneDrive/Documents/GitHub/Project2/ST558PR2new.Rmd",
 output_format = "github_document",
 output_file = "README.md",
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
library(e1071)
require(knitr)

```
# Introduction Section


For this project, we are reviewing a dataset related to social media shares and popularity of articles from Mashable. We initially subsetted the data to analyze the lifestyle channel and find the best model to predict the popularity of an article based on the most significant variables. We can then automate this process to do the same for the other identified channels (entertainment, business, social media, technology, and world).


# Data

```{r}
#Reading in Data
NewsData<- read_excel("C:/Users/Stefa/Downloads/OnlineNewsPopularity.xlsx")

#Subsetting Data
NewsData <- NewsData %>% filter(data_channel_is_lifestyle == 1) %>%
        select(-url, -timedelta, -data_channel_is_entertainment, -data_channel_is_bus, -data_channel_is_socmed, -data_channel_is_tech, -data_channel_is_world, -data_channel_is_lifestyle) 
```

# Summarizations

```{r}
plotImages <- ggplot(data = NewsData, aes(x=num_imgs, y=shares))
plotImages + geom_jitter() +
  labs(x= "Number of Images", y = "Shares")
```

The data points on this plot, visualizing how many shares result based on the number of images, are clustered around the x-axis. It appears that more images does not necessarily translate to more shares.

```{r}
plotSubject <- ggplot(data = NewsData, aes(x=global_subjectivity, y=shares))
plotSubject + geom_jitter() +
  labs(x= "Global Subjectivity", y= "Shares")
```

This plot compares global subjectivity and the number of shares. The subjectivity is clustered around 0.5, which indicates that articles closer to neutral generate more shares.

```{r}
plotTitle <- ggplot(data = NewsData, aes(x=n_tokens_title, y=shares))
plotTitle + geom_jitter() +
  labs(x = "Number of Words in Title", y = "Shares")
```
This plot looks at the number of words in the title and how that affects the number of shares. It appears that titles that range from around 7-12 words or so in length generate more shares than title lengths outside of that range.


```{r}
means <- c(mean(NewsData$num_imgs), mean(NewsData$global_subjectivity), mean(NewsData$n_tokens_title))
SDs <- c(sd(NewsData$num_imgs), sd(NewsData$global_subjectivity), sd(NewsData$n_tokens_title))
```

```{r}
summaryImages <- summarize(NewsData, mean(num_imgs), sd(num_imgs))
summaryImages
```

```{r}
summarySubject <- summarize(NewsData, mean(global_subjectivity), sd(global_subjectivity))
summarySubject
```

```{r}
summaryTitle <- summarize(NewsData, mean(n_tokens_title), sd(n_tokens_title))
summaryTitle
```


```{r}
summaryStats <- tbl_df(cbind(summaryImages, summarySubject, summaryTitle))
summaryStats
```

These summary statistics of means and standard deviations help describe the variables plotted above. The mean number of images is around 5, the mean global subjectivity is just under 0.5, and the mean title length is around 10 words.

```{r}
tableVideosDay <- table(NewsData$num_videos, NewsData$weekday_is_wednesday)
tableVideosDay
```

This categorical table shows the number of videos in an article and whether it was shared on a Wednesday.

# Modeling
```{r}
#Split data into training and test set 70:30
set.seed(1)
dt = sort(sample(nrow(NewsData), nrow(NewsData) * .7))
train<- NewsData[dt,]
test<- NewsData[-dt,]
```

# Explanation of of the idea of a linear regression model-Stefanee Tillman

A linear regression model is a model that displays the relationship between two or more variables. Here we are demonstrating the relationship between Lifestyle and Shares. With the predictors used in the formula and shares being the response we are attempting to see the relationship between the two. Does Lifestyle have a significant effect on the amount of shares? Is there a correlation between the variables?

```{r}
#Fit Linear Regression Model 1- All Predictors
model1 <- lm(shares~., data=train)
summary(model1)
```
Here the Adjusted R-squared value is .02891 which is fairly low. The p-value is .0002 which is significant. The significant variables here include: n_tokens_content, n_non_stop_unique_tokens, num_videos, and  kw_avg_avg

```{r}
#Fit Linear Regression Model 2- Significant Variables
model2 <- lm(shares~ n_tokens_content + n_non_stop_unique_tokens + num_videos + kw_avg_avg, data=train)

summary(model2)
```

# Explanation of of the idea of a Random Forest Ensemble-Stefanee Tillman
A random forest is a machine learning technique that’s used to solve regression and classification problems. The Random Forest Technique extends the idea of bagging and is generally better than bagging.The Random Forest will create multiple trees from bootstrap samples and average the results. For this project, we are using the Random Forest model after subsetting the data_channel_is_lifestyle variable. After using the Cross Validation Method we are able to observe the RMSE and R^2 values for the predictor variables with shares as the response.

```{r, cache=TRUE}
#Random Forest Model using Cross-Validation

rfmodel <- train(shares ~ n_tokens_title + num_hrefs + num_imgs + average_token_length + kw_min_min, data = train, method = "rf", trControl = trainControl(method= "cv", number = 5), preProcess = c("center", "scale"), tuneGrid = data.frame(mtry = 1:18))

print(rfmodel)
```


```{r}
#Boosted Tree
gbmGrid <-  expand.grid(interaction.depth = 1:4,
                        n.trees = c(25, 50, 100, 150, 200),
                        shrinkage = 0.1,
                        n.minobsinnode = 10)
nrow(gbmGrid)
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
boostFit <- train(shares~ n_tokens_title + num_hrefs + num_imgs + average_token_length + kw_min_min,
                data = train,
                preProcess = c("center", "scale"),
                trControl = fitControl,
                method = "gbm",
                tuneGrid = gbmGrid)

```

# Comparison
```{r}
#Model 1
set.seed(1)
# Train the model
fit1 <- train(shares ~., data = train, method = "lm")
print(fit1)

#See how well it performs on the test set
#Predict fit 1-Main Effect
pred.fit1 <- predict(fit1, newdata = test)
#calculating RSME
error <- test$shares-pred.fit1
RSME<- sqrt(mean(error^2))
RSME

#calculating MAE
MAE<- mean((test$shares-pred.fit1)^2)
MAE
```

```{r}
#Comparison for Model 2
set.seed(1)
# Train the model
fit2 <- train(shares ~n_tokens_content + n_non_stop_unique_tokens + num_videos + kw_avg_avg, data = train, method = "lm")
print(fit2)

#See how well it performs on the test set
#Predict fit 1-Main Effect
pred.fit2 <- predict(fit2, newdata = test)
#calculating RSME
error <- test$shares-pred.fit2
RSME<- sqrt(mean(error^2))
RSME

#calculating MAE
MAE<- mean((test$shares-pred.fit2)^2)
MAE
```

```{r}
#Comparison for Random Forest Model
#See how well it performs on the test set
#Predict fit 1-Main Effect
pred.rf<- predict(rfmodel, newdata = test)
#calculating RSME
error <- test$shares-pred.rf
RSME<- sqrt(mean(error^2))
RSME

#calculating MAE
MAE<- mean((test$shares-pred.rf)^2)
MAE

```


```{r}
#Comparison for Boosted Model
boostPred <- predict(boostFit, newdata = test)
boostRMSE <- sqrt(mean((boostPred-test$shares)^2))
boostRMSE
```


It appears that the boosted model is the declared winner. This is because on the test set the Boosted Model has the lowest RSME of 5627.462.

# Automation

```{r, bindings = FALSE}
#Creating New Variable
NewsData<- read_excel("C:/Users/Stefa/Downloads/OnlineNewsPopularity.xlsx")

NewsData <- NewsData %>% 
            mutate(Topic = case_when(
               data_channel_is_lifestyle == 1 ~ "Lifestyle",
               data_channel_is_entertainment == 1 ~ "Entertainment",
               data_channel_is_bus == 1 ~ "Business",
               data_channel_is_socmed == 1 ~ "SocialMedia",
               data_channel_is_tech == 1 ~ "Tech",
               data_channel_is_world == 1 ~ "World"
               ))
NewsData$Topic<- replace_na(NewsData$Topic, "Other")
NewsData$Topic <- as.factor(NewsData$Topic)

#Beginning Automation
Channel<- unique(NewsData$Topic)
output_file<- paste0(Channel, ".html")
unlockBinding("params", environment())
params<- lapply(Channel, FUN = function(Entertainment){list(channel = Entertainment)})
reports<- tibble(output_file, params)
reports
```



