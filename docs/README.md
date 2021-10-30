```{r}
rmarkdown::render("C:/Users/Stefa/OneDrive/Documents/GitHub/Project2/ST558PR2.Rmd",
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
# Introduction

For this project, we are reviewing a dataset related to social media shares and popularity of articles from Mashable. We initially subsetted the data to analyze the lifestyle channel and find the best model to predict the popularity of an article based on the most significant variables. We can then automate this process to do the same for the other identified channels (entertainment, business, social media, technology, and world).

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

#The data points on this plot, visualizing how many shares result based on the number of images, are clustered around the x-axis. It appears that more images does not necessarily translate to more shares.

plotSubject <- ggplot(data = NewsData, aes(x=global_subjectivity, y=shares))
plotSubject + geom_jitter() +
  labs(x= "Global Subjectivity", y= "Shares")

#This plot compares global subjectivity and the number of shares. The subjectivity is clustered around 0.5, which indicates that articles closer to neutral generate more shares.

plotTitle <- ggplot(data = NewsData, aes(x=n_tokens_title, y=shares))
plotTitle + geom_jitter() +
  labs(x = "Number of Words in Title", y = "Shares")

#This plot looks at the number of words in the title and how that affects the number of shares. It appears that titles that range from around 7-12 words or so in length generate more shares than title lengths outside of that range.

summaryImages <- summarize(NewsData, mean(num_imgs), sd(num_imgs))

summarySubject <- summarize(NewsData, mean(global_subjectivity), sd(global_subjectivity))

summaryTitle <- summarize(NewsData, mean(n_tokens_title), sd(n_tokens_title))

summaryStats <- tbl_df(cbind(summaryImages, summarySubject, summaryTitle))

#These summary statistics of means and standard deviations help describe the variables plotted above. The mean number of images is around 5, the mean global subjectivity is just under 0.5, and the mean title length is around 10 words. 

summaryStats

#This categorical table shows the number of videos in an article and whether it was shared on a Wednesday. 

tableVideosDay <- table(NewsData$num_videos, NewsData$weekday_is_wednesday)

#This categorical table shows the number of videos in an article and whether it was shared on a Wednesday.

tableVideosDay

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
#Random Forest Model using Cross-Validation
rfmodel <- train(shares~ n_tokens_title + num_hrefs + num_imgs + average_token_length + data_channel_is_entertainment + kw_min_min + kw_max_min + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares + weekday_is_wednesday + global_subjectivity + title_sentiment_polarity, 
data = train, method = "rf", trControl = trainControl(method= "repeatedcv", number = 5, repeats = 3), preProcess = c("center", "scale"))
rfmodel
confusionMatrix(rfmodel, newdata= test)
```

# Explanation of of the idea of a linear regression model-Stefanee Tillman
A linear regression model is a models that displays the relationship between two or more variables. Here we are demonstrating the relationship between Social Media and Shares. With Social Media being the predictor and shares being the response we are attempting to see the relationship between the two. Does social media have a significant effect on the amount of shares? Is there a correlation between the two variables.


