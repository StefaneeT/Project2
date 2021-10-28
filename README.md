<<<<<<< HEAD
ST 558 Project 2
================
Stefanee Tillman & Kaitlyn Bayley
10/19/2021

``` r
rmarkdown::render("C:/Users/Stefa/OneDrive/Documents/GitHub/Project2/ST558PR2new.Rmd",
 output_format = "github_document",
 output_file = "README.md",
=======
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
>>>>>>> ea10444536847c2fa6be877c1291cebe06ffc439
 output_options = list(
 toc = FALSE,
 toc_depth = 3,
 number_sections = FALSE,
 df_print = "default",
keep_html = FALSE
))
```

<<<<<<< HEAD
# Required Packages

``` r
=======
# Required Packages 

```{r, message= FALSE}
>>>>>>> ea10444536847c2fa6be877c1291cebe06ffc439
require(readxl)
require(dplyr)
#install.packages("randomForest")
library(randomForest)
require(tidyr)
require(caret)
require(gbm)
require(randomForest)
<<<<<<< HEAD
library(e1071)
require(knitr)
```

=======
```
>>>>>>> ea10444536847c2fa6be877c1291cebe06ffc439
# Introduction Section

# Data

<<<<<<< HEAD
``` r
=======
```{r}
>>>>>>> ea10444536847c2fa6be877c1291cebe06ffc439
#Reading in Data
NewsData<- read_excel("C:/Users/Stefa/Downloads/OnlineNewsPopularity.xlsx")

#Subsetting Data
<<<<<<< HEAD
NewsData <- NewsData %>% filter(data_channel_is_lifestyle == 1) %>%
        select(-url, -timedelta, -data_channel_is_entertainment, -data_channel_is_bus, -data_channel_is_socmed, -data_channel_is_tech, -data_channel_is_world, -data_channel_is_lifestyle) 
```

# Summarizations

# Modeling

``` r
=======
NewsData <- NewsData %>% mutate(channel = case_when(data_channel_is_bus == 1 ~ "Business",data_channel_is_entertainment == 1 ~ "Entertainment",data_channel_is_lifestyle == 1 ~ "Lifestyle",data_channel_is_socmed == 1 ~ "SocialMedia",data_channel_is_tech == 1 ~ "Tech", data_channel_is_world == 1 ~ "World"))

#Some of the output is NA, replace with other value to produce better results
NewsData$channel <- replace_na(NewsData$channel, "Other")
NewsData$channel <- as.factor(NewsData$channel) 
print(NewsData)

NewsData <- NewsData %>% select(-url, -timedelta)

```

# Summarizations
```{r}

```

# Modeling
```{r}
>>>>>>> ea10444536847c2fa6be877c1291cebe06ffc439
#Split data into training and test set 70:30
set.seed(1)
dt = sort(sample(nrow(NewsData), nrow(NewsData) * .7))
train<- NewsData[dt,]
test<- NewsData[-dt,]
```

<<<<<<< HEAD
# Explanation of of the idea of a linear regression model-Stefanee Tillman

A linear regression model is a model that displays the relationship
between two or more variables. Here we are demonstrating the
relationship between Lifestyle and Shares. With the predictors used in
the formula and shares being the response we are attempting to see the
relationship between the two. Does Lifestyle have a significant effect
on the amount of shares? Is there a correlation between the variables?

``` r
#Fit Linear Regression Model 1- All Predictors
=======
```{r}
#Fit Linear Regression model1- Main Effect
>>>>>>> ea10444536847c2fa6be877c1291cebe06ffc439
model1 <- lm(shares~., data=train)
summary(model1)
```

<<<<<<< HEAD
    ## 
    ## Call:
    ## lm(formula = shares ~ ., data = train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -25010  -3042  -1239    684 196708 
    ## 
    ## Coefficients: (4 not defined because of singularities)
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   5.145e+02  4.080e+03   0.126 0.899659    
    ## n_tokens_title                1.101e+02  1.452e+02   0.758 0.448414    
    ## n_tokens_content              2.347e+00  8.254e-01   2.844 0.004523 ** 
    ## n_unique_tokens              -8.432e+03  9.218e+03  -0.915 0.360467    
    ## n_non_stop_words             -1.214e+04  8.637e+03  -1.406 0.160022    
    ## n_non_stop_unique_tokens      1.714e+04  7.886e+03   2.174 0.029888 *  
    ## num_hrefs                     4.020e+01  2.864e+01   1.404 0.160599    
    ## num_self_hrefs               -1.184e+02  9.930e+01  -1.192 0.233506    
    ## num_imgs                     -3.036e+01  4.931e+01  -0.616 0.538206    
    ## num_videos                    4.452e+02  1.233e+02   3.612 0.000315 ***
    ## average_token_length          4.716e+02  1.148e+03   0.411 0.681315    
    ## num_keywords                  4.837e+01  1.874e+02   0.258 0.796348    
    ## kw_min_min                   -1.659e+00  6.020e+00  -0.276 0.782846    
    ## kw_max_min                   -8.807e-02  2.423e-01  -0.364 0.716268    
    ## kw_avg_min                   -1.253e-01  1.621e+00  -0.077 0.938384    
    ## kw_min_max                    1.509e-03  1.996e-02   0.076 0.939730    
    ## kw_max_max                    6.505e-05  2.304e-03   0.028 0.977479    
    ## kw_avg_max                   -1.004e-02  5.966e-03  -1.682 0.092763 .  
    ## kw_min_avg                   -5.707e-01  3.308e-01  -1.725 0.084750 .  
    ## kw_max_avg                   -1.609e-01  9.171e-02  -1.754 0.079585 .  
    ## kw_avg_avg                    1.862e+00  5.859e-01   3.178 0.001513 ** 
    ## self_reference_min_shares     1.278e-01  7.059e-02   1.811 0.070352 .  
    ## self_reference_max_shares     2.384e-02  4.226e-02   0.564 0.572799    
    ## self_reference_avg_sharess   -8.795e-02  1.021e-01  -0.862 0.389064    
    ## weekday_is_monday             1.440e+03  1.073e+03   1.342 0.179820    
    ## weekday_is_tuesday            6.555e+02  1.065e+03   0.615 0.538360    
    ## weekday_is_wednesday         -5.067e+02  1.023e+03  -0.495 0.620349    
    ## weekday_is_thursday          -2.739e+02  1.057e+03  -0.259 0.795533    
    ## weekday_is_friday            -4.056e+02  1.076e+03  -0.377 0.706221    
    ## weekday_is_saturday           3.471e+02  1.205e+03   0.288 0.773361    
    ## weekday_is_sunday                    NA         NA      NA       NA    
    ## is_weekend                           NA         NA      NA       NA    
    ## LDA_00                       -4.303e+01  1.109e+03  -0.039 0.969056    
    ## LDA_01                       -1.556e+03  2.804e+03  -0.555 0.579046    
    ## LDA_02                       -1.883e+03  2.581e+03  -0.729 0.465826    
    ## LDA_03                        2.320e+03  1.835e+03   1.264 0.206285    
    ## LDA_04                               NA         NA      NA       NA    
    ## global_subjectivity           1.040e+03  4.299e+03   0.242 0.808911    
    ## global_sentiment_polarity    -6.590e+03  7.968e+03  -0.827 0.408343    
    ## global_rate_positive_words    3.268e+04  3.562e+04   0.917 0.359044    
    ## global_rate_negative_words   -1.391e+04  8.640e+04  -0.161 0.872077    
    ## rate_positive_words          -2.219e+03  6.942e+03  -0.320 0.749298    
    ## rate_negative_words                  NA         NA      NA       NA    
    ## avg_positive_polarity         6.086e+03  6.730e+03   0.904 0.365982    
    ## min_positive_polarity        -4.168e+03  5.295e+03  -0.787 0.431265    
    ## max_positive_polarity        -2.452e+03  2.066e+03  -1.187 0.235589    
    ## avg_negative_polarity        -3.877e+02  5.669e+03  -0.068 0.945490    
    ## min_negative_polarity         9.122e+02  1.962e+03   0.465 0.641953    
    ## max_negative_polarity        -2.260e+03  4.897e+03  -0.461 0.644580    
    ## title_subjectivity            3.387e+02  1.197e+03   0.283 0.777332    
    ## title_sentiment_polarity      6.295e+02  1.237e+03   0.509 0.610875    
    ## abs_title_subjectivity        1.913e+03  1.663e+03   1.150 0.250136    
    ## abs_title_sentiment_polarity -1.067e+03  1.745e+03  -0.611 0.541013    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9814 on 1420 degrees of freedom
    ## Multiple R-squared:  0.06066,    Adjusted R-squared:  0.02891 
    ## F-statistic:  1.91 on 48 and 1420 DF,  p-value: 0.0002136

``` r
#Fit Linear Regression Model 2- Significant Variables
model2 <- lm(shares~ n_tokens_title + num_hrefs + num_imgs + average_token_length + kw_min_min + kw_max_min + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares + weekday_is_wednesday + LDA_01 + global_subjectivity + title_sentiment_polarity, data=train)
=======
```{r}
#Fit Linear Regression model1- Main Effect
model2 <- lm(shares~ n_tokens_title + num_hrefs + num_imgs + average_token_length + data_channel_is_entertainment + kw_min_min + kw_max_min + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares + weekday_is_wednesday + global_subjectivity + title_sentiment_polarity, data=train)
>>>>>>> ea10444536847c2fa6be877c1291cebe06ffc439

summary(model2)
```

<<<<<<< HEAD
    ## 
    ## Call:
    ## lm(formula = shares ~ n_tokens_title + num_hrefs + num_imgs + 
    ##     average_token_length + kw_min_min + kw_max_min + kw_min_avg + 
    ##     kw_max_avg + kw_avg_avg + self_reference_min_shares + weekday_is_wednesday + 
    ##     LDA_01 + global_subjectivity + title_sentiment_polarity, 
    ##     data = train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -9601  -2687  -1701    -45 199816 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)                1.842e+03  2.931e+03   0.628  0.52985   
    ## n_tokens_title            -1.694e+01  1.391e+02  -0.122  0.90305   
    ## num_hrefs                  2.425e+01  2.532e+01   0.958  0.33831   
    ## num_imgs                   6.892e+00  3.329e+01   0.207  0.83604   
    ## average_token_length      -5.151e+02  5.249e+02  -0.981  0.32667   
    ## kw_min_min                 2.501e+00  3.423e+00   0.731  0.46509   
    ## kw_max_min                -5.888e-02  7.027e-02  -0.838  0.40225   
    ## kw_min_avg                -6.288e-01  2.923e-01  -2.151  0.03164 * 
    ## kw_max_avg                -1.541e-01  8.694e-02  -1.772  0.07655 . 
    ## kw_avg_avg                 1.683e+00  5.112e-01   3.292  0.00102 **
    ## self_reference_min_shares  5.853e-02  2.427e-02   2.412  0.01600 * 
    ## weekday_is_wednesday      -7.598e+02  6.590e+02  -1.153  0.24912   
    ## LDA_01                    -1.772e+03  2.735e+03  -0.648  0.51712   
    ## global_subjectivity        1.852e+02  3.128e+03   0.059  0.95280   
    ## title_sentiment_polarity  -3.039e+02  9.383e+02  -0.324  0.74605   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9887 on 1454 degrees of freedom
    ## Multiple R-squared:  0.02379,    Adjusted R-squared:  0.01439 
    ## F-statistic: 2.531 on 14 and 1454 DF,  p-value: 0.001395

# Explanation of of the idea of a Random Forest Ensemble-Stefanee Tillman

A random forest is a machine learning technique that’s used to solve
regression and classification problems. The Random Forest Technique
extends the idea of bagging and is generally better than bagging.The
Random Forest will create multiple trees from bootstrap samples and
average the results. For this project, we are using the Random Forest
model after subsetting the data\_channel\_is\_lifestyle variable. After
using the Cross Validation Method we are able to observe the RMSE and
R^2 values for the predictor variables with shares as the response.

``` r
#Random Forest Model using Cross-Validation

rfmodel <- train(shares ~ n_tokens_title + num_hrefs + num_imgs + average_token_length + kw_min_min, data = train, method = "rf", trControl = trainControl(method= "cv", number = 5), preProcess = c("center", "scale"), tuneGrid = data.frame(mtry = 1:3))

print(rfmodel)
```

    ## Random Forest 
    ## 
    ## 1469 samples
    ##    5 predictor
    ## 
    ## Pre-processing: centered (5), scaled (5) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 1174, 1175, 1176, 1176, 1175 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared     MAE     
    ##   1     8904.761  0.003120248  3553.277
    ##   2     9145.536  0.003801411  3735.405
    ##   3     9227.125  0.005574572  3795.873
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 1.
=======
```{r}
#Random Forest Model using Cross-Validation
rfmodel <- train(shares~ n_tokens_title + num_hrefs + num_imgs + average_token_length + data_channel_is_entertainment + kw_min_min + kw_max_min + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares + weekday_is_wednesday + global_subjectivity + title_sentiment_polarity, 
data = train, method = "rf", trControl = trainControl(method= "repeatedcv", number = 5, repeats = 3), preProcess = c("center", "scale"))



rfmodel

confusionMatrix(rfmodel, newdata= test)
```

# Explanation of of the idea of a linear regression model-Stefanee Tillman
A linear regression model is a models that displays the relationship between two or more variables. Here we are demonstrating the relationship between Social Media and Shares. With Social Media being the predictor and shares being the response we are attempting to see the relationship between the two. Does social media have a significant effect on the amount of shares? Is there a correlation between the two variables.



>>>>>>> ea10444536847c2fa6be877c1291cebe06ffc439
