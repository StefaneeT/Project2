ST 558 Project 2
================
Stefanee Tillman & Kaitlyn Bayley
10/19/2021

``` r
rmarkdown::render("Project2.Rmd",
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

``` r
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

For this project, we are reviewing a dataset related to social media
shares and popularity of articles from Mashable. We initially subsetted
the data to analyze the lifestyle channel and find the best model to
predict the popularity of an article based on the most significant
variables. We can then automate this process to do the same for the
other identified channels (entertainment, business, social media,
technology, and world). \# Data

``` r
#Reading in Data
NewsData<- read_excel("OnlineNewsPopularity.xlsx")
#Subsetting Data
NewsData <- NewsData %>% filter(data_channel_is_lifestyle == 1) %>%
        select(-url, -timedelta, -data_channel_is_entertainment, -data_channel_is_bus, -data_channel_is_socmed, -data_channel_is_tech, -data_channel_is_world, -data_channel_is_lifestyle)
```

# Summarizations

``` r
NewsDataPairs <- cbind(NewsData$n_tokens_title, NewsData$num_imgs, NewsData$global_subjectivity)

pairs(NewsDataPairs)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
plotImages <- ggplot(data = NewsData, aes(x=num_imgs, y=shares))
plotImages + geom_jitter() +
  labs(x= "Number of Images", y = "Shares")
```

![plot](https://raw.githubusercontent.com/StefaneeT/Project2/main/image/PlotImages.png)

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> The data
points on this plot, visualizing how many shares result based on the
number of images, are clustered around the x-axis. It appears that more
images does not necessarily translate to more shares.

``` r
plotSubject <- ggplot(data = NewsData, aes(x=global_subjectivity, y=shares))
plotSubject + geom_jitter() +
  labs(x= "Global Subjectivity", y= "Shares")
```
![plot](https://raw.githubusercontent.com/StefaneeT/Project2/main/image/plotSubject.png)

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> This plot
compares global subjectivity and the number of shares. The subjectivity
is clustered around 0.5, which indicates that articles closer to neutral
generate more shares.

``` r
plotTitle <- ggplot(data = NewsData, aes(x=n_tokens_title, y=shares))
plotTitle + geom_jitter() +
  labs(x = "Number of Words in Title", y = "Shares")
```
![plot](https://raw.githubusercontent.com/StefaneeT/Project2/main/image/plotTitle.png)

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> This plot
looks at the number of words in the title and how that affects the
number of shares. It appears that titles that range from around 7-12
words or so in length generate more shares than title lengths outside of
that range.

``` r
means <- c(mean(NewsData$num_imgs), mean(NewsData$global_subjectivity), mean(NewsData$n_tokens_title))
SDs <- c(sd(NewsData$num_imgs), sd(NewsData$global_subjectivity), sd(NewsData$n_tokens_title))
```

``` r
summaryImages <- summarize(NewsData, mean(num_imgs), sd(num_imgs))
summaryImages
```

    ## # A tibble: 1 x 2
    ##   `mean(num_imgs)` `sd(num_imgs)`
    ##              <dbl>          <dbl>
    ## 1             4.90           8.15

``` r
summarySubject <- summarize(NewsData, mean(global_subjectivity), sd(global_subjectivity))
summarySubject
```

    ## # A tibble: 1 x 2
    ##   `mean(global_subjectivity)` `sd(global_subjectivity)`
    ##                         <dbl>                     <dbl>
    ## 1                       0.473                    0.0941

``` r
summaryTitle <- summarize(NewsData, mean(n_tokens_title), sd(n_tokens_title))
summaryTitle
```

    ## # A tibble: 1 x 2
    ##   `mean(n_tokens_title)` `sd(n_tokens_title)`
    ##                    <dbl>                <dbl>
    ## 1                   9.77                 1.91

``` r
summaryStats <- tbl_df(cbind(summaryImages, summarySubject, summaryTitle))
summaryStats
```

    ## # A tibble: 1 x 6
    ##   `mean(num_imgs)` `sd(num_imgs)` `mean(global_subjectivity)` `sd(global_subjecti~
    ##              <dbl>          <dbl>                       <dbl>                <dbl>
    ## 1             4.90           8.15                       0.473               0.0941
    ## # ... with 2 more variables: mean(n_tokens_title) <dbl>, sd(n_tokens_title) <dbl>

These summary statistics of means and standard deviations help describe
the variables plotted above. The mean number of images is around 5, the
mean global subjectivity is just under 0.5, and the mean title length is
around 10 words.

``` r
tableVideosDay <- table(NewsData$num_videos, NewsData$weekday_is_wednesday)
tableVideosDay
```

    ##     
    ##         0    1
    ##   0  1330  301
    ##   1   271   62
    ##   2    50   15
    ##   3    17    4
    ##   4     6    1
    ##   5     6    2
    ##   6     4    0
    ##   7     4    0
    ##   8     3    0
    ##   9     2    0
    ##   10    8    1
    ##   11    4    1
    ##   12    2    0
    ##   15    1    0
    ##   21    1    0
    ##   26    0    1
    ##   28    1    0
    ##   50    1    0

This categorical table shows the number of videos in an article and
whether it was shared on a Wednesday. \# Modeling

``` r
#Split data into training and test set 70:30
set.seed(1)
dt = sort(sample(nrow(NewsData), nrow(NewsData) * .7))
train<- NewsData[dt,]
test<- NewsData[-dt,]
```

# Explanation of of the idea of a linear regression model-Stefanee Tillman

A linear regression model is a model that displays the relationship
between two or more variables. Here we are demonstrating the
relationship between Lifestyle and Shares. With the predictors used in
the formula and shares being the response we are attempting to see the
relationship between the two. Does Lifestyle have a significant effect
on the amount of shares? Is there a correlation between the variables?

``` r
#Fit Linear Regression Model 1- All Predictors
model1 <- lm(shares~., data=train)
summary(model1)
```

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

Here the Adjusted R-squared value is .02891 which is fairly low. The
p-value is .0002 which is significant. The significant variables here
include: n\_tokens\_content, n\_non\_stop\_unique\_tokens, num\_videos,
and kw\_avg\_avg

``` r
#Fit Linear Regression Model 2- Significant Variables
model2 <- lm(shares~ n_tokens_content + n_non_stop_unique_tokens + num_videos + kw_avg_avg, data=train)
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = shares ~ n_tokens_content + n_non_stop_unique_tokens + 
    ##     num_videos + kw_avg_avg, data = train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -25461  -2581  -1657   -229 202309 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              -1843.2849  1946.8898  -0.947  0.34390    
    ## n_tokens_content             1.3512     0.4761   2.838  0.00460 ** 
    ## n_non_stop_unique_tokens  3087.8200  2372.5884   1.301  0.19331    
    ## num_videos                 405.6970   118.1184   3.435  0.00061 ***
    ## kw_avg_avg                   0.7292     0.1852   3.938  8.6e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 9843 on 1464 degrees of freedom
    ## Multiple R-squared:  0.0259, Adjusted R-squared:  0.02323 
    ## F-statistic:  9.73 on 4 and 1464 DF,  p-value: 9.109e-08

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

``` r
#Boosted Tree
gbmGrid <-  expand.grid(interaction.depth = 1:4,
                        n.trees = c(25, 50, 100, 150, 200),
                        shrinkage = 0.1,
                        n.minobsinnode = 10)
nrow(gbmGrid)
```

    ## [1] 20

``` r
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
boostFit <- train(shares~ n_tokens_title + num_hrefs + num_imgs + average_token_length + kw_min_min,
                data = train,
                preProcess = c("center", "scale"),
                trControl = fitControl,
                method = "gbm",
                tuneGrid = gbmGrid)
```

    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 70696526.8002             nan     0.1000 188174.4019
    ##      2 70510039.8244             nan     0.1000 14422.0154
    ##      3 70421783.8115             nan     0.1000 72195.9574
    ##      4 70253494.3559             nan     0.1000 51486.8924
    ##      5 70106935.9657             nan     0.1000 -11322.9633
    ##      6 69997750.2005             nan     0.1000 42456.4210
    ##      7 69950606.1365             nan     0.1000 51428.5438
    ##      8 69856519.2868             nan     0.1000 -42540.9574
    ##      9 69765619.8118             nan     0.1000 -67753.1659
    ##     10 69701497.5638             nan     0.1000 -71902.9537
    ##     20 69363954.7936             nan     0.1000 5566.7324
    ##     40 68888054.2369             nan     0.1000 -87719.3383
    ##     60 68673564.6563             nan     0.1000 -159030.8762
    ##     80 68292580.8604             nan     0.1000 -151735.8914
    ##    100 67895193.8561             nan     0.1000 -68564.8112
    ##    120 67635713.0692             nan     0.1000 -203198.0013
    ##    140 67398034.0402             nan     0.1000 4034.1532
    ##    160 67126782.0392             nan     0.1000 -68986.6009
    ##    180 66873393.9342             nan     0.1000 2253.4215
    ##    200 66698206.5791             nan     0.1000 -142407.6359
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 70661667.4728             nan     0.1000 16996.2998
    ##      2 70440488.2300             nan     0.1000 109941.7628
    ##      3 69896477.2867             nan     0.1000 -188826.2262
    ##      4 69598981.4460             nan     0.1000 71832.6930
    ##      5 69391227.4495             nan     0.1000 42210.3375
    ##      6 69367252.4967             nan     0.1000 -115852.5798
    ##      7 69164916.4770             nan     0.1000 -68725.4015
    ##      8 68967913.6988             nan     0.1000 -50515.5786
    ##      9 67116019.6491             nan     0.1000 29221.5317
    ##     10 66953465.4344             nan     0.1000 -143024.8353
    ##     20 65179092.0541             nan     0.1000 -69927.8306
    ##     40 63281777.5724             nan     0.1000 -96174.9727
    ##     60 60831876.9239             nan     0.1000 -101028.7787
    ##     80 59924437.6757             nan     0.1000 -163832.7630
    ##    100 59053148.2493             nan     0.1000 -141848.4234
    ##    120 58260476.5401             nan     0.1000 -183649.3857
    ##    140 57703702.5664             nan     0.1000 -70531.2676
    ##    160 56556704.7618             nan     0.1000 -100626.4669
    ##    180 55996927.1438             nan     0.1000 -199723.5540
    ##    200 55218119.1822             nan     0.1000 -86030.0200
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 70677007.6983             nan     0.1000 256907.0290
    ##      2 70309183.2443             nan     0.1000 207226.7316
    ##      3 70124434.4692             nan     0.1000 131484.0760
    ##      4 69863788.7404             nan     0.1000 -187664.1532
    ##      5 69337812.2897             nan     0.1000 -11546.4108
    ##      6 68956290.9088             nan     0.1000 -181198.0171
    ##      7 68768170.8671             nan     0.1000 2733.5131
    ##      8 68666902.6526             nan     0.1000 -169464.5944
    ##      9 68230287.6968             nan     0.1000 -465092.6778
    ##     10 68152228.3845             nan     0.1000 -43626.8705
    ##     20 64259612.5333             nan     0.1000 -60264.6814
    ##     40 60624918.3149             nan     0.1000 -178753.5153
    ##     60 58635350.1878             nan     0.1000 -80190.9300
    ##     80 56480225.3223             nan     0.1000 -684933.8571
    ##    100 54920744.8880             nan     0.1000 -326582.1143
    ##    120 53155100.3472             nan     0.1000 -275839.1056
    ##    140 51636836.3025             nan     0.1000 -610121.1551
    ##    160 50575014.9057             nan     0.1000 -160235.4046
    ##    180 49032217.4139             nan     0.1000 -142050.6635
    ##    200 48277768.2736             nan     0.1000 -829281.8920
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 70644372.2419             nan     0.1000 110709.2163
    ##      2 69748679.1527             nan     0.1000 -66686.8760
    ##      3 69104260.7754             nan     0.1000 26875.6514
    ##      4 68987466.2357             nan     0.1000 -44470.2758
    ##      5 68892705.1244             nan     0.1000 -7470.7847
    ##      6 68493044.2273             nan     0.1000 -73586.3715
    ##      7 67109810.1104             nan     0.1000 234486.4098
    ##      8 66706547.0272             nan     0.1000 -112748.2611
    ##      9 65262227.0698             nan     0.1000 -195829.3904
    ##     10 64653712.4047             nan     0.1000 -222891.5969
    ##     20 62132876.6058             nan     0.1000 -105993.0778
    ##     40 57492630.7779             nan     0.1000 -59717.7033
    ##     60 54769906.9839             nan     0.1000 -459643.4598
    ##     80 52969336.2227             nan     0.1000 -314713.6748
    ##    100 50778197.5769             nan     0.1000 -167034.4037
    ##    120 48772500.9286             nan     0.1000 -101196.4527
    ##    140 47039904.7529             nan     0.1000 -125766.2920
    ##    160 45650446.3318             nan     0.1000 -38526.0081
    ##    180 43634915.0164             nan     0.1000 -392160.8135
    ##    200 42239075.8475             nan     0.1000 -111752.9366
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 114639865.1754             nan     0.1000 -21571.9995
    ##      2 114608018.6926             nan     0.1000 1114.8712
    ##      3 114560738.0346             nan     0.1000 -63745.0809
    ##      4 114406118.8125             nan     0.1000 19738.3822
    ##      5 114358643.3781             nan     0.1000 -73584.0105
    ##      6 114292082.0463             nan     0.1000 -55385.2502
    ##      7 114236169.8087             nan     0.1000 -79994.0453
    ##      8 114125979.9728             nan     0.1000 10877.0812
    ##      9 114035148.0656             nan     0.1000 -27053.9350
    ##     10 114056656.1913             nan     0.1000 -150458.1477
    ##     20 113642722.6960             nan     0.1000 -49488.3670
    ##     40 113211189.6822             nan     0.1000 -134724.3109
    ##     60 112659274.6578             nan     0.1000 -61500.1825
    ##     80 112258043.9107             nan     0.1000 -153453.2547
    ##    100 111988568.7964             nan     0.1000 -134500.5062
    ##    120 111438483.2947             nan     0.1000 -60529.4242
    ##    140 111126050.2682             nan     0.1000 -149404.8132
    ##    160 110892529.4548             nan     0.1000 -87041.9915
    ##    180 110623385.6079             nan     0.1000 -30208.0009
    ##    200 110241166.5948             nan     0.1000 -99071.5609
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 114639346.8087             nan     0.1000 18534.1319
    ##      2 114336009.1777             nan     0.1000 -45428.1007
    ##      3 114201199.8112             nan     0.1000 1868.1160
    ##      4 113780429.4224             nan     0.1000 -171976.8091
    ##      5 113613817.5664             nan     0.1000 -64940.2141
    ##      6 113411200.6528             nan     0.1000 -159608.0998
    ##      7 112289080.2514             nan     0.1000 -134171.9588
    ##      8 112150282.7917             nan     0.1000 -103848.9249
    ##      9 111932105.6604             nan     0.1000 -153901.1924
    ##     10 111822349.6225             nan     0.1000 -101700.1879
    ##     20 110203052.2811             nan     0.1000 -64983.3578
    ##     40 107852368.3434             nan     0.1000 -66690.5311
    ##     60 105672524.6531             nan     0.1000 -399070.8025
    ##     80 103553482.6022             nan     0.1000 -256613.9971
    ##    100 101321183.1914             nan     0.1000 -315445.5997
    ##    120 100029124.3321             nan     0.1000 -186691.6835
    ##    140 98625798.3691             nan     0.1000 -127170.0467
    ##    160 97571589.4299             nan     0.1000 -137749.3971
    ##    180 96910753.6492             nan     0.1000 -254482.5592
    ##    200 95664766.5166             nan     0.1000 -223798.3132
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 114046164.7058             nan     0.1000 -191940.2866
    ##      2 113658409.3768             nan     0.1000 -165726.2351
    ##      3 113506474.0074             nan     0.1000 -42095.0904
    ##      4 112989239.5258             nan     0.1000 -69480.4117
    ##      5 112920192.2554             nan     0.1000 -78245.2931
    ##      6 112440216.7669             nan     0.1000 -320718.2949
    ##      7 112248068.6634             nan     0.1000 -184296.6049
    ##      8 111756645.1416             nan     0.1000 -313423.6921
    ##      9 110776725.7454             nan     0.1000 400725.2985
    ##     10 110571002.0901             nan     0.1000 -128329.3643
    ##     20 106242864.2669             nan     0.1000 -273565.2636
    ##     40 100872068.3624             nan     0.1000 -345819.3830
    ##     60 97510856.4168             nan     0.1000 -242474.5273
    ##     80 94840926.3699             nan     0.1000 -159485.9600
    ##    100 91950833.0920             nan     0.1000 -140952.8185
    ##    120 89551249.4977             nan     0.1000 -526296.5266
    ##    140 87453044.6898             nan     0.1000 -356653.2523
    ##    160 85807067.4535             nan     0.1000 -216818.3065
    ##    180 83870377.3430             nan     0.1000 -196776.5250
    ##    200 82397457.1817             nan     0.1000 -487595.7741
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 114171478.5614             nan     0.1000  448.2985
    ##      2 113579359.8210             nan     0.1000 -39212.4670
    ##      3 113379410.0584             nan     0.1000 -43400.2882
    ##      4 112866644.8378             nan     0.1000 -145828.3459
    ##      5 112045837.4431             nan     0.1000 -268497.7291
    ##      6 111009685.0074             nan     0.1000 -66691.6341
    ##      7 110189925.1531             nan     0.1000 -298466.9637
    ##      8 109730695.4605             nan     0.1000 -415067.8902
    ##      9 109072617.6028             nan     0.1000 -187610.2607
    ##     10 108793706.5127             nan     0.1000 -47557.1921
    ##     20 103762908.5466             nan     0.1000 -291626.6110
    ##     40 97027921.6582             nan     0.1000 -312129.9858
    ##     60 92150743.6943             nan     0.1000 -339494.0558
    ##     80 88505798.0243             nan     0.1000 -317932.5846
    ##    100 85796455.5048             nan     0.1000 -259135.0793
    ##    120 84000821.0092             nan     0.1000 -581606.9935
    ##    140 80889246.6103             nan     0.1000 -511040.4310
    ##    160 77439514.8314             nan     0.1000 -324717.4009
    ##    180 75111578.9566             nan     0.1000 -151001.7248
    ##    200 73797218.5133             nan     0.1000 -172831.1642
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 79447734.9823             nan     0.1000 -19922.4376
    ##      2 79175039.3631             nan     0.1000 15810.5984
    ##      3 79122295.5037             nan     0.1000 10303.4962
    ##      4 79067254.9621             nan     0.1000 -25298.2631
    ##      5 79014573.4884             nan     0.1000 -37070.6093
    ##      6 78877989.3280             nan     0.1000 139527.8078
    ##      7 78663167.4633             nan     0.1000 -55594.7464
    ##      8 78632948.0640             nan     0.1000 -52113.3933
    ##      9 78599277.2374             nan     0.1000 -34029.2518
    ##     10 78588260.1644             nan     0.1000 -37502.4500
    ##     20 77993743.2432             nan     0.1000 -41145.1804
    ##     40 77519578.0653             nan     0.1000 -133793.9535
    ##     60 77235613.3095             nan     0.1000 -128171.2245
    ##     80 76831959.0187             nan     0.1000 -48517.7792
    ##    100 76599829.1134             nan     0.1000 -42265.8462
    ##    120 76330003.5135             nan     0.1000 -111400.1563
    ##    140 76127917.4570             nan     0.1000 -69717.6671
    ##    160 75963195.9387             nan     0.1000 -128951.9569
    ##    180 75751673.6510             nan     0.1000 -94333.4624
    ##    200 75627364.4146             nan     0.1000 -56723.9781
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 79418214.2917             nan     0.1000 -47516.3998
    ##      2 79080884.2191             nan     0.1000 46545.5501
    ##      3 78747371.1379             nan     0.1000 -169070.1052
    ##      4 78513572.4564             nan     0.1000 -99079.5804
    ##      5 78391477.8608             nan     0.1000 -75625.2429
    ##      6 78066569.3437             nan     0.1000 -78576.4882
    ##      7 77785683.1746             nan     0.1000 140989.6661
    ##      8 77733845.1826             nan     0.1000 -62106.4577
    ##      9 77596349.3391             nan     0.1000 -102997.7928
    ##     10 77443667.9709             nan     0.1000 -146902.3810
    ##     20 76448168.9642             nan     0.1000 -169651.7896
    ##     40 75220860.6305             nan     0.1000 -218255.4920
    ##     60 73664646.7821             nan     0.1000 -501193.7307
    ##     80 72453105.9597             nan     0.1000 -155865.5357
    ##    100 70649355.0371             nan     0.1000 -219599.2690
    ##    120 70048005.6097             nan     0.1000 -205720.7320
    ##    140 69482345.5957             nan     0.1000 -127421.7224
    ##    160 68970404.5317             nan     0.1000 -137885.6981
    ##    180 67540585.5239             nan     0.1000 -175507.6777
    ##    200 66956828.7922             nan     0.1000 -158417.8585
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 79044604.0187             nan     0.1000 -130975.7057
    ##      2 78728755.5913             nan     0.1000 200448.8684
    ##      3 78438367.8677             nan     0.1000 -63914.8142
    ##      4 77810832.7655             nan     0.1000 -146476.4018
    ##      5 77550030.7511             nan     0.1000 -171819.9058
    ##      6 77407159.6098             nan     0.1000 -81623.1444
    ##      7 77229755.9446             nan     0.1000 -48372.4237
    ##      8 76944458.9248             nan     0.1000 -200141.3549
    ##      9 76340846.9836             nan     0.1000 -254538.9708
    ##     10 76164730.9030             nan     0.1000 -212512.0425
    ##     20 74316544.3504             nan     0.1000 -56232.7449
    ##     40 70793135.3279             nan     0.1000 -299872.9263
    ##     60 68258613.2638             nan     0.1000 -152678.5493
    ##     80 66040352.9643             nan     0.1000 -96434.9848
    ##    100 64735604.3010             nan     0.1000 -217219.4024
    ##    120 63585371.0990             nan     0.1000 -84682.3185
    ##    140 62160267.1696             nan     0.1000 -329730.9025
    ##    160 60421675.2996             nan     0.1000 -174658.8005
    ##    180 59239396.1036             nan     0.1000 -22230.9999
    ##    200 58072608.1501             nan     0.1000 -192463.2921
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 79309393.5769             nan     0.1000 -71286.9823
    ##      2 78747199.0844             nan     0.1000 120061.6826
    ##      3 78082893.4768             nan     0.1000 -119399.1132
    ##      4 77554061.4764             nan     0.1000 -89509.2505
    ##      5 76827824.7216             nan     0.1000 -217051.3072
    ##      6 76262897.2937             nan     0.1000 -118755.5817
    ##      7 75818164.3455             nan     0.1000 -287686.5432
    ##      8 75708364.4089             nan     0.1000 -46275.4944
    ##      9 75245993.4956             nan     0.1000 -134986.9540
    ##     10 74911326.4333             nan     0.1000 -139841.6143
    ##     20 72039888.1073             nan     0.1000 -258674.5225
    ##     40 67802834.8029             nan     0.1000 -435704.0698
    ##     60 64022168.8090             nan     0.1000 -97557.1423
    ##     80 61239178.5338             nan     0.1000 -223241.2411
    ##    100 58844748.2387             nan     0.1000 -327392.0862
    ##    120 56830904.8566             nan     0.1000 -220642.5126
    ##    140 55190316.4953             nan     0.1000 -128131.6476
    ##    160 53807415.8259             nan     0.1000 -332991.1114
    ##    180 52212634.7791             nan     0.1000 -157020.6638
    ##    200 50581938.6673             nan     0.1000 -29869.2002
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 116364658.0795             nan     0.1000 -34927.6003
    ##      2 116268340.5394             nan     0.1000 67088.9612
    ##      3 116068060.4593             nan     0.1000 -21995.7945
    ##      4 115993086.8758             nan     0.1000 23306.7724
    ##      5 115819086.5789             nan     0.1000 64886.3078
    ##      6 115756715.9651             nan     0.1000 -2152.5143
    ##      7 115708368.0987             nan     0.1000 -51896.5845
    ##      8 115638917.0953             nan     0.1000 -59788.4392
    ##      9 115527512.2654             nan     0.1000 -6414.3251
    ##     10 115432150.1854             nan     0.1000 -67388.8428
    ##     20 114657459.4763             nan     0.1000 -95372.8894
    ##     40 113892577.6598             nan     0.1000 -38429.1649
    ##     60 113461986.6664             nan     0.1000 -80203.6689
    ##     80 112905516.6302             nan     0.1000 -158232.0496
    ##    100 112337374.5396             nan     0.1000 -87841.0162
    ##    120 111975629.5399             nan     0.1000 -104811.6650
    ##    140 111628503.4159             nan     0.1000 -84113.4130
    ##    160 111339959.9411             nan     0.1000 -110168.7397
    ##    180 111006625.3337             nan     0.1000 -46378.5941
    ##    200 110804435.2404             nan     0.1000 -16164.9370
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 115984816.6940             nan     0.1000 51105.6109
    ##      2 115750005.1117             nan     0.1000 32105.4861
    ##      3 115450276.1149             nan     0.1000 -234227.4437
    ##      4 115252012.2925             nan     0.1000 51467.9473
    ##      5 115050368.5959             nan     0.1000 -145546.1316
    ##      6 114963136.6430             nan     0.1000 -93661.2018
    ##      7 114778142.8446             nan     0.1000 -158695.6460
    ##      8 114277306.6232             nan     0.1000 -171286.0662
    ##      9 114135760.7325             nan     0.1000 -205047.5891
    ##     10 114029277.0352             nan     0.1000 -145232.3173
    ##     20 112544977.9213             nan     0.1000 -430552.4915
    ##     40 108016991.2892             nan     0.1000 -331848.4673
    ##     60 105139702.8021             nan     0.1000 -186187.0804
    ##     80 103963228.6738             nan     0.1000 -74894.2943
    ##    100 102617740.0256             nan     0.1000 -75295.1178
    ##    120 101440582.0270             nan     0.1000 -178883.2698
    ##    140 100562372.8161             nan     0.1000 -246939.6648
    ##    160 99595920.2115             nan     0.1000 -16904.5323
    ##    180 98645047.9937             nan     0.1000 -406781.2930
    ##    200 97632112.7518             nan     0.1000 -122679.5967
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 115959862.2782             nan     0.1000 -95381.8779
    ##      2 115310784.7100             nan     0.1000 49389.5937
    ##      3 114915568.0452             nan     0.1000 56839.5372
    ##      4 114439321.3868             nan     0.1000 -133654.3471
    ##      5 114011378.0397             nan     0.1000 -375419.4170
    ##      6 113836673.7131             nan     0.1000 -89566.3929
    ##      7 113720423.0100             nan     0.1000 -186836.7047
    ##      8 113433858.1100             nan     0.1000 -162169.0027
    ##      9 113252333.8326             nan     0.1000 -93686.2928
    ##     10 112648202.3912             nan     0.1000 -76600.9903
    ##     20 109176993.8632             nan     0.1000 -5929.1026
    ##     40 104218880.3562             nan     0.1000 -138295.0947
    ##     60 101140262.0707             nan     0.1000 -406108.8874
    ##     80 99076662.4467             nan     0.1000 -69993.9648
    ##    100 96196578.9401             nan     0.1000 -295278.1450
    ##    120 93621213.0136             nan     0.1000 -173131.0536
    ##    140 91767373.5495             nan     0.1000 -197776.9263
    ##    160 89572369.7517             nan     0.1000 -123705.5339
    ##    180 87851929.0416             nan     0.1000 -181318.4061
    ##    200 86718220.8831             nan     0.1000 -292163.5516
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 116260610.0943             nan     0.1000 -143213.2659
    ##      2 114837086.1036             nan     0.1000 -161321.3234
    ##      3 113813185.4069             nan     0.1000 -260782.5661
    ##      4 113468898.7442             nan     0.1000 -53432.2985
    ##      5 112595165.1439             nan     0.1000 -145646.7122
    ##      6 112307747.6868             nan     0.1000 -160789.3794
    ##      7 112220929.1671             nan     0.1000 -120532.6167
    ##      8 111596286.0661             nan     0.1000 -323552.1283
    ##      9 111298587.7954             nan     0.1000 -194016.7500
    ##     10 111108030.9991             nan     0.1000 -213313.4951
    ##     20 106974911.7079             nan     0.1000 -206120.8098
    ##     40 100732927.0003             nan     0.1000 -713564.2341
    ##     60 96611080.2495             nan     0.1000 -75628.9726
    ##     80 92736736.1563             nan     0.1000 -221406.2037
    ##    100 88760090.8631             nan     0.1000 -95316.9186
    ##    120 85434803.2483             nan     0.1000 -491120.9481
    ##    140 82761162.9444             nan     0.1000 -507921.1481
    ##    160 80018627.9609             nan     0.1000 -476771.8696
    ##    180 77744244.0199             nan     0.1000 -630338.4294
    ##    200 75599614.0922             nan     0.1000 -490514.8308
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 113837114.7829             nan     0.1000 85198.5881
    ##      2 113657468.8903             nan     0.1000 13776.2495
    ##      3 113549090.1274             nan     0.1000 -35920.5158
    ##      4 113460726.3087             nan     0.1000 45184.5818
    ##      5 113373996.7648             nan     0.1000 10834.4271
    ##      6 113264246.4260             nan     0.1000 -117172.0722
    ##      7 113186739.3910             nan     0.1000 -51563.5118
    ##      8 113145414.9442             nan     0.1000 -176167.5209
    ##      9 113010219.4353             nan     0.1000 -16916.5234
    ##     10 112930350.6423             nan     0.1000 -7321.3607
    ##     20 112291062.8596             nan     0.1000 -37742.5373
    ##     40 111731854.0872             nan     0.1000 -116915.6821
    ##     60 111106218.2829             nan     0.1000 -59506.0740
    ##     80 110819279.4830             nan     0.1000 -73279.1804
    ##    100 110467160.0898             nan     0.1000 -142614.7661
    ##    120 110036565.4532             nan     0.1000 -62947.1142
    ##    140 109641171.9797             nan     0.1000 -53041.2997
    ##    160 109228975.6030             nan     0.1000 -199633.8367
    ##    180 108915174.8916             nan     0.1000 -181870.4295
    ##    200 108523638.3611             nan     0.1000 -19604.2008
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 113719549.4549             nan     0.1000 -51571.0835
    ##      2 113569232.9078             nan     0.1000 -16831.1919
    ##      3 113338367.9370             nan     0.1000 27362.7217
    ##      4 113145639.4707             nan     0.1000 -148322.1760
    ##      5 112964072.7064             nan     0.1000 -172464.6218
    ##      6 112561250.5219             nan     0.1000 -27482.0545
    ##      7 112443850.3763             nan     0.1000 -59058.3198
    ##      8 112349450.8079             nan     0.1000 -67685.6370
    ##      9 112160941.5160             nan     0.1000 -80115.9915
    ##     10 111934051.3664             nan     0.1000 -15338.7340
    ##     20 110885933.2790             nan     0.1000 -34508.5558
    ##     40 107585726.7560             nan     0.1000 -205603.2173
    ##     60 104058283.8121             nan     0.1000 -185962.0010
    ##     80 101679921.1785             nan     0.1000 -32849.5132
    ##    100 100408059.9013             nan     0.1000 -571106.9108
    ##    120 99006274.2784             nan     0.1000 -164308.1044
    ##    140 98177328.5189             nan     0.1000 -227599.7123
    ##    160 96974297.3989             nan     0.1000 -199978.5332
    ##    180 96551932.4478             nan     0.1000 -131996.1088
    ##    200 95287491.7438             nan     0.1000 -73749.3546
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 113402158.9417             nan     0.1000 -105014.6251
    ##      2 113209768.3570             nan     0.1000 -96296.1157
    ##      3 112056565.7442             nan     0.1000 -405439.7236
    ##      4 111610529.6122             nan     0.1000 -217454.2591
    ##      5 111414560.2972             nan     0.1000 -82855.9958
    ##      6 110275800.8194             nan     0.1000 -67315.9918
    ##      7 110191358.4395             nan     0.1000 -135630.8919
    ##      8 110115452.5089             nan     0.1000 -189742.1968
    ##      9 109449932.7544             nan     0.1000 -232583.7469
    ##     10 108752670.7846             nan     0.1000 -301393.0946
    ##     20 106424081.9621             nan     0.1000 -242290.7405
    ##     40 100471814.9903             nan     0.1000 -385946.7284
    ##     60 96533717.2818             nan     0.1000 -156091.8428
    ##     80 93234847.0810             nan     0.1000 -196171.6135
    ##    100 90890822.5744             nan     0.1000 -163627.6016
    ##    120 89088850.0031             nan     0.1000 -192250.8948
    ##    140 86242595.9106             nan     0.1000 -92673.8206
    ##    160 84322346.1241             nan     0.1000 -226476.3139
    ##    180 82024749.1745             nan     0.1000 -267088.4620
    ##    200 80191610.3393             nan     0.1000 -253896.3905
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 113041420.2733             nan     0.1000 -393834.1847
    ##      2 112845691.7769             nan     0.1000 36317.2018
    ##      3 112010009.7383             nan     0.1000 -126765.6515
    ##      4 111400847.4517             nan     0.1000 -328880.9309
    ##      5 110823133.4698             nan     0.1000 -90643.3430
    ##      6 110623296.4239             nan     0.1000 -74458.8375
    ##      7 110093603.6212             nan     0.1000 -140854.7972
    ##      8 109673856.7751             nan     0.1000 -583726.8798
    ##      9 108973978.2632             nan     0.1000 19075.8810
    ##     10 108593129.6205             nan     0.1000 -442446.9827
    ##     20 103517981.3428             nan     0.1000 -228982.4309
    ##     40 96921862.2210             nan     0.1000 -367872.3292
    ##     60 91797483.0509             nan     0.1000 -489075.5169
    ##     80 88498722.8097             nan     0.1000 -347723.8010
    ##    100 85664686.7023             nan     0.1000 -166309.4157
    ##    120 82540141.5250             nan     0.1000 -250991.6272
    ##    140 78835896.0271             nan     0.1000 -127130.0135
    ##    160 76811139.0810             nan     0.1000 -462784.4571
    ##    180 74299262.5934             nan     0.1000 -336014.5067
    ##    200 71602715.3995             nan     0.1000 -157978.5082
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 117983811.1637             nan     0.1000 3697.9064
    ##      2 117908283.1189             nan     0.1000 -59009.2161
    ##      3 117703278.2771             nan     0.1000 -85662.1959
    ##      4 117641128.8167             nan     0.1000 -54791.6185
    ##      5 117576894.3827             nan     0.1000 -1964.3030
    ##      6 117514795.1940             nan     0.1000 -32593.7133
    ##      7 117358212.9487             nan     0.1000 -100463.0685
    ##      8 117282346.9200             nan     0.1000 -35589.0221
    ##      9 117276652.7229             nan     0.1000 -77431.5467
    ##     10 117228237.7394             nan     0.1000 -73338.2941
    ##     20 116766432.7928             nan     0.1000 -52942.6146
    ##     40 116189396.5844             nan     0.1000 -143945.7449
    ##     60 115466363.7885             nan     0.1000 -322959.8871
    ##     80 114835821.2624             nan     0.1000 -128957.3549
    ##    100 114451860.4090             nan     0.1000 -109191.7441
    ##    120 114205100.4904             nan     0.1000 -142904.0161
    ##    140 113983082.3224             nan     0.1000 -110574.8756
    ##    160 113839890.6583             nan     0.1000 -69070.1689
    ##    180 113411906.0344             nan     0.1000 -101629.0322
    ##    200 113154713.7124             nan     0.1000 -153528.9944
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 117907630.5206             nan     0.1000 93961.1264
    ##      2 117680248.4898             nan     0.1000 154625.4638
    ##      3 117484013.5798             nan     0.1000 -63768.6735
    ##      4 117429737.4536             nan     0.1000 -51134.6050
    ##      5 117153653.9522             nan     0.1000 -191436.6605
    ##      6 116964592.4522             nan     0.1000 -192396.9720
    ##      7 116643331.9850             nan     0.1000 -131035.7176
    ##      8 115634723.2742             nan     0.1000 273942.8574
    ##      9 115515736.5072             nan     0.1000 -309083.1984
    ##     10 115447940.2929             nan     0.1000 -173137.5860
    ##     20 113691995.6608             nan     0.1000 -53453.9242
    ##     40 110878394.8426             nan     0.1000 -127391.0664
    ##     60 108915946.1485             nan     0.1000 -194052.2446
    ##     80 107267666.9389             nan     0.1000 -219243.7966
    ##    100 104596392.6159             nan     0.1000 -220855.3356
    ##    120 103135381.5729             nan     0.1000 -195567.2772
    ##    140 101856661.6273             nan     0.1000 -230336.8786
    ##    160 100587439.8609             nan     0.1000 -234736.0472
    ##    180 99582657.9511             nan     0.1000 -216546.4371
    ##    200 98749654.2051             nan     0.1000 -154487.6625
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 117538166.4380             nan     0.1000 34495.0226
    ##      2 117042913.6903             nan     0.1000 -214290.5846
    ##      3 116917158.6957             nan     0.1000 -56728.5426
    ##      4 116664701.2625             nan     0.1000 -227992.7758
    ##      5 116439647.2733             nan     0.1000 -129393.4150
    ##      6 115749935.3856             nan     0.1000 -84544.8499
    ##      7 115443576.0347             nan     0.1000 26457.2112
    ##      8 115191888.0871             nan     0.1000 -380086.9992
    ##      9 114771946.6801             nan     0.1000 -166501.1973
    ##     10 114280562.1409             nan     0.1000 -276382.6929
    ##     20 111960576.1139             nan     0.1000 -328888.2064
    ##     40 105659279.8476             nan     0.1000 -143601.4707
    ##     60 100912815.0997             nan     0.1000 -155495.9069
    ##     80 97996626.7729             nan     0.1000 -340588.9276
    ##    100 95848679.8945             nan     0.1000 -409426.6769
    ##    120 94347983.5567             nan     0.1000 -122906.9800
    ##    140 92424034.5085             nan     0.1000 -381178.7812
    ##    160 90112230.9812             nan     0.1000 -164372.6391
    ##    180 87811006.5652             nan     0.1000 -300361.6767
    ##    200 86358135.3543             nan     0.1000 -210582.0779
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 117146761.2183             nan     0.1000 -19178.2500
    ##      2 116776750.3765             nan     0.1000 -90019.4225
    ##      3 116143111.6073             nan     0.1000 -182367.5681
    ##      4 115630897.4007             nan     0.1000 -252143.1327
    ##      5 114824201.0271             nan     0.1000 -232469.5681
    ##      6 114700520.7165             nan     0.1000 -225503.6242
    ##      7 114535780.9513             nan     0.1000 -97083.6835
    ##      8 114080616.1445             nan     0.1000 -235103.1479
    ##      9 113804994.5492             nan     0.1000 -425032.4712
    ##     10 113746422.2678             nan     0.1000 -193234.1883
    ##     20 108891428.7360             nan     0.1000 -386770.2942
    ##     40 100585582.9466             nan     0.1000 -340099.2904
    ##     60 96762689.0611             nan     0.1000 -1436888.8597
    ##     80 92742080.6591             nan     0.1000 -440404.4822
    ##    100 89633935.4037             nan     0.1000 -145801.7035
    ##    120 86992643.0547             nan     0.1000 -278556.9769
    ##    140 84707668.5678             nan     0.1000 -304822.4730
    ##    160 81926472.0633             nan     0.1000 -258243.3660
    ##    180 79439356.3469             nan     0.1000 -179034.0310
    ##    200 76944691.7849             nan     0.1000 -364486.0520
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 84653166.4587             nan     0.1000 68195.8392
    ##      2 84470194.6840             nan     0.1000 -29211.6463
    ##      3 84355370.6223             nan     0.1000 78141.3173
    ##      4 84128324.7300             nan     0.1000 -90046.9011
    ##      5 84046044.0365             nan     0.1000 -28313.4521
    ##      6 83861295.9965             nan     0.1000 8600.6642
    ##      7 83747010.4732             nan     0.1000 14298.9419
    ##      8 83698779.4980             nan     0.1000 11755.8157
    ##      9 83669303.4872             nan     0.1000 -153510.0179
    ##     10 83572472.0887             nan     0.1000 48624.5405
    ##     20 83231761.1801             nan     0.1000 -64550.0427
    ##     40 82763466.0054             nan     0.1000 -79349.3214
    ##     60 82278882.0506             nan     0.1000 -62411.8089
    ##     80 81919905.9403             nan     0.1000 -31506.0991
    ##    100 81541580.8354             nan     0.1000 -80794.5507
    ##    120 81206521.8006             nan     0.1000 -35453.9151
    ##    140 81172864.1383             nan     0.1000 -76812.3558
    ##    160 80873590.0694             nan     0.1000 -46039.1684
    ##    180 80560073.3349             nan     0.1000 -66462.4720
    ##    200 80287533.2509             nan     0.1000 -3032.0566
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 84193091.2089             nan     0.1000 -10924.1776
    ##      2 83820674.5978             nan     0.1000 26596.3445
    ##      3 83639652.5915             nan     0.1000 91262.3612
    ##      4 83210426.6467             nan     0.1000 -204872.3600
    ##      5 83127789.4620             nan     0.1000 -47669.4930
    ##      6 82111593.9878             nan     0.1000 563659.4414
    ##      7 81876184.7480             nan     0.1000 -131170.6735
    ##      8 81695045.9173             nan     0.1000 -12064.5504
    ##      9 81618057.8836             nan     0.1000 -71379.4956
    ##     10 81498977.1071             nan     0.1000 -18916.1847
    ##     20 78983658.6379             nan     0.1000 -44464.3566
    ##     40 75349784.7476             nan     0.1000 -245651.5528
    ##     60 74120221.6383             nan     0.1000 -68067.7540
    ##     80 72956681.4240             nan     0.1000 -165840.1843
    ##    100 71757923.0651             nan     0.1000 -266064.3835
    ##    120 70995232.7645             nan     0.1000 -138267.1316
    ##    140 69536121.2357             nan     0.1000 -74101.5567
    ##    160 68627682.4927             nan     0.1000 -66809.9863
    ##    180 67717723.0065             nan     0.1000 -94194.1450
    ##    200 66698095.9561             nan     0.1000 -820027.1060
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 83832966.4285             nan     0.1000 -112346.6091
    ##      2 83732458.0374             nan     0.1000 39682.0063
    ##      3 83271730.7639             nan     0.1000 -110620.9600
    ##      4 81932990.5253             nan     0.1000 -260391.2283
    ##      5 81278865.5361             nan     0.1000 -145184.1667
    ##      6 80811134.9358             nan     0.1000 -135386.8106
    ##      7 80659122.3314             nan     0.1000 -180779.4543
    ##      8 80305388.8984             nan     0.1000 -56743.4120
    ##      9 79675039.6469             nan     0.1000 78791.2676
    ##     10 79480195.0063             nan     0.1000 -268166.2648
    ##     20 75654761.1509             nan     0.1000 -56335.9862
    ##     40 71182675.7028             nan     0.1000 -27700.3155
    ##     60 67979455.5952             nan     0.1000 -130520.1408
    ##     80 65909565.6496             nan     0.1000 -88215.4877
    ##    100 64228879.7099             nan     0.1000 -81853.6337
    ##    120 62738410.8121             nan     0.1000 -259545.4963
    ##    140 60698659.5417             nan     0.1000 -200400.9686
    ##    160 58779782.8406             nan     0.1000 -86831.3983
    ##    180 56841653.5794             nan     0.1000 -136142.5921
    ##    200 55111001.0446             nan     0.1000 -589798.4729
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 84590447.5747             nan     0.1000 54585.3114
    ##      2 83626318.4668             nan     0.1000 -171153.2461
    ##      3 83335274.1433             nan     0.1000 -2053.7456
    ##      4 83180461.4371             nan     0.1000 41393.7552
    ##      5 82773750.5399             nan     0.1000 32269.2608
    ##      6 82511450.8323             nan     0.1000 -166045.1402
    ##      7 82174261.8080             nan     0.1000 -253268.5774
    ##      8 81621842.5293             nan     0.1000 -169987.5897
    ##      9 80955946.1828             nan     0.1000 -35594.3953
    ##     10 80408223.3398             nan     0.1000 -171240.6885
    ##     20 74371288.1550             nan     0.1000 353848.7248
    ##     40 70090191.2938             nan     0.1000 -397738.4491
    ##     60 66226961.6823             nan     0.1000 -594422.8500
    ##     80 63254279.8510             nan     0.1000 -256996.4516
    ##    100 60638015.2182             nan     0.1000 -175580.8325
    ##    120 58860641.7158             nan     0.1000 -120306.1325
    ##    140 56908145.7716             nan     0.1000 -184957.1995
    ##    160 54642018.3540             nan     0.1000 -225050.1721
    ##    180 53064680.3135             nan     0.1000 -181414.7982
    ##    200 50630503.4882             nan     0.1000 -279249.7469
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 109355376.2159             nan     0.1000 15538.9584
    ##      2 109305621.1811             nan     0.1000 -22048.2028
    ##      3 109231520.9030             nan     0.1000 -22041.8628
    ##      4 109108610.0358             nan     0.1000 22645.4426
    ##      5 109051972.6860             nan     0.1000 -35970.7113
    ##      6 108995565.1363             nan     0.1000 -115863.1488
    ##      7 108960863.3594             nan     0.1000 -24103.9690
    ##      8 108845204.8110             nan     0.1000 30067.3828
    ##      9 108798411.7174             nan     0.1000 -125702.3905
    ##     10 108768591.8889             nan     0.1000 -110444.2058
    ##     20 108200367.5319             nan     0.1000 1537.9493
    ##     40 107526641.1238             nan     0.1000 -117206.4705
    ##     60 106985834.3002             nan     0.1000 -160793.5322
    ##     80 106459894.1776             nan     0.1000 -152418.1572
    ##    100 106194690.7008             nan     0.1000 -81970.9217
    ##    120 105811376.2517             nan     0.1000 -48091.2903
    ##    140 105490119.4141             nan     0.1000 -118578.0020
    ##    160 105261877.1832             nan     0.1000 -56404.2929
    ##    180 104981594.8978             nan     0.1000 -132819.5504
    ##    200 104742923.6661             nan     0.1000 -84279.0680
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 109268057.4520             nan     0.1000 20393.9240
    ##      2 108678985.1293             nan     0.1000 -11123.0623
    ##      3 108596603.2050             nan     0.1000 -21886.5130
    ##      4 108546977.5600             nan     0.1000 -95946.8257
    ##      5 108327921.7002             nan     0.1000 -73581.9681
    ##      6 108138124.5684             nan     0.1000 -44377.9848
    ##      7 107970247.0371             nan     0.1000 -142211.8548
    ##      8 107642790.8333             nan     0.1000 -100761.3598
    ##      9 107479194.6998             nan     0.1000 -199666.6163
    ##     10 107416276.4183             nan     0.1000 -89103.1397
    ##     20 105761059.2588             nan     0.1000 -200780.7494
    ##     40 102028288.5065             nan     0.1000 -27365.9183
    ##     60 99261553.6083             nan     0.1000 -180980.9257
    ##     80 97455617.7889             nan     0.1000 -179187.5967
    ##    100 95108803.3898             nan     0.1000 -400342.2652
    ##    120 93888771.8666             nan     0.1000 -281638.8452
    ##    140 92791996.3494             nan     0.1000 -45090.2459
    ##    160 92093412.5378             nan     0.1000 -135801.7719
    ##    180 91157877.2051             nan     0.1000 -221076.6919
    ##    200 89882574.6066             nan     0.1000 -150254.0829
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 109187382.6478             nan     0.1000 12833.1892
    ##      2 108955556.6848             nan     0.1000 -64907.4674
    ##      3 108726497.2122             nan     0.1000 36011.0586
    ##      4 108582941.4277             nan     0.1000 24244.3142
    ##      5 107843791.1372             nan     0.1000 -14868.1276
    ##      6 107242070.7712             nan     0.1000 -15370.3827
    ##      7 107040719.3917             nan     0.1000 -113043.5850
    ##      8 106452513.4524             nan     0.1000 -186251.7438
    ##      9 106204178.8911             nan     0.1000 -167467.3410
    ##     10 105966957.0588             nan     0.1000 -85460.8547
    ##     20 101483340.2106             nan     0.1000 -118867.7650
    ##     40 96441256.5450             nan     0.1000 -317371.9154
    ##     60 93030959.7345             nan     0.1000 -196244.7688
    ##     80 90343835.9320             nan     0.1000 -359932.0706
    ##    100 87534424.8914             nan     0.1000 -107770.3070
    ##    120 85185058.8415             nan     0.1000 -177322.7185
    ##    140 83457243.6424             nan     0.1000 -107462.7108
    ##    160 81396080.3759             nan     0.1000 -145330.7901
    ##    180 79376081.7241             nan     0.1000 -210322.7674
    ##    200 78524774.0682             nan     0.1000 -124699.7528
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 108648239.9633             nan     0.1000 -129563.1638
    ##      2 108049864.0212             nan     0.1000 -77201.8702
    ##      3 106511891.7604             nan     0.1000 -486945.4855
    ##      4 105832340.8036             nan     0.1000 -140125.1189
    ##      5 105280998.3815             nan     0.1000 -15207.1922
    ##      6 104829065.7579             nan     0.1000 -70834.0117
    ##      7 104440925.7314             nan     0.1000 -264587.0457
    ##      8 104156785.5339             nan     0.1000 -273950.9351
    ##      9 104071321.0322             nan     0.1000 -131823.0800
    ##     10 104023170.8591             nan     0.1000 -68244.2669
    ##     20 100281936.5898             nan     0.1000 -232241.1799
    ##     40 91810170.6335             nan     0.1000 -67053.1598
    ##     60 87953396.7439             nan     0.1000 -275278.3307
    ##     80 84136563.3466             nan     0.1000 -30127.6346
    ##    100 81147248.8474             nan     0.1000 -442781.8588
    ##    120 78940310.9560             nan     0.1000 -303434.3424
    ##    140 75977353.7279             nan     0.1000 -67738.5931
    ##    160 74047790.7453             nan     0.1000 -193516.6563
    ##    180 72014559.1661             nan     0.1000 -277248.0573
    ##    200 69707687.6326             nan     0.1000 -198201.3589
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 100943724.2811             nan     0.1000 271510.0917
    ##      2 100694399.8906             nan     0.1000 -78130.9680
    ##      3 100552830.7606             nan     0.1000 55438.5393
    ##      4 100447725.7021             nan     0.1000 16796.1457
    ##      5 100367735.8830             nan     0.1000 18868.6443
    ##      6 100276088.9933             nan     0.1000 -29116.6052
    ##      7 100209991.0173             nan     0.1000 8359.7674
    ##      8 100139451.5519             nan     0.1000 1704.3805
    ##      9 100028666.6669             nan     0.1000 48421.5485
    ##     10 99931397.8115             nan     0.1000 19100.0650
    ##     20 99518220.0812             nan     0.1000 24082.8035
    ##     40 98907759.8489             nan     0.1000 5824.0464
    ##     60 98472087.6598             nan     0.1000 -63393.2280
    ##     80 98140762.5407             nan     0.1000 -85777.8311
    ##    100 97744916.0343             nan     0.1000 -193425.4776
    ##    120 97437637.8638             nan     0.1000 -152098.9481
    ##    140 97192574.9039             nan     0.1000 -2927.3685
    ##    160 96868188.9793             nan     0.1000 -121910.3252
    ##    180 96614107.8108             nan     0.1000 -163307.0165
    ##    200 96430500.2445             nan     0.1000 -36963.3200
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 101252232.9535             nan     0.1000 -136294.6374
    ##      2 100911377.9298             nan     0.1000 238985.2133
    ##      3 100562433.5480             nan     0.1000 -46432.7354
    ##      4 100428214.0669             nan     0.1000 -86075.0026
    ##      5 100271183.9310             nan     0.1000 51069.7233
    ##      6 99872450.0574             nan     0.1000 228370.1981
    ##      7 99698072.2232             nan     0.1000 -124828.3599
    ##      8 99594897.0374             nan     0.1000 -87642.1437
    ##      9 99542792.4412             nan     0.1000 -7031.9681
    ##     10 99337316.2506             nan     0.1000 -106508.4111
    ##     20 97106084.5682             nan     0.1000 -105565.7248
    ##     40 95797403.7244             nan     0.1000 -60056.7937
    ##     60 92646222.4636             nan     0.1000 -175745.4333
    ##     80 91501873.7976             nan     0.1000 -162529.9626
    ##    100 89417205.9496             nan     0.1000 -157886.4238
    ##    120 88672540.0024             nan     0.1000 -135018.1195
    ##    140 86683574.8860             nan     0.1000 -243266.8042
    ##    160 85579062.6573             nan     0.1000 -112838.3597
    ##    180 84782302.9826             nan     0.1000 -123781.5071
    ##    200 83511332.9192             nan     0.1000 -131936.5372
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 100559150.9304             nan     0.1000 -232776.4953
    ##      2 100192317.9144             nan     0.1000 -91163.4415
    ##      3 99706534.2322             nan     0.1000 310452.2095
    ##      4 99123535.9874             nan     0.1000 -219351.9485
    ##      5 98664452.8148             nan     0.1000 -112167.0255
    ##      6 97629938.4643             nan     0.1000 289886.4306
    ##      7 97523967.2089             nan     0.1000 -100865.9594
    ##      8 96719596.8838             nan     0.1000 -144768.3363
    ##      9 96649194.8008             nan     0.1000 -95082.9066
    ##     10 96441623.2718             nan     0.1000 23079.2520
    ##     20 93904404.2682             nan     0.1000 -288226.3005
    ##     40 90196430.1037             nan     0.1000 -53379.4704
    ##     60 86595587.8243             nan     0.1000 -231032.9205
    ##     80 84425089.3548             nan     0.1000 -197792.7749
    ##    100 81586160.2981             nan     0.1000 -633323.1704
    ##    120 79190968.2483             nan     0.1000 -480615.4810
    ##    140 76822097.4661             nan     0.1000 -281555.5226
    ##    160 74564205.8056             nan     0.1000 -188203.9488
    ##    180 72395924.5522             nan     0.1000 -254205.4084
    ##    200 70426837.7297             nan     0.1000 -163942.4176
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 100642908.7500             nan     0.1000 149675.9198
    ##      2 99624579.7587             nan     0.1000 -45806.8317
    ##      3 98948378.7407             nan     0.1000 23164.4032
    ##      4 98262370.4736             nan     0.1000 -137536.9088
    ##      5 97973155.0231             nan     0.1000 128594.8272
    ##      6 97275332.8537             nan     0.1000 -78352.3158
    ##      7 96650134.3269             nan     0.1000 -206830.3813
    ##      8 95725775.9728             nan     0.1000 -80612.8416
    ##      9 95355402.3423             nan     0.1000 -374224.6885
    ##     10 94285004.5366             nan     0.1000 158685.6948
    ##     20 89865156.4433             nan     0.1000 -130342.7183
    ##     40 85237990.9109             nan     0.1000 -420714.6469
    ##     60 80519117.5923             nan     0.1000 -245903.7112
    ##     80 76878195.0747             nan     0.1000 -115384.0467
    ##    100 74166962.9155             nan     0.1000 -129349.0357
    ##    120 71885814.7679             nan     0.1000 -383137.6472
    ##    140 69619671.2701             nan     0.1000 -376244.6393
    ##    160 67539506.9964             nan     0.1000 -305235.2056
    ##    180 65803035.4331             nan     0.1000 -715192.2408
    ##    200 63683190.1939             nan     0.1000 -211130.6730
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 81737985.9649             nan     0.1000 -70469.7419
    ##      2 81622796.1305             nan     0.1000 -31430.8364
    ##      3 81580411.5872             nan     0.1000 -30441.0385
    ##      4 81513460.6615             nan     0.1000 -36252.7189
    ##      5 81351119.5010             nan     0.1000 48559.7979
    ##      6 81209393.0329             nan     0.1000 -97466.8811
    ##      7 81120755.0605             nan     0.1000 30369.7562
    ##      8 81087419.0043             nan     0.1000 9457.1620
    ##      9 81034818.3687             nan     0.1000 -65192.7582
    ##     10 81008742.4829             nan     0.1000 -56411.3060
    ##     20 80545702.0056             nan     0.1000 -58716.4461
    ##     40 80096862.8536             nan     0.1000 -20137.6790
    ##     60 79837327.7568             nan     0.1000 -123646.5474
    ##     80 79496891.9849             nan     0.1000 -4754.5910
    ##    100 79268632.0878             nan     0.1000 -44469.1366
    ##    120 79097586.8295             nan     0.1000 -97698.4916
    ##    140 78825446.9891             nan     0.1000 -84239.8395
    ##    160 78571054.3094             nan     0.1000 -122714.3882
    ##    180 78509856.4151             nan     0.1000 -42020.2677
    ##    200 78333045.1330             nan     0.1000 -172217.6297
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 81585972.6513             nan     0.1000 16967.3226
    ##      2 81368387.9695             nan     0.1000 198517.8129
    ##      3 81080321.3044             nan     0.1000 -215046.0642
    ##      4 80788537.0460             nan     0.1000 -15914.7783
    ##      5 80518543.2773             nan     0.1000 -130886.8654
    ##      6 80347094.2034             nan     0.1000 -91509.7972
    ##      7 80218981.2102             nan     0.1000 -115310.0151
    ##      8 80096308.5678             nan     0.1000 -81441.6749
    ##      9 79947747.7447             nan     0.1000 -74997.8000
    ##     10 79809196.7786             nan     0.1000 -187723.7677
    ##     20 79168847.5552             nan     0.1000 -288204.6189
    ##     40 77555016.0385             nan     0.1000 -138515.6932
    ##     60 75960987.3065             nan     0.1000 -113795.9043
    ##     80 74744987.1868             nan     0.1000 -267573.5674
    ##    100 73679473.7796             nan     0.1000 -39536.3069
    ##    120 72745829.9677             nan     0.1000 -122098.3233
    ##    140 71078191.3621             nan     0.1000 -322440.8496
    ##    160 70299317.9593             nan     0.1000 -139776.3316
    ##    180 69718200.6421             nan     0.1000 -316856.7704
    ##    200 68820920.7903             nan     0.1000 -108107.9364
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 81567607.3005             nan     0.1000 -139390.9181
    ##      2 81390732.3815             nan     0.1000 -107125.2403
    ##      3 81043128.1422             nan     0.1000 104705.9384
    ##      4 80837465.9502             nan     0.1000 -3499.0789
    ##      5 80628846.3275             nan     0.1000 -93413.6188
    ##      6 80478574.8334             nan     0.1000 -128221.8363
    ##      7 80146721.3365             nan     0.1000 -68965.1192
    ##      8 79476656.7098             nan     0.1000 -147599.4587
    ##      9 79371119.7033             nan     0.1000 15618.6812
    ##     10 79307382.7865             nan     0.1000 -141843.3747
    ##     20 77430408.2235             nan     0.1000 -77035.7920
    ##     40 74251290.6529             nan     0.1000 -118682.3458
    ##     60 71947516.6769             nan     0.1000 -373785.9171
    ##     80 70344786.8262             nan     0.1000 -323555.3065
    ##    100 68509045.5059             nan     0.1000 -640685.2783
    ##    120 67109522.5719             nan     0.1000 -123984.0250
    ##    140 65449540.3659             nan     0.1000 -220335.0692
    ##    160 63353436.3559             nan     0.1000 -703863.2209
    ##    180 62085139.4235             nan     0.1000 -183228.1464
    ##    200 61059547.9730             nan     0.1000 -220430.7141
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 81601252.9964             nan     0.1000 -24347.3499
    ##      2 81174231.3616             nan     0.1000 92053.9243
    ##      3 80781922.0784             nan     0.1000 152792.0510
    ##      4 80499738.8820             nan     0.1000 4582.6532
    ##      5 80301829.7293             nan     0.1000 -38407.5955
    ##      6 79750967.4117             nan     0.1000 -170743.0382
    ##      7 79319063.6894             nan     0.1000 -64520.4507
    ##      8 79163790.5939             nan     0.1000 -212167.9534
    ##      9 78584091.8350             nan     0.1000 -450256.9449
    ##     10 78473324.9113             nan     0.1000 -76747.8088
    ##     20 75413696.8787             nan     0.1000 -30055.4312
    ##     40 71216598.5429             nan     0.1000 -191888.6296
    ##     60 68637212.0636             nan     0.1000 -42453.5798
    ##     80 66118062.8615             nan     0.1000 -415739.5065
    ##    100 64170871.0834             nan     0.1000 -146358.0888
    ##    120 62426233.6314             nan     0.1000 -230424.2899
    ##    140 60075919.0609             nan     0.1000 -74247.0892
    ##    160 57985834.8336             nan     0.1000 -133513.4463
    ##    180 55712345.9463             nan     0.1000 -285068.3763
    ##    200 54342577.2403             nan     0.1000 -430531.4066
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 119611678.3574             nan     0.1000 47569.4590
    ##      2 119455771.7932             nan     0.1000 39624.0105
    ##      3 119371099.3882             nan     0.1000 -93993.2366
    ##      4 119228474.4982             nan     0.1000 108242.7810
    ##      5 119157616.2594             nan     0.1000 -68217.4343
    ##      6 119035080.3261             nan     0.1000 70888.1951
    ##      7 118856534.5606             nan     0.1000 -169078.9730
    ##      8 118762875.8027             nan     0.1000 -3387.3677
    ##      9 118683932.1902             nan     0.1000 -21030.0227
    ##     10 118634683.2328             nan     0.1000 -21878.8960
    ##     20 118027876.4581             nan     0.1000 -83887.1718
    ##     40 117284134.0278             nan     0.1000 -87892.8948
    ##     60 116879382.2782             nan     0.1000 -254193.7831
    ##     80 116227256.7757             nan     0.1000 -9050.2852
    ##    100 115628325.2451             nan     0.1000 -22620.2059
    ##    120 115151378.4661             nan     0.1000 -43772.5126
    ##    140 114669097.1398             nan     0.1000 -174886.0175
    ##    160 114397302.0568             nan     0.1000 -115036.1730
    ##    180 113983104.7167             nan     0.1000 -64447.7506
    ##    200 113617856.4350             nan     0.1000 -53624.7466
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 119534870.5959             nan     0.1000 133417.7182
    ##      2 119351157.3753             nan     0.1000 22968.0172
    ##      3 119160356.0010             nan     0.1000 91256.1466
    ##      4 118792096.8126             nan     0.1000 -36436.5679
    ##      5 118687175.8115             nan     0.1000 -82537.5059
    ##      6 118301110.7127             nan     0.1000 -266916.8250
    ##      7 118154522.2465             nan     0.1000 -138517.4277
    ##      8 117594798.3108             nan     0.1000 -183782.0872
    ##      9 117268482.0372             nan     0.1000 17653.8399
    ##     10 117088210.8411             nan     0.1000 -133050.9500
    ##     20 115501936.2557             nan     0.1000 -78386.2014
    ##     40 110415291.9088             nan     0.1000 -143017.2507
    ##     60 108268416.2896             nan     0.1000 -261592.2929
    ##     80 106597152.3202             nan     0.1000 -268880.1075
    ##    100 104965427.1152             nan     0.1000 -313008.3212
    ##    120 102768667.2659             nan     0.1000 -178012.6115
    ##    140 101629880.1723             nan     0.1000 -167408.9513
    ##    160 100926421.6020             nan     0.1000 -411913.1235
    ##    180 99939812.6314             nan     0.1000 -299629.6332
    ##    200 99016419.0631             nan     0.1000 -201397.9872
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 119558959.8375             nan     0.1000 107226.8626
    ##      2 119091940.1037             nan     0.1000 -144436.3719
    ##      3 118768263.2211             nan     0.1000 -142646.8609
    ##      4 118499425.8364             nan     0.1000 -104787.1516
    ##      5 118192140.8794             nan     0.1000 -92145.1775
    ##      6 118177501.5486             nan     0.1000 -297432.8874
    ##      7 117389268.1437             nan     0.1000 -258264.5801
    ##      8 115819460.0738             nan     0.1000 457456.9662
    ##      9 115609835.8333             nan     0.1000 -202331.3142
    ##     10 115039860.6615             nan     0.1000 -140637.1550
    ##     20 110221578.0551             nan     0.1000 -176918.4681
    ##     40 105283821.6629             nan     0.1000 -328424.0326
    ##     60 101864392.9853             nan     0.1000 -308980.6160
    ##     80 99166774.9796             nan     0.1000 -189849.5480
    ##    100 96569336.1270             nan     0.1000 -408141.0968
    ##    120 94704169.1556             nan     0.1000 -274765.6734
    ##    140 92562578.9667             nan     0.1000 -393784.9258
    ##    160 91063306.4529             nan     0.1000 -563516.6467
    ##    180 88622017.7290             nan     0.1000 -358452.3748
    ##    200 86314689.9422             nan     0.1000 -239062.6169
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 119036630.3453             nan     0.1000 75488.7469
    ##      2 118368848.6143             nan     0.1000 -78539.8685
    ##      3 117791285.2009             nan     0.1000 -55443.0653
    ##      4 117047088.8358             nan     0.1000 -503986.5827
    ##      5 116737105.2810             nan     0.1000 88168.1762
    ##      6 116274766.1267             nan     0.1000 -234555.3384
    ##      7 115869138.9963             nan     0.1000 -44072.6197
    ##      8 115324768.5000             nan     0.1000 -211169.0041
    ##      9 114871517.4313             nan     0.1000 -156646.6177
    ##     10 112974492.5638             nan     0.1000 524154.5417
    ##     20 108025206.2026             nan     0.1000 -200860.1669
    ##     40 101858348.9689             nan     0.1000 -354881.8809
    ##     60 97099138.5932             nan     0.1000 -44006.5399
    ##     80 93472060.9597             nan     0.1000 -379672.5416
    ##    100 90300027.4079             nan     0.1000 -336673.5968
    ##    120 86305662.9473             nan     0.1000 -221043.1823
    ##    140 83480615.7104             nan     0.1000 -337898.5736
    ##    160 80494149.1847             nan     0.1000 -580234.7318
    ##    180 77459936.3330             nan     0.1000 -158846.8941
    ##    200 75189009.1973             nan     0.1000 -301293.3769
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 114137479.0507             nan     0.1000 -87812.0647
    ##      2 113917017.1599             nan     0.1000 -153410.1651
    ##      3 113834105.7988             nan     0.1000 -32728.1085
    ##      4 113656793.8229             nan     0.1000 -106888.2846
    ##      5 113558113.6269             nan     0.1000 -147632.4065
    ##      6 113501794.7674             nan     0.1000 -81986.2969
    ##      7 113423591.9499             nan     0.1000 -122058.2369
    ##      8 113346317.6631             nan     0.1000 -167531.5822
    ##      9 113261687.6200             nan     0.1000 64106.9195
    ##     10 113202790.9665             nan     0.1000 -26311.2393
    ##     20 112601905.8977             nan     0.1000 -20966.5633
    ##     40 111975171.2469             nan     0.1000 -104255.5589
    ##     60 111497681.1390             nan     0.1000 -74848.3387
    ##     80 111068245.1745             nan     0.1000 -13791.7430
    ##    100 110561607.8472             nan     0.1000 -76685.7171
    ##    120 110097282.3483             nan     0.1000 -75205.0991
    ##    140 109777639.2335             nan     0.1000 -125465.1951
    ##    160 109589667.1368             nan     0.1000 -25299.5865
    ##    180 109325304.6807             nan     0.1000 -63931.4602
    ##    200 109081931.5411             nan     0.1000 -155923.5393
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 114137549.6923             nan     0.1000 -129715.3618
    ##      2 113903686.9606             nan     0.1000 -102114.5195
    ##      3 113726363.8314             nan     0.1000 75381.9254
    ##      4 113280676.4632             nan     0.1000 -55817.2497
    ##      5 112775320.0884             nan     0.1000 -114696.4373
    ##      6 112571137.5159             nan     0.1000 13667.6131
    ##      7 112176955.0937             nan     0.1000 -16486.4111
    ##      8 111990217.4451             nan     0.1000 -106852.1100
    ##      9 111796447.8290             nan     0.1000 -186465.3763
    ##     10 111603218.7403             nan     0.1000 53743.8540
    ##     20 109555646.0968             nan     0.1000 -221303.8506
    ##     40 106521543.4061             nan     0.1000 -141016.8405
    ##     60 104181682.3015             nan     0.1000 -254731.6316
    ##     80 103016592.6737             nan     0.1000 -447728.4288
    ##    100 101363228.3975             nan     0.1000 -281634.1879
    ##    120 100289036.6853             nan     0.1000 -159526.5668
    ##    140 99576633.9834             nan     0.1000 -161588.6496
    ##    160 98246733.0684             nan     0.1000 -145978.7202
    ##    180 96897977.8428             nan     0.1000 -456525.7769
    ##    200 96137047.9249             nan     0.1000 -204280.6738
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 114033182.3486             nan     0.1000 164808.2178
    ##      2 113570411.6675             nan     0.1000 -105417.0904
    ##      3 113241155.9277             nan     0.1000 36050.6415
    ##      4 112738010.4148             nan     0.1000 284200.8783
    ##      5 112416131.5343             nan     0.1000 187831.2208
    ##      6 111918448.0054             nan     0.1000 -296369.2200
    ##      7 111359492.7326             nan     0.1000 94070.7177
    ##      8 110811459.7125             nan     0.1000 -29024.7528
    ##      9 110412756.4112             nan     0.1000 -237405.7552
    ##     10 110121193.4080             nan     0.1000 -46648.5504
    ##     20 106672958.3742             nan     0.1000 -164145.7295
    ##     40 103311942.5961             nan     0.1000 -430887.2750
    ##     60 99202175.2889             nan     0.1000 -287214.1028
    ##     80 95456514.2549             nan     0.1000 -179878.4984
    ##    100 93439392.4594             nan     0.1000 -25270.3987
    ##    120 90769447.5806             nan     0.1000 -293769.0471
    ##    140 88462624.8234             nan     0.1000 -107189.9608
    ##    160 86160651.5509             nan     0.1000 -40666.5213
    ##    180 84207666.9934             nan     0.1000 -136648.0238
    ##    200 81488010.5283             nan     0.1000 -221739.7580
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 114095074.3069             nan     0.1000 -202526.4815
    ##      2 113428988.6802             nan     0.1000 -44798.8966
    ##      3 111908807.1952             nan     0.1000 -219669.4084
    ##      4 111212079.5396             nan     0.1000 -84839.4605
    ##      5 110624421.7259             nan     0.1000 -16326.2128
    ##      6 110470188.4353             nan     0.1000 -96053.0190
    ##      7 109681739.8739             nan     0.1000 265224.1426
    ##      8 109574623.6927             nan     0.1000 -98317.5345
    ##      9 108734722.2475             nan     0.1000 -77129.0841
    ##     10 108297544.8523             nan     0.1000 -275608.5028
    ##     20 104150556.9008             nan     0.1000 -484631.6446
    ##     40 96625505.2818             nan     0.1000 -51773.1781
    ##     60 91762201.0016             nan     0.1000 -308225.7526
    ##     80 88392808.3208             nan     0.1000 -280936.0114
    ##    100 85715850.7107             nan     0.1000 -363581.9613
    ##    120 83902515.2078             nan     0.1000 -319103.1715
    ##    140 80402974.1135             nan     0.1000 -477312.8461
    ##    160 78503555.4556             nan     0.1000 -217848.2633
    ##    180 76135843.6882             nan     0.1000 -121129.2853
    ##    200 74161719.9350             nan     0.1000 -406680.0598
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 96748448.5229             nan     0.1000 17766.3324
    ##      2 96681917.3345             nan     0.1000 -22216.3413
    ##      3 96629037.4593             nan     0.1000 -57076.9085
    ##      4 96565923.1137             nan     0.1000 -27219.3844
    ##      5 96377249.1174             nan     0.1000 35826.1528
    ##      6 96231422.0227             nan     0.1000 23672.3109
    ##      7 96176289.5141             nan     0.1000 -122505.7832
    ##      8 96150196.7049             nan     0.1000 -19788.2338
    ##      9 96126975.8697             nan     0.1000 -38376.8898
    ##     10 95999879.2252             nan     0.1000 -54905.1003
    ##     20 95605854.1998             nan     0.1000 -77534.4496
    ##     40 95142888.0575             nan     0.1000 -80868.3663
    ##     60 94576748.7564             nan     0.1000 -24140.0565
    ##     80 94347102.1002             nan     0.1000 -23653.7353
    ##    100 93945886.7147             nan     0.1000 -97002.0280
    ##    120 93660906.1165             nan     0.1000 -122931.9189
    ##    140 93452013.2389             nan     0.1000 -71785.3490
    ##    160 93218866.6292             nan     0.1000 -95496.9808
    ##    180 92952419.5032             nan     0.1000 -108514.5137
    ##    200 92689132.8091             nan     0.1000 -144041.2482
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 96602767.9940             nan     0.1000 36933.0391
    ##      2 96427591.9117             nan     0.1000 40731.2812
    ##      3 96338171.6973             nan     0.1000 -40181.2041
    ##      4 95479590.1938             nan     0.1000 -152106.4836
    ##      5 95393670.1102             nan     0.1000 -41282.3006
    ##      6 95185250.0829             nan     0.1000 -130897.6777
    ##      7 94968268.1114             nan     0.1000 18994.9990
    ##      8 94962517.3349             nan     0.1000 -133257.3924
    ##      9 94720119.6587             nan     0.1000 60014.2808
    ##     10 94618482.9659             nan     0.1000 -58480.2258
    ##     20 92937008.7451             nan     0.1000 -110543.7997
    ##     40 89773839.2133             nan     0.1000 -47573.2878
    ##     60 87381124.9440             nan     0.1000 -195370.0872
    ##     80 86525078.8209             nan     0.1000 -240111.8128
    ##    100 84892416.4960             nan     0.1000 -149255.4936
    ##    120 84009169.5560             nan     0.1000 -155367.2544
    ##    140 83171458.3044             nan     0.1000 -43697.7490
    ##    160 81983482.3224             nan     0.1000 -135390.2521
    ##    180 81001340.5809             nan     0.1000 -353100.3173
    ##    200 80509632.0623             nan     0.1000 -69817.3832
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 96590069.0402             nan     0.1000 -123030.9154
    ##      2 95850914.1136             nan     0.1000 -19845.5615
    ##      3 95689620.8328             nan     0.1000 51685.3063
    ##      4 95547662.0390             nan     0.1000 -71846.7202
    ##      5 95258059.6328             nan     0.1000 -174036.5495
    ##      6 95051839.2262             nan     0.1000 -175830.8619
    ##      7 94556195.1481             nan     0.1000 -250687.7376
    ##      8 94212146.6219             nan     0.1000 -83015.7583
    ##      9 93581839.9088             nan     0.1000 -342646.6130
    ##     10 93104023.7008             nan     0.1000 -257662.3907
    ##     20 89256807.1376             nan     0.1000 -155099.5279
    ##     40 83840645.2685             nan     0.1000 -155097.4406
    ##     60 80490680.9711             nan     0.1000 -307805.2027
    ##     80 77164747.9881             nan     0.1000 -91473.8192
    ##    100 74854193.2704             nan     0.1000 -231895.6636
    ##    120 72696073.0260             nan     0.1000 -234161.1356
    ##    140 70673343.8867             nan     0.1000 -66598.6603
    ##    160 68880257.5976             nan     0.1000 -174042.5507
    ##    180 66781522.8919             nan     0.1000 -143739.4452
    ##    200 65630253.8129             nan     0.1000 -96976.6978
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 96704931.5022             nan     0.1000 -42479.7246
    ##      2 96561626.5295             nan     0.1000 -3751.1943
    ##      3 96471073.0052             nan     0.1000 -50308.3220
    ##      4 96430456.9495             nan     0.1000 -114760.1461
    ##      5 96305184.7665             nan     0.1000 49678.1665
    ##      6 95919567.1161             nan     0.1000 -57369.1297
    ##      7 94652654.2932             nan     0.1000 -45918.5290
    ##      8 94154337.7223             nan     0.1000 -377982.6046
    ##      9 93516246.2658             nan     0.1000 -268125.8745
    ##     10 92912990.4181             nan     0.1000 -230126.9283
    ##     20 87735689.5452             nan     0.1000 -242373.9749
    ##     40 80770769.4465             nan     0.1000 -227148.6964
    ##     60 76933212.8065             nan     0.1000 -310225.6844
    ##     80 73695526.0854             nan     0.1000 -547341.8540
    ##    100 70576533.3927             nan     0.1000 -332871.6687
    ##    120 67957257.8972             nan     0.1000 -318191.7748
    ##    140 65303174.3502             nan     0.1000 -475430.1780
    ##    160 62894905.5413             nan     0.1000 -96196.5376
    ##    180 60539679.4966             nan     0.1000 -193328.2142
    ##    200 58477002.3310             nan     0.1000 -229127.0447
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 86950005.9725             nan     0.1000 28332.9606
    ##      2 86790318.1224             nan     0.1000 134828.7644
    ##      3 86702499.7820             nan     0.1000 31162.4256
    ##      4 86480586.7163             nan     0.1000 46038.0891
    ##      5 86343990.9862             nan     0.1000 108206.8351
    ##      6 86198541.2824             nan     0.1000 57502.7605
    ##      7 86067605.7653             nan     0.1000 25738.5089
    ##      8 85898591.4797             nan     0.1000 -122601.8199
    ##      9 85805886.1364             nan     0.1000 34404.0043
    ##     10 85850209.3613             nan     0.1000 -191934.2504
    ##     20 85232745.9787             nan     0.1000 -268255.4459
    ##     40 84834862.3641             nan     0.1000 -81917.0859
    ##     60 84398393.6161             nan     0.1000 -87365.8423
    ##     80 84015457.1822             nan     0.1000 1854.0066
    ##    100 83816986.9275             nan     0.1000 -193164.6916
    ##    120 83427759.6888             nan     0.1000 -52264.3241
    ##    140 83112618.1186             nan     0.1000 -43880.0051
    ##    160 82993155.2520             nan     0.1000 -187068.0244
    ##    180 82758321.1984             nan     0.1000 -77419.3375
    ##    200 82460631.4805             nan     0.1000 -118887.3411
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 87175463.7872             nan     0.1000 -9331.9306
    ##      2 86911689.1290             nan     0.1000 -59366.5200
    ##      3 86788480.1394             nan     0.1000 2640.3006
    ##      4 86515850.3083             nan     0.1000 64979.5072
    ##      5 86444815.9213             nan     0.1000 -118751.8384
    ##      6 86250851.1907             nan     0.1000 87194.8696
    ##      7 86039404.9482             nan     0.1000 -120962.3325
    ##      8 85939445.5578             nan     0.1000 -41513.3155
    ##      9 85683357.6631             nan     0.1000 -95626.6844
    ##     10 85608607.2783             nan     0.1000 35109.4255
    ##     20 83945114.6447             nan     0.1000 -87999.3574
    ##     40 80888904.0879             nan     0.1000 -87108.4367
    ##     60 78696958.5097             nan     0.1000 -109135.5105
    ##     80 77481404.8473             nan     0.1000 -153119.2927
    ##    100 76788542.9426             nan     0.1000 -42513.1288
    ##    120 74891593.0509             nan     0.1000 -34472.2221
    ##    140 74102838.1638             nan     0.1000 -1231093.7135
    ##    160 72588033.5537             nan     0.1000 -107785.1781
    ##    180 71628177.9215             nan     0.1000 -76242.4710
    ##    200 70901601.9063             nan     0.1000 -89569.8406
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 87019591.9941             nan     0.1000 289478.3917
    ##      2 86249914.6127             nan     0.1000 -68895.9323
    ##      3 85784714.9048             nan     0.1000 -223147.3562
    ##      4 84872457.3916             nan     0.1000 380225.6290
    ##      5 84544685.8079             nan     0.1000 -147284.7096
    ##      6 84373871.1562             nan     0.1000 -47739.8541
    ##      7 84321055.8368             nan     0.1000 -58609.7800
    ##      8 83650125.3929             nan     0.1000 -385512.8543
    ##      9 83553658.1662             nan     0.1000 -221112.5033
    ##     10 83392373.1773             nan     0.1000 -235751.7147
    ##     20 80063339.4288             nan     0.1000 -472844.0318
    ##     40 75685234.3175             nan     0.1000 -126018.1723
    ##     60 73666810.1576             nan     0.1000 -460627.0640
    ##     80 70749329.0531             nan     0.1000 -299008.4473
    ##    100 68669258.0039             nan     0.1000 -385575.9028
    ##    120 66907161.1232             nan     0.1000 -484816.5284
    ##    140 65460613.0521             nan     0.1000 -242562.2625
    ##    160 63787119.9595             nan     0.1000 -233616.9935
    ##    180 61692627.3829             nan     0.1000 -168186.8258
    ##    200 59982237.0759             nan     0.1000 -21932.6117
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 86953877.7840             nan     0.1000 -26759.7035
    ##      2 86748217.5386             nan     0.1000 26642.0117
    ##      3 86453604.2443             nan     0.1000 -227933.1455
    ##      4 85694541.2871             nan     0.1000 30118.0357
    ##      5 85111030.7105             nan     0.1000 -222190.1418
    ##      6 84697629.0471             nan     0.1000 -256752.5263
    ##      7 84508777.6894             nan     0.1000 21248.2804
    ##      8 84133861.8976             nan     0.1000 -255382.4620
    ##      9 84081364.4307             nan     0.1000 -215607.5929
    ##     10 83441669.5493             nan     0.1000 -406317.4825
    ##     20 78359446.7638             nan     0.1000 -181414.0275
    ##     40 73332963.5486             nan     0.1000 -166428.5190
    ##     60 68841171.5675             nan     0.1000 -120400.8060
    ##     80 65097738.2179             nan     0.1000 -138823.0539
    ##    100 63134465.4627             nan     0.1000 -312942.7333
    ##    120 60495967.8070             nan     0.1000 -464211.6186
    ##    140 58601436.4006             nan     0.1000 -278908.1309
    ##    160 56340221.4408             nan     0.1000 -135589.2781
    ##    180 55010963.9180             nan     0.1000 -388201.4440
    ##    200 53745275.2359             nan     0.1000 -286050.6996
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 76960052.8587             nan     0.1000 -90808.7818
    ##      2 76911777.5720             nan     0.1000 -136782.0421
    ##      3 76870453.9722             nan     0.1000 -79341.0589
    ##      4 76845247.8218             nan     0.1000 -95700.0766
    ##      5 76768159.0811             nan     0.1000 6482.3072
    ##      6 76756801.8539             nan     0.1000 -129781.6357
    ##      7 76703247.9928             nan     0.1000 -90667.0727
    ##      8 76646679.0945             nan     0.1000  398.2980
    ##      9 76604139.1601             nan     0.1000 -18033.6107
    ##     10 76566077.7580             nan     0.1000 -53291.0811
    ##     20 76385191.7199             nan     0.1000 -59218.0593
    ##     40 75936951.4648             nan     0.1000 -107857.3347
    ##     60 75720958.6038             nan     0.1000 -39329.3564
    ##     80 75626199.2746             nan     0.1000 -71812.5214
    ##    100 75383660.9639             nan     0.1000 -40128.0922
    ##    120 75259584.3250             nan     0.1000 -72827.7315
    ##    140 75043959.1041             nan     0.1000 -143882.7117
    ##    160 74868655.8435             nan     0.1000 -69942.3985
    ##    180 74703636.5414             nan     0.1000 -98605.2181
    ##    200 74615923.8873             nan     0.1000 -66771.8059
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 76862678.0544             nan     0.1000 -116264.5183
    ##      2 76761256.3052             nan     0.1000 -74124.1687
    ##      3 76616648.2163             nan     0.1000 -81132.8472
    ##      4 76452218.1877             nan     0.1000 -96999.3179
    ##      5 76058090.2949             nan     0.1000 -207249.2963
    ##      6 75937545.5198             nan     0.1000 -49845.1076
    ##      7 75808283.9448             nan     0.1000 37199.2731
    ##      8 75690261.5115             nan     0.1000 -59434.4234
    ##      9 75390335.8859             nan     0.1000 -78469.7946
    ##     10 75300389.5496             nan     0.1000 7963.8462
    ##     20 74219626.4277             nan     0.1000 -118816.4773
    ##     40 72232350.3337             nan     0.1000 -50225.8938
    ##     60 70729645.3282             nan     0.1000 -95246.7917
    ##     80 69632563.1355             nan     0.1000 -169841.6194
    ##    100 68817689.9385             nan     0.1000 -60466.9456
    ##    120 67528150.0649             nan     0.1000 -120309.2976
    ##    140 66697051.5600             nan     0.1000 -164093.5706
    ##    160 65745840.3907             nan     0.1000 -153589.0613
    ##    180 64816115.5104             nan     0.1000 -188.4212
    ##    200 64178934.9620             nan     0.1000 -265861.4781
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 76852239.8437             nan     0.1000 -124198.0789
    ##      2 76735260.2984             nan     0.1000 -91398.8798
    ##      3 76664256.6191             nan     0.1000 -132981.7295
    ##      4 76564601.1883             nan     0.1000 78927.3941
    ##      5 76436334.6114             nan     0.1000 5963.5950
    ##      6 76394612.7902             nan     0.1000 -133907.3792
    ##      7 76181982.5969             nan     0.1000 -236965.1497
    ##      8 75989765.0423             nan     0.1000 -126002.0762
    ##      9 75910719.4865             nan     0.1000 -2632.9428
    ##     10 75479416.1823             nan     0.1000 -89838.9595
    ##     20 72436145.4233             nan     0.1000 -115515.2113
    ##     40 69722964.5318             nan     0.1000 -244515.8839
    ##     60 67231330.1963             nan     0.1000 -234820.9808
    ##     80 65414056.7373             nan     0.1000 -119705.0492
    ##    100 63767983.2440             nan     0.1000 -78826.4259
    ##    120 62141769.2320             nan     0.1000 -202181.4428
    ##    140 60614272.2473             nan     0.1000 -195640.3012
    ##    160 58714774.0207             nan     0.1000 -288658.3595
    ##    180 56899505.0926             nan     0.1000 -54139.4158
    ##    200 55488374.2109             nan     0.1000 -133326.2715
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 76773472.2953             nan     0.1000 -66138.6200
    ##      2 76235572.4174             nan     0.1000 -400623.1012
    ##      3 75910894.2219             nan     0.1000 -26385.5231
    ##      4 75643952.4880             nan     0.1000 -141316.9514
    ##      5 75546141.4190             nan     0.1000 -22574.6126
    ##      6 75069637.0994             nan     0.1000 -162378.6032
    ##      7 74576416.2443             nan     0.1000 -216603.0839
    ##      8 74354768.1906             nan     0.1000 -100113.8494
    ##      9 73974817.9853             nan     0.1000 -116794.8744
    ##     10 73914240.6379             nan     0.1000 -79579.7633
    ##     20 70823817.5587             nan     0.1000 -194426.1647
    ##     40 67030960.0804             nan     0.1000 -440331.4600
    ##     60 64292232.0123             nan     0.1000 -534193.0996
    ##     80 60851994.6925             nan     0.1000 -522070.8781
    ##    100 58362100.8150             nan     0.1000 -189212.6086
    ##    120 56650307.2360             nan     0.1000 -138988.6588
    ##    140 54587748.6730             nan     0.1000 -217897.6678
    ##    160 53120542.8639             nan     0.1000 -132149.8765
    ##    180 51330971.5418             nan     0.1000 -365904.3382
    ##    200 49891666.9198             nan     0.1000 -236416.1845
    ## 
    ## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
    ##      1 99002172.7867             nan     0.1000 46438.3959
    ##      2 98868337.0638             nan     0.1000 41774.8530
    ##      3 98787343.6553             nan     0.1000 8995.1178
    ##      4 98735915.2521             nan     0.1000 -3005.1248
    ##      5 98700720.6576             nan     0.1000 -72816.2535
    ##      6 98642983.3096             nan     0.1000 -52281.7301
    ##      7 98514110.5689             nan     0.1000 -109467.0969
    ##      8 98460675.6111             nan     0.1000 -21763.0461
    ##      9 98438359.5246             nan     0.1000 -61290.4591
    ##     10 98392218.0919             nan     0.1000    6.1367
    ##     20 97902674.4774             nan     0.1000 -53536.1908
    ##     25 97758521.4159             nan     0.1000 -12157.2747

# Comparison

``` r
#Model 1
set.seed(1)
# Train the model
fit1 <- train(shares ~., data = train, method = "lm")
```

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

``` r
print(fit1)
```

    ## Linear Regression 
    ## 
    ## 1469 samples
    ##   52 predictor
    ## 
    ## No pre-processing
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 1469, 1469, 1469, 1469, 1469, 1469, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared     MAE     
    ##   10393.56  0.007722759  4069.307
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

``` r
#See how well it performs on the test set
#Predict fit 1-Main Effect
pred.fit1 <- predict(fit1, newdata = test)
```

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may
    ## be misleading

``` r
#calculating RSME
error <- test$shares-pred.fit1
RSME<- sqrt(mean(error^2))
RSME
```

    ## [1] 5941.299

``` r
#calculating MAE
MAE<- mean((test$shares-pred.fit1)^2)
MAE
```

    ## [1] 35299030

``` r
#Comparison for Model 2
set.seed(1)
# Train the model
fit2 <- train(shares ~n_tokens_content + n_non_stop_unique_tokens + num_videos + kw_avg_avg, data = train, method = "lm")
print(fit2)
```

    ## Linear Regression 
    ## 
    ## 1469 samples
    ##    4 predictor
    ## 
    ## No pre-processing
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 1469, 1469, 1469, 1469, 1469, 1469, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared     MAE     
    ##   10107.78  0.009601575  3600.424
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

``` r
#See how well it performs on the test set
#Predict fit 1-Main Effect
pred.fit2 <- predict(fit2, newdata = test)
#calculating RSME
error <- test$shares-pred.fit2
RSME<- sqrt(mean(error^2))
RSME
```

    ## [1] 5743.126

``` r
#calculating MAE
MAE<- mean((test$shares-pred.fit2)^2)
MAE
```

    ## [1] 32983494

``` r
#Comparison for Random Forest Model
#See how well it performs on the test set
#Predict fit 1-Main Effect
pred.rf<- predict(rfmodel, newdata = test)
#calculating RSME
error <- test$shares-pred.rf
RSME<- sqrt(mean(error^2))
RSME
```

    ## [1] 5905.796

``` r
#calculating MAE
MAE<- mean((test$shares-pred.rf)^2)
MAE
```

    ## [1] 34878432

``` r
#Comparison for Boosted Model
boostPred <- predict(boostFit, newdata = test)
boostRMSE <- sqrt(mean((boostPred-test$shares)^2))
boostRMSE
```

    ## [1] 5622.948

It appears that the boosted model is the declared winner. This is
because on the test set the Boosted Model has the lowest RSME of
5627.462.
