coyote\_glm
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.4
    ## v tibble  1.4.2     v dplyr   0.7.4
    ## v tidyr   0.8.0     v stringr 1.2.0
    ## v readr   1.1.1     v forcats 0.2.0

    ## -- Conflicts -------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
sept15 <- read.csv("15sept.csv", TRUE)
oct16 <- read.csv("16oct.csv", TRUE)
oct17 <- read.csv("17oct.csv", TRUE)
yearswet <- read.csv("3yearwet.csv", TRUE)
head(sept15)
```

    ##   OBJECTID_1..  Shape.. Condition springpres  COUNT       SUM boul_area
    ## 1            1 Polyline       Dry          1  61489 167557525  167.5575
    ## 2            2 Polyline       Dry          1 197678 538672550  538.6725
    ## 3            3 Polyline       Dry          0  38209 104119525  104.1195
    ## 4            4 Polyline       Dry          0   6990  19047750  19.04775
    ## 5            5 Polyline       Dry          0   5080  13843000    13.843
    ## 6            6 Polyline       Dry          1  53494 145771150  145.7711
    ##    length_m area_leng boul_pres Shape_Length
    ## 1  703.8706  0.238052         1     0.007821
    ## 2 1679.7430  0.320687         1     0.018664
    ## 3  218.3987  0.476741         1     0.002427
    ## 4  240.6930  0.079137         1     0.002674
    ## 5  360.3567  0.038415         1     0.004004
    ## 6  791.7947  0.184102         1     0.008798

``` r
sept15$area_leng <- as.numeric(as.character(sept15$area_leng))
```

    ## Warning: NAs introduced by coercion

``` r
oct16$area_leng <- as.numeric(as.character(oct16$area_leng))
```

    ## Warning: NAs introduced by coercion

``` r
oct17$area_leng <- as.numeric(as.character(oct17$area_leng))
```

    ## Warning: NAs introduced by coercion

``` r
sept15$cond <- ifelse(sept15$Condition=="Wet", 1, 0)
oct16$cond <- ifelse(oct16$condition=="Wet", 1, 0)
oct17$cond <- ifelse(oct17$condition=="Wet", 1, 0)
```

``` r
sept15glm <- glm(cond ~ springpres + area_leng, data = sept15, family = binomial())
summary(sept15glm)
```

    ## 
    ## Call:
    ## glm(formula = cond ~ springpres + area_leng, family = binomial(), 
    ##     data = sept15)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5292  -0.6908  -0.2020   0.4291   1.7591  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)   -1.323      1.069  -1.238    0.216
    ## springpres    -1.955      1.920  -1.019    0.308
    ## area_leng      4.447      2.922   1.522    0.128
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 19.408  on 13  degrees of freedom
    ## Residual deviance: 11.244  on 11  degrees of freedom
    ##   (32 observations deleted due to missingness)
    ## AIC: 17.244
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
oct16glm <- glm(cond ~ springpres + area_leng, data = oct16, family = binomial())
summary(oct16glm)
```

    ## 
    ## Call:
    ## glm(formula = cond ~ springpres + area_leng, family = binomial(), 
    ##     data = oct16)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5590  -0.9574  -0.8742   1.1623   1.5087  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -0.8047     0.8433  -0.954    0.340
    ## springpres    0.6471     1.2317   0.525    0.599
    ## area_leng     0.2901     0.3668   0.791    0.429
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 21.930  on 15  degrees of freedom
    ## Residual deviance: 21.176  on 13  degrees of freedom
    ##   (20 observations deleted due to missingness)
    ## AIC: 27.176
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
oct17glm <- glm(cond ~ springpres + area_leng, data = oct17, family = binomial())
summary(oct17glm)
```

    ## 
    ## Call:
    ## glm(formula = cond ~ springpres + area_leng, family = binomial(), 
    ##     data = oct17)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.65967  -1.07446   0.00014   1.12061   1.29003  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)   -0.2797     0.5973  -0.468    0.640
    ## springpres    18.6672  3240.6247   0.006    0.995
    ## area_leng      0.1838     0.2459   0.747    0.455
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 29.767  on 21  degrees of freedom
    ## Residual deviance: 24.349  on 19  degrees of freedom
    ##   (24 observations deleted due to missingness)
    ## AIC: 30.349
    ## 
    ## Number of Fisher Scoring iterations: 17

``` r
oct17thresh <- oct17 %>% filter(area_leng > 0.5)
oct17test <- glm(cond ~ springpres + area_leng, data = oct17thresh, family = binomial())
summary(oct17test)
```

    ## 
    ## Call:
    ## glm(formula = cond ~ springpres + area_leng, family = binomial(), 
    ##     data = oct17thresh)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.05536   0.00013   0.43618   0.60976   1.00121  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)    2.5573     1.6512   1.549    0.121
    ## springpres    16.7211  4546.5506   0.004    0.997
    ## area_leng     -0.3849     0.3693  -1.042    0.297
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10.4311  on 10  degrees of freedom
    ## Residual deviance:  8.3447  on  8  degrees of freedom
    ## AIC: 14.345
    ## 
    ## Number of Fisher Scoring iterations: 17

``` r
oct16thresh <- oct16 %>% filter(area_leng > 0.5)
oct16test <- glm(cond ~ springpres + area_leng, data = oct16thresh, family = binomial())
summary(oct16test)
```

    ## 
    ## Call:
    ## glm(formula = cond ~ springpres + area_leng, family = binomial(), 
    ##     data = oct16thresh)
    ## 
    ## Deviance Residuals: 
    ##        1         2         3         4         5         6         7  
    ##  0.00013   1.04616  -0.63487   0.45948   0.00013  -1.82310   0.78246  
    ##        8  
    ##  0.68183  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)    3.1677     2.5268   1.254    0.210
    ## springpres    16.0376  4611.6721   0.003    0.997
    ## area_leng     -0.8118     0.7623  -1.065    0.287
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 8.9974  on 7  degrees of freedom
    ## Residual deviance: 6.1095  on 5  degrees of freedom
    ## AIC: 12.109
    ## 
    ## Number of Fisher Scoring iterations: 17
