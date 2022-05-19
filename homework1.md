Homework1
================

#### 1. What factors do you think one should incorporate in the demand function? Please, discuss.

Considering porter’s 5 forces model, I believe factors to include in the
demand function include:

-   price of product - the higher the price the less the willingness to
    buy the less the demand,
-   consumer’s preferences - loyalty to goods increases spending and
    demand,
-   consumer’s income - an increase in disposable income enabling
    consumers to be able to afford more goods,
-   availability of substitutes - an increase in the price of a product
    will increase the demand for its major substitutes,
-   complements - a fall in price of complements will increase demand,
-   market size / consumer base - economy of scale can bring down costs
    and increase demand,
-   seasonality - weather could drive seasonal demand, and trends -
    economic cycle and more.

### Estimation - Single Item Model

``` r
# code from 'Studentspart1.R'
## Import data from CSV file, inside "" is the location of your CSV file in your computer. 
Data <- read.csv("singleitemSKU88.csv")

##Add seasonal factor to the data 
Data$Week_factor <-  as.factor(rep(rep(seq(1,13),each=4),3))

library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.0.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
Data %>% head()
```

    ##   Week Price Price_1 Price_2       St Week_factor
    ## 1    1     1       1       1 213.5493           1
    ## 2    2     1       1       1 213.9310           1
    ## 3    3     1       1       1 223.9246           1
    ## 4    4     1       1       1 195.4201           1
    ## 5    5     1       1       1 312.5411           2
    ## 6    6     1       1       1 233.0407           2

#### 2. Estimate the demand function for SKU88 using R

Train test split

``` r
# set first 2 years of data as training set
train <- Data %>% filter(Week<=104)

# set the last year of data as testing set
test <- Data %>% filter(Week>104)

# check the number of rows in each set
nrow(train)
```

    ## [1] 104

``` r
nrow(test)
```

    ## [1] 52

(i)the summary of the regressions

``` r
# run model (1)
lm1 <- lm(log(St) ~ Week_factor + Week+ log(Price), data = train)
# generate the summary of model (1)
summary(lm1)
```

    ## 
    ## Call:
    ## lm(formula = log(St) ~ Week_factor + Week + log(Price), data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.86995 -0.17959  0.04269  0.24223  0.78927 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    5.203550   0.128925  40.361   <2e-16 ***
    ## Week_factor2   0.233520   0.174404   1.339   0.1840    
    ## Week_factor3  -0.178967   0.177538  -1.008   0.3162    
    ## Week_factor4  -0.082988   0.176317  -0.471   0.6390    
    ## Week_factor5  -0.094127   0.179483  -0.524   0.6013    
    ## Week_factor6   0.230207   0.176476   1.304   0.1954    
    ## Week_factor7  -0.241677   0.181199  -1.334   0.1857    
    ## Week_factor8  -0.083542   0.182336  -0.458   0.6479    
    ## Week_factor9  -0.181463   0.181117  -1.002   0.3191    
    ## Week_factor10  0.296648   0.181280   1.636   0.1053    
    ## Week_factor11  0.256366   0.184754   1.388   0.1687    
    ## Week_factor12 -0.221539   0.185099  -1.197   0.2345    
    ## Week_factor13  0.029748   0.186255   0.160   0.8735    
    ## Week           0.002461   0.001326   1.856   0.0667 .  
    ## log(Price)    -3.699180   0.342353 -10.805   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3486 on 89 degrees of freedom
    ## Multiple R-squared:  0.6114, Adjusted R-squared:  0.5503 
    ## F-statistic:    10 on 14 and 89 DF,  p-value: 5.204e-13

``` r
# model (2)
lm2 <- lm(log(St) ~ Week_factor + Week+ log(Price) + log(Price_1) + log(Price_2), data = train)
# generate the summary of model (2)
summary(lm2)
```

    ## 
    ## Call:
    ## lm(formula = log(St) ~ Week_factor + Week + log(Price) + log(Price_1) + 
    ##     log(Price_2), data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.24147 -0.07295 -0.00887  0.07000  0.34604 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    5.3964535  0.0464434 116.194  < 2e-16 ***
    ## Week_factor2   0.1072018  0.0621134   1.726  0.08791 .  
    ## Week_factor3   0.2023309  0.0649496   3.115  0.00249 ** 
    ## Week_factor4   0.0666978  0.0628632   1.061  0.29163    
    ## Week_factor5   0.2041267  0.0650093   3.140  0.00231 ** 
    ## Week_factor6   0.4254402  0.0631454   6.737 1.67e-09 ***
    ## Week_factor7   0.0284168  0.0652580   0.435  0.66431    
    ## Week_factor8   0.4428343  0.0680922   6.503 4.78e-09 ***
    ## Week_factor9   0.2210226  0.0667118   3.313  0.00135 ** 
    ## Week_factor10  0.3834187  0.0644279   5.951 5.46e-08 ***
    ## Week_factor11  0.3225230  0.0656173   4.915 4.14e-06 ***
    ## Week_factor12  0.2623244  0.0686746   3.820  0.00025 ***
    ## Week_factor13  0.0152876  0.0660999   0.231  0.81764    
    ## Week           0.0004117  0.0004779   0.862  0.39127    
    ## log(Price)    -2.9208085  0.1255607 -23.262  < 2e-16 ***
    ## log(Price_1)   2.9282787  0.1274345  22.979  < 2e-16 ***
    ## log(Price_2)   1.8941977  0.1267821  14.941  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1237 on 87 degrees of freedom
    ## Multiple R-squared:  0.9522, Adjusted R-squared:  0.9434 
    ## F-statistic: 108.2 on 16 and 87 DF,  p-value: < 2.2e-16

(ii)the value of R-squared and MAPE

``` r
# extract R-squared parameter from summary ###to update for testing set but not training set
summary(lm1)$r.squared #0.6114079
```

    ## [1] 0.6114079

``` r
summary(lm2)$r.squared #0.9521644
```

    ## [1] 0.9521644

``` r
 # make predictions on test data
lm1_predict=predict(lm1,test)
lm2_predict=predict(lm2,test)

# compute MAPE
library(MLmetrics)
```

    ## 
    ## Attaching package: 'MLmetrics'

    ## The following object is masked from 'package:base':
    ## 
    ##     Recall

``` r
MAPE(lm1_predict, log(test$St)) #0.1071977
```

    ## [1] 0.1071977

``` r
MAPE(lm2_predict, log(test$St)) #0.02063047
```

    ## [1] 0.02063047

#### 3. For the model in equation (2), what can you say on the magnitudes of the estimated parameters

From lm2, we can obtain b0=-2.92, b1=2.92, b2=1.89. This shows the price
is negatively correlated to the demand, while the effective of last
price and second last price on current demand have a positive effect.
The higher the current price the lower the demand, but the more the
price reduced the higher the demand. Also, recency matters as the effect
of the last price is stronger than the second last price.

#### 4. Compute the values of the predicted demand d7 using models (1) and (2) above.

Assume p7=1, p6=0.8 and p5=0.8, we can predict the demand of week 7 (d7)
as follows

``` r
# set assumption parameters
p7=1
p6=0.8
p5=0.8

# prediction using model (1)
a0=coef(lm1)[1]
a2=coef(lm1)[2]
trend=coef(lm1)[14]
b0=coef(lm1)[15]

a0=coef(lm1)[1]
a2=coef(lm1)[2]
trend=coef(lm1)[14]
b0=coef(lm1)[15]

exp(a0+a2+trend*7+b0*log(p7)) #233.7596 
```

    ## (Intercept) 
    ##    233.7596

``` r
# prediction using model (2)
a0=coef(lm2)[1]
a2=coef(lm2)[2]
trend=coef(lm2)[14] 
b0=coef(lm2)[15]
b1=coef(lm2)[16]
b2=coef(lm2)[17]

exp(a0+a2+trend*7+b0*log(p7)+b1*log(p6)+b2*log(p5)) #83.96775 
```

    ## (Intercept) 
    ##    83.96775

#### 5. Discuss the differences between the two models (1) and (2) above. For which type of products do you think that model (2) is better? Please discuss.

The demand predicted using model 1 (253.14) is higher than model 2
(84.30). Model 2 has considered the effect of last two prices on current
demand and predicts a lower demand that consumers are less willing to
buy after price increase, hence might be more applicable for
non-essential products with volatile pricing.

### Estimation - Multiple Items

``` r
# code from 'Studentspart2.R'
## Read data from CSV file
DATA <- read.csv("multipleitempart1.csv")

## Adding week factor indicator 
DATA$Week_factor <- as.factor(rep(rep(seq(1,13),each=4),3))
```

#### 6. Estimate the demand function for the 5 different brands using R

Train test split

``` r
# set first 2 years of data as training set
train <- DATA %>% filter(Week<=104)

# set the last year of data as testing set
test <- DATA %>% filter(Week>104)

# check the number of rows in each set
nrow(train)
```

    ## [1] 104

``` r
nrow(test)
```

    ## [1] 52

(i)the summary of the regressions

``` r
# run models
for(i in 1:5){
  # run model (1)
  assign(paste0("lm",i),lm(log(get(paste0("S",i,"_t"))) ~ Week_factor + Week 
                           + eval(parse(text=paste0("log(PriceB",i,")")))
                           + eval(parse(text=paste0("log(PriceB",i,"_1)")))
                           + eval(parse(text=paste0("log(PriceB",i,"_2)")))
                           + eval(parse(text=paste0("PriceB",ifelse(i+1>5,i+1-5,i+1))))
                           + eval(parse(text=paste0("PriceB",ifelse(i+2>5,i+2-5,i+2))))
                           + eval(parse(text=paste0("PriceB",ifelse(i+3>5,i+3-5,i+3))))
                           + eval(parse(text=paste0("PriceB",ifelse(i+4>5,i+4-5,i+4))))
                           , data = train))
  # generate the summary of each model
  print(paste0("===============brand",i,"================"))
  print(summary(get(paste0("lm",i))))
}
```

    ## [1] "===============brand1================"
    ## 
    ## Call:
    ## lm(formula = log(get(paste0("S", i, "_t"))) ~ Week_factor + Week + 
    ##     eval(parse(text = paste0("log(PriceB", i, ")"))) + eval(parse(text = paste0("log(PriceB", 
    ##     i, "_1)"))) + eval(parse(text = paste0("log(PriceB", i, "_2)"))) + 
    ##     eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 
    ##         1 - 5, i + 1)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 4 > 5, i + 4 - 5, i + 4)))), data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.33214 -0.09788 -0.00198  0.08456  0.38003 
    ## 
    ## Coefficients:
    ##                                                                             Estimate
    ## (Intercept)                                                                2.0814287
    ## Week_factor2                                                              -0.1274134
    ## Week_factor3                                                              -0.2182050
    ## Week_factor4                                                              -0.0317570
    ## Week_factor5                                                               0.1463979
    ## Week_factor6                                                              -0.1213347
    ## Week_factor7                                                              -0.1704535
    ## Week_factor8                                                              -0.0508460
    ## Week_factor9                                                               0.0527367
    ## Week_factor10                                                             -0.0991975
    ## Week_factor11                                                              0.0521012
    ## Week_factor12                                                              0.1601961
    ## Week_factor13                                                             -0.1792815
    ## Week                                                                       0.0093811
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -3.4520266
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         2.9069109
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         1.9807048
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.6164920
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.3326457
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.6802756
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.7397395
    ##                                                                           Std. Error
    ## (Intercept)                                                                0.3061679
    ## Week_factor2                                                               0.0816072
    ## Week_factor3                                                               0.0855641
    ## Week_factor4                                                               0.0809588
    ## Week_factor5                                                               0.0852548
    ## Week_factor6                                                               0.0831729
    ## Week_factor7                                                               0.0878248
    ## Week_factor8                                                               0.0870988
    ## Week_factor9                                                               0.0865444
    ## Week_factor10                                                              0.0824118
    ## Week_factor11                                                              0.0863088
    ## Week_factor12                                                              0.0908849
    ## Week_factor13                                                              0.0888008
    ## Week                                                                       0.0006369
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           0.1647543
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         0.1633207
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         0.1620502
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.1682456
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.1591370
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.1608463
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.1634971
    ##                                                                           t value
    ## (Intercept)                                                                 6.798
    ## Week_factor2                                                               -1.561
    ## Week_factor3                                                               -2.550
    ## Week_factor4                                                               -0.392
    ## Week_factor5                                                                1.717
    ## Week_factor6                                                               -1.459
    ## Week_factor7                                                               -1.941
    ## Week_factor8                                                               -0.584
    ## Week_factor9                                                                0.609
    ## Week_factor10                                                              -1.204
    ## Week_factor11                                                               0.604
    ## Week_factor12                                                               1.763
    ## Week_factor13                                                              -2.019
    ## Week                                                                       14.730
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -20.953
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         17.799
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         12.223
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))   3.664
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))   2.090
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))   4.229
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))   4.524
    ##                                                                           Pr(>|t|)
    ## (Intercept)                                                               1.51e-09
    ## Week_factor2                                                              0.122258
    ## Week_factor3                                                              0.012604
    ## Week_factor4                                                              0.695871
    ## Week_factor5                                                              0.089676
    ## Week_factor6                                                              0.148387
    ## Week_factor7                                                              0.055673
    ## Week_factor8                                                              0.560956
    ## Week_factor9                                                              0.543950
    ## Week_factor10                                                             0.232135
    ## Week_factor11                                                             0.547715
    ## Week_factor12                                                             0.081645
    ## Week_factor13                                                             0.046724
    ## Week                                                                       < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         < 2e-16
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) 0.000436
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) 0.039650
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) 6.00e-05
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) 2.00e-05
    ##                                                                              
    ## (Intercept)                                                               ***
    ## Week_factor2                                                                 
    ## Week_factor3                                                              *  
    ## Week_factor4                                                                 
    ## Week_factor5                                                              .  
    ## Week_factor6                                                                 
    ## Week_factor7                                                              .  
    ## Week_factor8                                                                 
    ## Week_factor9                                                                 
    ## Week_factor10                                                                
    ## Week_factor11                                                                
    ## Week_factor12                                                             .  
    ## Week_factor13                                                             *  
    ## Week                                                                      ***
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          ***
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                        ***
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                        ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) *  
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1574 on 83 degrees of freedom
    ## Multiple R-squared:  0.9541, Adjusted R-squared:  0.943 
    ## F-statistic: 86.21 on 20 and 83 DF,  p-value: < 2.2e-16
    ## 
    ## [1] "===============brand2================"
    ## 
    ## Call:
    ## lm(formula = log(get(paste0("S", i, "_t"))) ~ Week_factor + Week + 
    ##     eval(parse(text = paste0("log(PriceB", i, ")"))) + eval(parse(text = paste0("log(PriceB", 
    ##     i, "_1)"))) + eval(parse(text = paste0("log(PriceB", i, "_2)"))) + 
    ##     eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 
    ##         1 - 5, i + 1)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 4 > 5, i + 4 - 5, i + 4)))), data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.29483 -0.08152  0.00599  0.08733  0.36171 
    ## 
    ## Coefficients:
    ##                                                                             Estimate
    ## (Intercept)                                                                1.7182446
    ## Week_factor2                                                              -0.1210733
    ## Week_factor3                                                              -0.0419936
    ## Week_factor4                                                               0.1811002
    ## Week_factor5                                                               0.2688616
    ## Week_factor6                                                              -0.1019152
    ## Week_factor7                                                               0.0061886
    ## Week_factor8                                                               0.0848625
    ## Week_factor9                                                               0.1868356
    ## Week_factor10                                                              0.0114718
    ## Week_factor11                                                              0.2609754
    ## Week_factor12                                                              0.3072144
    ## Week_factor13                                                              0.0071760
    ## Week                                                                       0.0088406
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -2.9914280
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         2.8597868
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         1.9645621
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.8471416
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.5165865
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.5199890
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.6849236
    ##                                                                           Std. Error
    ## (Intercept)                                                                0.3021487
    ## Week_factor2                                                               0.0731426
    ## Week_factor3                                                               0.0765843
    ## Week_factor4                                                               0.0721456
    ## Week_factor5                                                               0.0768285
    ## Week_factor6                                                               0.0740347
    ## Week_factor7                                                               0.0777228
    ## Week_factor8                                                               0.0742392
    ## Week_factor9                                                               0.0747500
    ## Week_factor10                                                              0.0737758
    ## Week_factor11                                                              0.0772735
    ## Week_factor12                                                              0.0778984
    ## Week_factor13                                                              0.0793340
    ## Week                                                                       0.0005793
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           0.1293094
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         0.1282987
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         0.1284611
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.1411743
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.1444519
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.1508947
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.1668497
    ##                                                                           t value
    ## (Intercept)                                                                 5.687
    ## Week_factor2                                                               -1.655
    ## Week_factor3                                                               -0.548
    ## Week_factor4                                                                2.510
    ## Week_factor5                                                                3.500
    ## Week_factor6                                                               -1.377
    ## Week_factor7                                                                0.080
    ## Week_factor8                                                                1.143
    ## Week_factor9                                                                2.499
    ## Week_factor10                                                               0.155
    ## Week_factor11                                                               3.377
    ## Week_factor12                                                               3.944
    ## Week_factor13                                                               0.090
    ## Week                                                                       15.262
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -23.134
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         22.290
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         15.293
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))   6.001
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))   3.576
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))   3.446
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))   4.105
    ##                                                                           Pr(>|t|)
    ## (Intercept)                                                               1.88e-07
    ## Week_factor2                                                              0.101639
    ## Week_factor3                                                              0.584936
    ## Week_factor4                                                              0.014008
    ## Week_factor5                                                              0.000752
    ## Week_factor6                                                              0.172342
    ## Week_factor7                                                              0.936728
    ## Week_factor8                                                              0.256285
    ## Week_factor9                                                              0.014408
    ## Week_factor10                                                             0.876808
    ## Week_factor11                                                             0.001116
    ## Week_factor12                                                             0.000167
    ## Week_factor13                                                             0.928145
    ## Week                                                                       < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         < 2e-16
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) 4.95e-08
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) 0.000585
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) 0.000895
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) 9.41e-05
    ##                                                                              
    ## (Intercept)                                                               ***
    ## Week_factor2                                                                 
    ## Week_factor3                                                                 
    ## Week_factor4                                                              *  
    ## Week_factor5                                                              ***
    ## Week_factor6                                                                 
    ## Week_factor7                                                                 
    ## Week_factor8                                                                 
    ## Week_factor9                                                              *  
    ## Week_factor10                                                                
    ## Week_factor11                                                             ** 
    ## Week_factor12                                                             ***
    ## Week_factor13                                                                
    ## Week                                                                      ***
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          ***
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                        ***
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                        ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1403 on 83 degrees of freedom
    ## Multiple R-squared:  0.9627, Adjusted R-squared:  0.9537 
    ## F-statistic: 107.2 on 20 and 83 DF,  p-value: < 2.2e-16
    ## 
    ## [1] "===============brand3================"
    ## 
    ## Call:
    ## lm(formula = log(get(paste0("S", i, "_t"))) ~ Week_factor + Week + 
    ##     eval(parse(text = paste0("log(PriceB", i, ")"))) + eval(parse(text = paste0("log(PriceB", 
    ##     i, "_1)"))) + eval(parse(text = paste0("log(PriceB", i, "_2)"))) + 
    ##     eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 
    ##         1 - 5, i + 1)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 4 > 5, i + 4 - 5, i + 4)))), data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.28830 -0.08264 -0.02397  0.08293  0.29548 
    ## 
    ## Coefficients:
    ##                                                                             Estimate
    ## (Intercept)                                                                1.4144826
    ## Week_factor2                                                              -0.2490313
    ## Week_factor3                                                              -0.1980671
    ## Week_factor4                                                              -0.0804497
    ## Week_factor5                                                               0.0782888
    ## Week_factor6                                                              -0.2119068
    ## Week_factor7                                                              -0.1807119
    ## Week_factor8                                                              -0.0208710
    ## Week_factor9                                                               0.0213084
    ## Week_factor10                                                             -0.2062676
    ## Week_factor11                                                              0.0790397
    ## Week_factor12                                                              0.0448899
    ## Week_factor13                                                             -0.1023901
    ## Week                                                                       0.0097983
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -3.5366542
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         2.7508048
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         1.8809524
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.7482473
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.6463781
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.8605867
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.3682506
    ##                                                                           Std. Error
    ## (Intercept)                                                                0.3060202
    ## Week_factor2                                                               0.0731745
    ## Week_factor3                                                               0.0756750
    ## Week_factor4                                                               0.0729428
    ## Week_factor5                                                               0.0759463
    ## Week_factor6                                                               0.0760203
    ## Week_factor7                                                               0.0781590
    ## Week_factor8                                                               0.0747258
    ## Week_factor9                                                               0.0763295
    ## Week_factor10                                                              0.0743656
    ## Week_factor11                                                              0.0777419
    ## Week_factor12                                                              0.0785727
    ## Week_factor13                                                              0.0848264
    ## Week                                                                       0.0005666
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           0.1263049
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         0.1274423
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         0.1341539
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.1454177
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.1463555
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.1677866
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.1617265
    ##                                                                           t value
    ## (Intercept)                                                                 4.622
    ## Week_factor2                                                               -3.403
    ## Week_factor3                                                               -2.617
    ## Week_factor4                                                               -1.103
    ## Week_factor5                                                                1.031
    ## Week_factor6                                                               -2.788
    ## Week_factor7                                                               -2.312
    ## Week_factor8                                                               -0.279
    ## Week_factor9                                                                0.279
    ## Week_factor10                                                              -2.774
    ## Week_factor11                                                               1.017
    ## Week_factor12                                                               0.571
    ## Week_factor13                                                              -1.207
    ## Week                                                                       17.293
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -28.001
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         21.585
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         14.021
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))   5.146
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))   4.416
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))   5.129
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))   2.277
    ##                                                                           Pr(>|t|)
    ## (Intercept)                                                               1.38e-05
    ## Week_factor2                                                               0.00103
    ## Week_factor3                                                               0.01053
    ## Week_factor4                                                               0.27325
    ## Week_factor5                                                               0.30561
    ## Week_factor6                                                               0.00658
    ## Week_factor7                                                               0.02325
    ## Week_factor8                                                               0.78071
    ## Week_factor9                                                               0.78081
    ## Week_factor10                                                              0.00684
    ## Week_factor11                                                              0.31225
    ## Week_factor12                                                              0.56933
    ## Week_factor13                                                              0.23084
    ## Week                                                                       < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         < 2e-16
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) 1.76e-06
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) 3.00e-05
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) 1.88e-06
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.02536
    ##                                                                              
    ## (Intercept)                                                               ***
    ## Week_factor2                                                              ** 
    ## Week_factor3                                                              *  
    ## Week_factor4                                                                 
    ## Week_factor5                                                                 
    ## Week_factor6                                                              ** 
    ## Week_factor7                                                              *  
    ## Week_factor8                                                                 
    ## Week_factor9                                                                 
    ## Week_factor10                                                             ** 
    ## Week_factor11                                                                
    ## Week_factor12                                                                
    ## Week_factor13                                                                
    ## Week                                                                      ***
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          ***
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                        ***
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                        ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1412 on 83 degrees of freedom
    ## Multiple R-squared:  0.9704, Adjusted R-squared:  0.9632 
    ## F-statistic: 135.9 on 20 and 83 DF,  p-value: < 2.2e-16
    ## 
    ## [1] "===============brand4================"
    ## 
    ## Call:
    ## lm(formula = log(get(paste0("S", i, "_t"))) ~ Week_factor + Week + 
    ##     eval(parse(text = paste0("log(PriceB", i, ")"))) + eval(parse(text = paste0("log(PriceB", 
    ##     i, "_1)"))) + eval(parse(text = paste0("log(PriceB", i, "_2)"))) + 
    ##     eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 
    ##         1 - 5, i + 1)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 4 > 5, i + 4 - 5, i + 4)))), data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.43535 -0.08716 -0.01669  0.11022  0.32042 
    ## 
    ## Coefficients:
    ##                                                                             Estimate
    ## (Intercept)                                                                1.5926016
    ## Week_factor2                                                              -0.1611475
    ## Week_factor3                                                              -0.2109336
    ## Week_factor4                                                               0.0989846
    ## Week_factor5                                                               0.1290294
    ## Week_factor6                                                              -0.2547175
    ## Week_factor7                                                              -0.1421864
    ## Week_factor8                                                              -0.0895601
    ## Week_factor9                                                               0.1891192
    ## Week_factor10                                                             -0.1071204
    ## Week_factor11                                                              0.1013956
    ## Week_factor12                                                              0.1943972
    ## Week_factor13                                                             -0.2551186
    ## Week                                                                       0.0100704
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -3.8357628
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         2.8562034
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         2.2162612
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.9844361
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.5745622
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.8134384
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.7256553
    ##                                                                           Std. Error
    ## (Intercept)                                                                0.3547703
    ## Week_factor2                                                               0.0878817
    ## Week_factor3                                                               0.0918093
    ## Week_factor4                                                               0.0877593
    ## Week_factor5                                                               0.0907231
    ## Week_factor6                                                               0.0907321
    ## Week_factor7                                                               0.0971368
    ## Week_factor8                                                               0.0898433
    ## Week_factor9                                                               0.0905497
    ## Week_factor10                                                              0.0887886
    ## Week_factor11                                                              0.0946551
    ## Week_factor12                                                              0.0952976
    ## Week_factor13                                                              0.0983162
    ## Week                                                                       0.0007062
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           0.1504291
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         0.1426702
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         0.1476937
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.1760889
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.2035545
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.1841441
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.1722253
    ##                                                                           t value
    ## (Intercept)                                                                 4.489
    ## Week_factor2                                                               -1.834
    ## Week_factor3                                                               -2.298
    ## Week_factor4                                                                1.128
    ## Week_factor5                                                                1.422
    ## Week_factor6                                                               -2.807
    ## Week_factor7                                                               -1.464
    ## Week_factor8                                                               -0.997
    ## Week_factor9                                                                2.089
    ## Week_factor10                                                              -1.206
    ## Week_factor11                                                               1.071
    ## Week_factor12                                                               2.040
    ## Week_factor13                                                              -2.595
    ## Week                                                                       14.260
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -25.499
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         20.020
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         15.006
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))   5.591
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))   2.823
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))   4.417
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))   4.213
    ##                                                                           Pr(>|t|)
    ## (Intercept)                                                               2.29e-05
    ## Week_factor2                                                               0.07028
    ## Week_factor3                                                               0.02411
    ## Week_factor4                                                               0.26261
    ## Week_factor5                                                               0.15871
    ## Week_factor6                                                               0.00622
    ## Week_factor7                                                               0.14703
    ## Week_factor8                                                               0.32174
    ## Week_factor9                                                               0.03981
    ## Week_factor10                                                              0.23107
    ## Week_factor11                                                              0.28718
    ## Week_factor12                                                              0.04454
    ## Week_factor13                                                              0.01119
    ## Week                                                                       < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         < 2e-16
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) 2.82e-07
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.00596
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) 2.99e-05
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) 6.36e-05
    ##                                                                              
    ## (Intercept)                                                               ***
    ## Week_factor2                                                              .  
    ## Week_factor3                                                              *  
    ## Week_factor4                                                                 
    ## Week_factor5                                                                 
    ## Week_factor6                                                              ** 
    ## Week_factor7                                                                 
    ## Week_factor8                                                                 
    ## Week_factor9                                                              *  
    ## Week_factor10                                                                
    ## Week_factor11                                                                
    ## Week_factor12                                                             *  
    ## Week_factor13                                                             *  
    ## Week                                                                      ***
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          ***
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                        ***
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                        ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) ** 
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1698 on 83 degrees of freedom
    ## Multiple R-squared:  0.9635, Adjusted R-squared:  0.9547 
    ## F-statistic: 109.6 on 20 and 83 DF,  p-value: < 2.2e-16
    ## 
    ## [1] "===============brand5================"
    ## 
    ## Call:
    ## lm(formula = log(get(paste0("S", i, "_t"))) ~ Week_factor + Week + 
    ##     eval(parse(text = paste0("log(PriceB", i, ")"))) + eval(parse(text = paste0("log(PriceB", 
    ##     i, "_1)"))) + eval(parse(text = paste0("log(PriceB", i, "_2)"))) + 
    ##     eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 
    ##         1 - 5, i + 1)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 4 > 5, i + 4 - 5, i + 4)))), data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.35295 -0.07166 -0.00201  0.08188  0.26469 
    ## 
    ## Coefficients:
    ##                                                                             Estimate
    ## (Intercept)                                                                1.8784631
    ## Week_factor2                                                              -0.0992806
    ## Week_factor3                                                              -0.2910778
    ## Week_factor4                                                               0.0064193
    ## Week_factor5                                                               0.0788356
    ## Week_factor6                                                              -0.1332555
    ## Week_factor7                                                              -0.1558577
    ## Week_factor8                                                               0.0089765
    ## Week_factor9                                                               0.0861684
    ## Week_factor10                                                             -0.0970467
    ## Week_factor11                                                              0.0478746
    ## Week_factor12                                                              0.2160597
    ## Week_factor13                                                             -0.1640332
    ## Week                                                                       0.0089440
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -3.9681001
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         2.6701656
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         1.7872304
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.9004155
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.8144935
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.8523411
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.7376851
    ##                                                                           Std. Error
    ## (Intercept)                                                                0.2692011
    ## Week_factor2                                                               0.0725616
    ## Week_factor3                                                               0.0739420
    ## Week_factor4                                                               0.0703784
    ## Week_factor5                                                               0.0730754
    ## Week_factor6                                                               0.0719613
    ## Week_factor7                                                               0.0758744
    ## Week_factor8                                                               0.0731823
    ## Week_factor9                                                               0.0730529
    ## Week_factor10                                                              0.0718039
    ## Week_factor11                                                              0.0753927
    ## Week_factor12                                                              0.0760048
    ## Week_factor13                                                              0.0773284
    ## Week                                                                       0.0005543
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           0.1262568
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         0.1211586
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         0.1250198
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.1647244
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.1474151
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.1379255
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.1397617
    ##                                                                           t value
    ## (Intercept)                                                                 6.978
    ## Week_factor2                                                               -1.368
    ## Week_factor3                                                               -3.937
    ## Week_factor4                                                                0.091
    ## Week_factor5                                                                1.079
    ## Week_factor6                                                               -1.852
    ## Week_factor7                                                               -2.054
    ## Week_factor8                                                                0.123
    ## Week_factor9                                                                1.180
    ## Week_factor10                                                              -1.352
    ## Week_factor11                                                               0.635
    ## Week_factor12                                                               2.843
    ## Week_factor13                                                              -2.121
    ## Week                                                                       16.136
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -31.429
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         22.039
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         14.296
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))   5.466
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))   5.525
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))   6.180
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))   5.278
    ##                                                                           Pr(>|t|)
    ## (Intercept)                                                               6.74e-10
    ## Week_factor2                                                              0.174934
    ## Week_factor3                                                              0.000171
    ## Week_factor4                                                              0.927544
    ## Week_factor5                                                              0.283791
    ## Week_factor6                                                              0.067615
    ## Week_factor7                                                              0.043108
    ## Week_factor8                                                              0.902673
    ## Week_factor9                                                              0.241555
    ## Week_factor10                                                             0.180191
    ## Week_factor11                                                             0.527173
    ## Week_factor12                                                             0.005629
    ## Week_factor13                                                             0.036884
    ## Week                                                                       < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         < 2e-16
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) 4.73e-07
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) 3.70e-07
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) 2.29e-08
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) 1.02e-06
    ##                                                                              
    ## (Intercept)                                                               ***
    ## Week_factor2                                                                 
    ## Week_factor3                                                              ***
    ## Week_factor4                                                                 
    ## Week_factor5                                                                 
    ## Week_factor6                                                              .  
    ## Week_factor7                                                              *  
    ## Week_factor8                                                                 
    ## Week_factor9                                                                 
    ## Week_factor10                                                                
    ## Week_factor11                                                                
    ## Week_factor12                                                             ** 
    ## Week_factor13                                                             *  
    ## Week                                                                      ***
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          ***
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                        ***
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                        ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.137 on 83 degrees of freedom
    ## Multiple R-squared:  0.9731, Adjusted R-squared:  0.9666 
    ## F-statistic:   150 on 20 and 83 DF,  p-value: < 2.2e-16

(ii)the value of R-squared and MAPE

``` r
for(i in 1:5){
  # extract R-squared parameter from summary ###to update for testing set but not training set
  print(summary(get(paste0("lm",i)))$r.squared)
}
```

    ## [1] 0.9540728
    ## [1] 0.9627298
    ## [1] 0.9703688
    ## [1] 0.9635254
    ## [1] 0.9730828

``` r
library(MLmetrics)
attach(test)
for(i in 1:5){
  # make predictions on test data
  assign(paste0("lm",i,"_predict"), predict(get(paste0("lm",i)),test))
  # compute MAPE
  print(MAPE(get(paste0("lm",i,"_predict")), get(paste0("S",i,"_t"))))
}
```

    ## [1] 0.9727404
    ## [1] 0.9702746
    ## [1] 0.9624618
    ## [1] 0.9785608
    ## [1] 0.9852506

#### 7. Repeat the same procedure as in Question 6 using the data from the csv file “multipleitem-part2.csv”

``` r
# code from 'Studentspart2.R'
## Read data from CSV file
DATA <- read.csv("multipleitempart2.csv")

## Adding week factor indicator 
DATA$Week_factor <- as.factor(rep(rep(seq(1,13),each=4),3))
```

Train test split

``` r
# set first 2 years of data as training set
train <- DATA %>% filter(Week<=104)

# set the last year of data as testing set
test <- DATA %>% filter(Week>104)

# check the number of rows in each set
nrow(train)
```

    ## [1] 104

``` r
nrow(test)
```

    ## [1] 52

(i)the summary of the regressions

``` r
# run models
for(i in 1:5){
  # run model (1)
  assign(paste0("new_lm",i),lm(log(get(paste0("S",i,"_t"))) ~ Week_factor + Week 
                           + eval(parse(text=paste0("log(PriceB",i,")")))
                           + eval(parse(text=paste0("log(PriceB",i,"_1)")))
                           + eval(parse(text=paste0("log(PriceB",i,"_2)")))
                           + eval(parse(text=paste0("PriceB",ifelse(i+1>5,i+1-5,i+1))))
                           + eval(parse(text=paste0("PriceB",ifelse(i+2>5,i+2-5,i+2))))
                           + eval(parse(text=paste0("PriceB",ifelse(i+3>5,i+3-5,i+3))))
                           + eval(parse(text=paste0("PriceB",ifelse(i+4>5,i+4-5,i+4))))
                           , data = train))
  # generate the summary of each model
  print(paste0("===============brand",i,"================"))
  print(summary(get(paste0("new_lm",i))))
}
```

    ## [1] "===============brand1================"
    ## 
    ## Call:
    ## lm(formula = log(get(paste0("S", i, "_t"))) ~ Week_factor + Week + 
    ##     eval(parse(text = paste0("log(PriceB", i, ")"))) + eval(parse(text = paste0("log(PriceB", 
    ##     i, "_1)"))) + eval(parse(text = paste0("log(PriceB", i, "_2)"))) + 
    ##     eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 
    ##         1 - 5, i + 1)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 4 > 5, i + 4 - 5, i + 4)))), data = train)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.277682 -0.048402  0.003516  0.052455  0.261886 
    ## 
    ## Coefficients:
    ##                                                                             Estimate
    ## (Intercept)                                                                4.7034269
    ## Week_factor2                                                              -0.0888374
    ## Week_factor3                                                              -0.1560253
    ## Week_factor4                                                               0.0383163
    ## Week_factor5                                                               0.0660612
    ## Week_factor6                                                              -0.1771107
    ## Week_factor7                                                              -0.0755209
    ## Week_factor8                                                              -0.0264816
    ## Week_factor9                                                               0.0859439
    ## Week_factor10                                                             -0.0909951
    ## Week_factor11                                                              0.1268405
    ## Week_factor12                                                              0.1533364
    ## Week_factor13                                                             -0.1392781
    ## Week                                                                       0.0095641
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -3.0450153
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         2.7220231
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         1.8900149
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.2670182
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.0532972
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) -0.0504657
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.0315458
    ##                                                                           Std. Error
    ## (Intercept)                                                                0.1937790
    ## Week_factor2                                                               0.0516506
    ## Week_factor3                                                               0.0541550
    ## Week_factor4                                                               0.0512402
    ## Week_factor5                                                               0.0539592
    ## Week_factor6                                                               0.0526416
    ## Week_factor7                                                               0.0555859
    ## Week_factor8                                                               0.0551264
    ## Week_factor9                                                               0.0547754
    ## Week_factor10                                                              0.0521599
    ## Week_factor11                                                              0.0546263
    ## Week_factor12                                                              0.0575226
    ## Week_factor13                                                              0.0562036
    ## Week                                                                       0.0004031
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           0.1042759
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         0.1033685
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         0.1025644
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.1064856
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.1007206
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.1018024
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.1034801
    ##                                                                           t value
    ## (Intercept)                                                                24.272
    ## Week_factor2                                                               -1.720
    ## Week_factor3                                                               -2.881
    ## Week_factor4                                                                0.748
    ## Week_factor5                                                                1.224
    ## Week_factor6                                                               -3.364
    ## Week_factor7                                                               -1.359
    ## Week_factor8                                                               -0.480
    ## Week_factor9                                                                1.569
    ## Week_factor10                                                              -1.745
    ## Week_factor11                                                               2.322
    ## Week_factor12                                                               2.666
    ## Week_factor13                                                              -2.478
    ## Week                                                                       23.727
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -29.202
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         26.333
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         18.428
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))   2.508
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))   0.529
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  -0.496
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))   0.305
    ##                                                                           Pr(>|t|)
    ## (Intercept)                                                                < 2e-16
    ## Week_factor2                                                               0.08916
    ## Week_factor3                                                               0.00504
    ## Week_factor4                                                               0.45671
    ## Week_factor5                                                               0.22431
    ## Week_factor6                                                               0.00116
    ## Week_factor7                                                               0.17794
    ## Week_factor8                                                               0.63222
    ## Week_factor9                                                               0.12045
    ## Week_factor10                                                              0.08477
    ## Week_factor11                                                              0.02269
    ## Week_factor12                                                              0.00923
    ## Week_factor13                                                              0.01524
    ## Week                                                                       < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         < 2e-16
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.01411
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.59811
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.62140
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.76124
    ##                                                                              
    ## (Intercept)                                                               ***
    ## Week_factor2                                                              .  
    ## Week_factor3                                                              ** 
    ## Week_factor4                                                                 
    ## Week_factor5                                                                 
    ## Week_factor6                                                              ** 
    ## Week_factor7                                                                 
    ## Week_factor8                                                                 
    ## Week_factor9                                                                 
    ## Week_factor10                                                             .  
    ## Week_factor11                                                             *  
    ## Week_factor12                                                             ** 
    ## Week_factor13                                                             *  
    ## Week                                                                      ***
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          ***
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                        ***
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                        ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) *  
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))    
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))    
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.09963 on 83 degrees of freedom
    ## Multiple R-squared:  0.9781, Adjusted R-squared:  0.9728 
    ## F-statistic: 184.9 on 20 and 83 DF,  p-value: < 2.2e-16
    ## 
    ## [1] "===============brand2================"
    ## 
    ## Call:
    ## lm(formula = log(get(paste0("S", i, "_t"))) ~ Week_factor + Week + 
    ##     eval(parse(text = paste0("log(PriceB", i, ")"))) + eval(parse(text = paste0("log(PriceB", 
    ##     i, "_1)"))) + eval(parse(text = paste0("log(PriceB", i, "_2)"))) + 
    ##     eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 
    ##         1 - 5, i + 1)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 4 > 5, i + 4 - 5, i + 4)))), data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.24652 -0.05268 -0.01269  0.06620  0.22867 
    ## 
    ## Coefficients:
    ##                                                                             Estimate
    ## (Intercept)                                                                4.6027450
    ## Week_factor2                                                              -0.1854672
    ## Week_factor3                                                              -0.2033528
    ## Week_factor4                                                               0.1005966
    ## Week_factor5                                                               0.1067566
    ## Week_factor6                                                              -0.2389042
    ## Week_factor7                                                              -0.1289166
    ## Week_factor8                                                              -0.0864109
    ## Week_factor9                                                               0.0694081
    ## Week_factor10                                                             -0.0732958
    ## Week_factor11                                                              0.0422819
    ## Week_factor12                                                              0.1978691
    ## Week_factor13                                                             -0.1983566
    ## Week                                                                       0.0100320
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -3.2210758
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         2.9470059
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         1.9536717
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.0317763
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.0531470
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) -0.2317571
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) -1.3907839
    ##                                                                           Std. Error
    ## (Intercept)                                                                0.2322584
    ## Week_factor2                                                               0.0562239
    ## Week_factor3                                                               0.0588695
    ## Week_factor4                                                               0.0554575
    ## Week_factor5                                                               0.0590573
    ## Week_factor6                                                               0.0569097
    ## Week_factor7                                                               0.0597447
    ## Week_factor8                                                               0.0570668
    ## Week_factor9                                                               0.0574595
    ## Week_factor10                                                              0.0567106
    ## Week_factor11                                                              0.0593993
    ## Week_factor12                                                              0.0598796
    ## Week_factor13                                                              0.0609831
    ## Week                                                                       0.0004453
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           0.0993987
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         0.0986218
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         0.0987466
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.1085191
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.1110386
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.1159911
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.1282556
    ##                                                                           t value
    ## (Intercept)                                                                19.817
    ## Week_factor2                                                               -3.299
    ## Week_factor3                                                               -3.454
    ## Week_factor4                                                                1.814
    ## Week_factor5                                                                1.808
    ## Week_factor6                                                               -4.198
    ## Week_factor7                                                               -2.158
    ## Week_factor8                                                               -1.514
    ## Week_factor9                                                                1.208
    ## Week_factor10                                                              -1.292
    ## Week_factor11                                                               0.712
    ## Week_factor12                                                               3.304
    ## Week_factor13                                                              -3.253
    ## Week                                                                       22.530
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -32.406
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         29.882
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         19.785
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))   0.293
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))   0.479
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  -1.998
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) -10.844
    ##                                                                           Pr(>|t|)
    ## (Intercept)                                                                < 2e-16
    ## Week_factor2                                                              0.001432
    ## Week_factor3                                                              0.000871
    ## Week_factor4                                                              0.073301
    ## Week_factor5                                                              0.074279
    ## Week_factor6                                                              6.73e-05
    ## Week_factor7                                                              0.033834
    ## Week_factor8                                                              0.133773
    ## Week_factor9                                                              0.230498
    ## Week_factor10                                                             0.199787
    ## Week_factor11                                                             0.478569
    ## Week_factor12                                                             0.001406
    ## Week_factor13                                                             0.001654
    ## Week                                                                       < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         < 2e-16
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) 0.770392
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) 0.633457
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) 0.048985
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  < 2e-16
    ##                                                                              
    ## (Intercept)                                                               ***
    ## Week_factor2                                                              ** 
    ## Week_factor3                                                              ***
    ## Week_factor4                                                              .  
    ## Week_factor5                                                              .  
    ## Week_factor6                                                              ***
    ## Week_factor7                                                              *  
    ## Week_factor8                                                                 
    ## Week_factor9                                                                 
    ## Week_factor10                                                                
    ## Week_factor11                                                                
    ## Week_factor12                                                             ** 
    ## Week_factor13                                                             ** 
    ## Week                                                                      ***
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          ***
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                        ***
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                        ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))    
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))    
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) *  
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1078 on 83 degrees of freedom
    ## Multiple R-squared:  0.9796, Adjusted R-squared:  0.9746 
    ## F-statistic:   199 on 20 and 83 DF,  p-value: < 2.2e-16
    ## 
    ## [1] "===============brand3================"
    ## 
    ## Call:
    ## lm(formula = log(get(paste0("S", i, "_t"))) ~ Week_factor + Week + 
    ##     eval(parse(text = paste0("log(PriceB", i, ")"))) + eval(parse(text = paste0("log(PriceB", 
    ##     i, "_1)"))) + eval(parse(text = paste0("log(PriceB", i, "_2)"))) + 
    ##     eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 
    ##         1 - 5, i + 1)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 4 > 5, i + 4 - 5, i + 4)))), data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.51699 -0.10299 -0.00527  0.13552  0.42332 
    ## 
    ## Coefficients:
    ##                                                                             Estimate
    ## (Intercept)                                                                4.7311137
    ## Week_factor2                                                              -0.0520715
    ## Week_factor3                                                              -0.2259144
    ## Week_factor4                                                              -0.0287427
    ## Week_factor5                                                               0.0429091
    ## Week_factor6                                                              -0.1700455
    ## Week_factor7                                                              -0.1621558
    ## Week_factor8                                                              -0.0779818
    ## Week_factor9                                                               0.1250798
    ## Week_factor10                                                             -0.1179275
    ## Week_factor11                                                              0.0857790
    ## Week_factor12                                                              0.2131333
    ## Week_factor13                                                             -0.1232575
    ## Week                                                                       0.0085110
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -3.9473133
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         2.6536782
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         1.7941403
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.0171570
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) -0.2874779
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) -1.2301046
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) -0.0041236
    ##                                                                           Std. Error
    ## (Intercept)                                                                0.4253712
    ## Week_factor2                                                               0.1017133
    ## Week_factor3                                                               0.1051889
    ## Week_factor4                                                               0.1013912
    ## Week_factor5                                                               0.1055661
    ## Week_factor6                                                               0.1056690
    ## Week_factor7                                                               0.1086418
    ## Week_factor8                                                               0.1038697
    ## Week_factor9                                                               0.1060988
    ## Week_factor10                                                              0.1033690
    ## Week_factor11                                                              0.1080620
    ## Week_factor12                                                              0.1092168
    ## Week_factor13                                                              0.1179096
    ## Week                                                                       0.0007876
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           0.1755651
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         0.1771461
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         0.1864752
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.2021320
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.2034357
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.2332251
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.2248014
    ##                                                                           t value
    ## (Intercept)                                                                11.122
    ## Week_factor2                                                               -0.512
    ## Week_factor3                                                               -2.148
    ## Week_factor4                                                               -0.283
    ## Week_factor5                                                                0.406
    ## Week_factor6                                                               -1.609
    ## Week_factor7                                                               -1.493
    ## Week_factor8                                                               -0.751
    ## Week_factor9                                                                1.179
    ## Week_factor10                                                              -1.141
    ## Week_factor11                                                               0.794
    ## Week_factor12                                                               1.951
    ## Week_factor13                                                              -1.045
    ## Week                                                                       10.806
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -22.483
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         14.980
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                          9.621
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))   0.085
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  -1.413
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  -5.274
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  -0.018
    ##                                                                           Pr(>|t|)
    ## (Intercept)                                                                < 2e-16
    ## Week_factor2                                                                0.6100
    ## Week_factor3                                                                0.0347
    ## Week_factor4                                                                0.7775
    ## Week_factor5                                                                0.6854
    ## Week_factor6                                                                0.1114
    ## Week_factor7                                                                0.1393
    ## Week_factor8                                                                0.4549
    ## Week_factor9                                                                0.2418
    ## Week_factor10                                                               0.2572
    ## Week_factor11                                                               0.4296
    ## Week_factor12                                                               0.0544
    ## Week_factor13                                                               0.2989
    ## Week                                                                       < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                        3.74e-15
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))   0.9326
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))   0.1614
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) 1.04e-06
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))   0.9854
    ##                                                                              
    ## (Intercept)                                                               ***
    ## Week_factor2                                                                 
    ## Week_factor3                                                              *  
    ## Week_factor4                                                                 
    ## Week_factor5                                                                 
    ## Week_factor6                                                                 
    ## Week_factor7                                                                 
    ## Week_factor8                                                                 
    ## Week_factor9                                                                 
    ## Week_factor10                                                                
    ## Week_factor11                                                                
    ## Week_factor12                                                             .  
    ## Week_factor13                                                                
    ## Week                                                                      ***
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          ***
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                        ***
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                        ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))    
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))    
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1963 on 83 degrees of freedom
    ## Multiple R-squared:  0.9479, Adjusted R-squared:  0.9353 
    ## F-statistic: 75.44 on 20 and 83 DF,  p-value: < 2.2e-16
    ## 
    ## [1] "===============brand4================"
    ## 
    ## Call:
    ## lm(formula = log(get(paste0("S", i, "_t"))) ~ Week_factor + Week + 
    ##     eval(parse(text = paste0("log(PriceB", i, ")"))) + eval(parse(text = paste0("log(PriceB", 
    ##     i, "_1)"))) + eval(parse(text = paste0("log(PriceB", i, "_2)"))) + 
    ##     eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 
    ##         1 - 5, i + 1)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 4 > 5, i + 4 - 5, i + 4)))), data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.46348 -0.11810 -0.00779  0.12277  0.46680 
    ## 
    ## Coefficients:
    ##                                                                             Estimate
    ## (Intercept)                                                                4.5012737
    ## Week_factor2                                                              -0.0599607
    ## Week_factor3                                                              -0.1211237
    ## Week_factor4                                                              -0.0347246
    ## Week_factor5                                                               0.2099183
    ## Week_factor6                                                              -0.0046697
    ## Week_factor7                                                              -0.1067425
    ## Week_factor8                                                               0.1157815
    ## Week_factor9                                                               0.0527739
    ## Week_factor10                                                             -0.0602494
    ## Week_factor11                                                              0.1154126
    ## Week_factor12                                                              0.2040425
    ## Week_factor13                                                             -0.0538253
    ## Week                                                                       0.0090677
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -3.8102388
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         2.9966332
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         2.1506707
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) -0.2533329
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) -0.6446534
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) -0.1291153
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) -0.0376136
    ##                                                                           Std. Error
    ## (Intercept)                                                                0.4582138
    ## Week_factor2                                                               0.1135062
    ## Week_factor3                                                               0.1185789
    ## Week_factor4                                                               0.1133481
    ## Week_factor5                                                               0.1171760
    ## Week_factor6                                                               0.1171877
    ## Week_factor7                                                               0.1254599
    ## Week_factor8                                                               0.1160397
    ## Week_factor9                                                               0.1169520
    ## Week_factor10                                                              0.1146775
    ## Week_factor11                                                              0.1222546
    ## Week_factor12                                                              0.1230844
    ## Week_factor13                                                              0.1269832
    ## Week                                                                       0.0009121
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           0.1942910
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         0.1842699
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         0.1907580
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.2274328
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.2629066
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.2378367
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.2224425
    ##                                                                           t value
    ## (Intercept)                                                                 9.824
    ## Week_factor2                                                               -0.528
    ## Week_factor3                                                               -1.021
    ## Week_factor4                                                               -0.306
    ## Week_factor5                                                                1.791
    ## Week_factor6                                                               -0.040
    ## Week_factor7                                                               -0.851
    ## Week_factor8                                                                0.998
    ## Week_factor9                                                                0.451
    ## Week_factor10                                                              -0.525
    ## Week_factor11                                                               0.944
    ## Week_factor12                                                               1.658
    ## Week_factor13                                                              -0.424
    ## Week                                                                        9.942
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -19.611
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         16.262
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         11.274
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  -1.114
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  -2.452
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  -0.543
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  -0.169
    ##                                                                           Pr(>|t|)
    ## (Intercept)                                                               1.48e-15
    ## Week_factor2                                                                0.5987
    ## Week_factor3                                                                0.3100
    ## Week_factor4                                                                0.7601
    ## Week_factor5                                                                0.0769
    ## Week_factor6                                                                0.9683
    ## Week_factor7                                                                0.3973
    ## Week_factor8                                                                0.3213
    ## Week_factor9                                                                0.6530
    ## Week_factor10                                                               0.6007
    ## Week_factor11                                                               0.3479
    ## Week_factor12                                                               0.1011
    ## Week_factor13                                                               0.6728
    ## Week                                                                      8.59e-16
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         < 2e-16
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))   0.2685
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))   0.0163
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))   0.5887
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))   0.8661
    ##                                                                              
    ## (Intercept)                                                               ***
    ## Week_factor2                                                                 
    ## Week_factor3                                                                 
    ## Week_factor4                                                                 
    ## Week_factor5                                                              .  
    ## Week_factor6                                                                 
    ## Week_factor7                                                                 
    ## Week_factor8                                                                 
    ## Week_factor9                                                                 
    ## Week_factor10                                                                
    ## Week_factor11                                                                
    ## Week_factor12                                                                
    ## Week_factor13                                                                
    ## Week                                                                      ***
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          ***
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                        ***
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                        ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))    
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) *  
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))    
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2193 on 83 degrees of freedom
    ## Multiple R-squared:  0.9321, Adjusted R-squared:  0.9157 
    ## F-statistic: 56.95 on 20 and 83 DF,  p-value: < 2.2e-16
    ## 
    ## [1] "===============brand5================"
    ## 
    ## Call:
    ## lm(formula = log(get(paste0("S", i, "_t"))) ~ Week_factor + Week + 
    ##     eval(parse(text = paste0("log(PriceB", i, ")"))) + eval(parse(text = paste0("log(PriceB", 
    ##     i, "_1)"))) + eval(parse(text = paste0("log(PriceB", i, "_2)"))) + 
    ##     eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 
    ##         1 - 5, i + 1)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) + eval(parse(text = paste0("PriceB", 
    ##     ifelse(i + 4 > 5, i + 4 - 5, i + 4)))), data = train)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.44102 -0.11409  0.01981  0.12452  0.43655 
    ## 
    ## Coefficients:
    ##                                                                             Estimate
    ## (Intercept)                                                                4.7259177
    ## Week_factor2                                                              -0.1064732
    ## Week_factor3                                                              -0.0647085
    ## Week_factor4                                                               0.1687617
    ## Week_factor5                                                               0.3368542
    ## Week_factor6                                                              -0.0291836
    ## Week_factor7                                                               0.0106929
    ## Week_factor8                                                               0.1362195
    ## Week_factor9                                                               0.2034165
    ## Week_factor10                                                              0.0036360
    ## Week_factor11                                                              0.3422562
    ## Week_factor12                                                              0.3477222
    ## Week_factor13                                                              0.0372765
    ## Week                                                                       0.0082341
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -3.9886138
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         2.7220686
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         2.1296599
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) -1.3279324
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2)))) -0.0575898
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3)))) -0.0938733
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4)))) -0.2264517
    ##                                                                           Std. Error
    ## (Intercept)                                                                0.3698127
    ## Week_factor2                                                               0.0996809
    ## Week_factor3                                                               0.1015772
    ## Week_factor4                                                               0.0966817
    ## Week_factor5                                                               0.1003867
    ## Week_factor6                                                               0.0988562
    ## Week_factor7                                                               0.1042318
    ## Week_factor8                                                               0.1005335
    ## Week_factor9                                                               0.1003557
    ## Week_factor10                                                              0.0986400
    ## Week_factor11                                                              0.1035701
    ## Week_factor12                                                              0.1044109
    ## Week_factor13                                                              0.1062293
    ## Week                                                                       0.0007614
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           0.1734442
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         0.1664406
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         0.1717449
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  0.2262888
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.2025103
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.1894740
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.1919965
    ##                                                                           t value
    ## (Intercept)                                                                12.779
    ## Week_factor2                                                               -1.068
    ## Week_factor3                                                               -0.637
    ## Week_factor4                                                                1.746
    ## Week_factor5                                                                3.356
    ## Week_factor6                                                               -0.295
    ## Week_factor7                                                                0.103
    ## Week_factor8                                                                1.355
    ## Week_factor9                                                                2.027
    ## Week_factor10                                                               0.037
    ## Week_factor11                                                               3.305
    ## Week_factor12                                                               3.330
    ## Week_factor13                                                               0.351
    ## Week                                                                       10.814
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          -22.997
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         16.355
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         12.400
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1))))  -5.868
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  -0.284
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  -0.495
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  -1.179
    ##                                                                           Pr(>|t|)
    ## (Intercept)                                                                < 2e-16
    ## Week_factor2                                                               0.28855
    ## Week_factor3                                                               0.52585
    ## Week_factor4                                                               0.08459
    ## Week_factor5                                                               0.00120
    ## Week_factor6                                                               0.76857
    ## Week_factor7                                                               0.91854
    ## Week_factor8                                                               0.17910
    ## Week_factor9                                                               0.04588
    ## Week_factor10                                                              0.97068
    ## Week_factor11                                                              0.00141
    ## Week_factor12                                                              0.00130
    ## Week_factor13                                                              0.72655
    ## Week                                                                       < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                           < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                         < 2e-16
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                         < 2e-16
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) 8.72e-08
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))  0.77683
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))  0.62160
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))  0.24159
    ##                                                                              
    ## (Intercept)                                                               ***
    ## Week_factor2                                                                 
    ## Week_factor3                                                                 
    ## Week_factor4                                                              .  
    ## Week_factor5                                                              ** 
    ## Week_factor6                                                                 
    ## Week_factor7                                                                 
    ## Week_factor8                                                                 
    ## Week_factor9                                                              *  
    ## Week_factor10                                                                
    ## Week_factor11                                                             ** 
    ## Week_factor12                                                             ** 
    ## Week_factor13                                                                
    ## Week                                                                      ***
    ## eval(parse(text = paste0("log(PriceB", i, ")")))                          ***
    ## eval(parse(text = paste0("log(PriceB", i, "_1)")))                        ***
    ## eval(parse(text = paste0("log(PriceB", i, "_2)")))                        ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 1 > 5, i + 1 - 5, i + 1)))) ***
    ## eval(parse(text = paste0("PriceB", ifelse(i + 2 > 5, i + 2 - 5, i + 2))))    
    ## eval(parse(text = paste0("PriceB", ifelse(i + 3 > 5, i + 3 - 5, i + 3))))    
    ## eval(parse(text = paste0("PriceB", ifelse(i + 4 > 5, i + 4 - 5, i + 4))))    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1882 on 83 degrees of freedom
    ## Multiple R-squared:  0.9507, Adjusted R-squared:  0.9388 
    ## F-statistic: 79.95 on 20 and 83 DF,  p-value: < 2.2e-16

(ii)the value of R-squared and MAPE

``` r
for(i in 1:5){
  # extract R-squared parameter from summary ###to update for testing set but not training set
  print(summary(get(paste0("new_lm",i)))$r.squared)
}
```

    ## [1] 0.9780533
    ## [1] 0.9795717
    ## [1] 0.9478599
    ## [1] 0.9320802
    ## [1] 0.9506542

``` r
library(MLmetrics)
attach(test)
```

    ## The following objects are masked from test (pos = 3):
    ## 
    ##     PriceB1, PriceB1_1, PriceB1_2, PriceB2, PriceB2_1, PriceB2_2,
    ##     PriceB3, PriceB3_1, PriceB3_2, PriceB4, PriceB4_1, PriceB4_2,
    ##     PriceB5, PriceB5_1, PriceB5_2, S1_t, S2_t, S3_t, S4_t, S5_t, Week,
    ##     Week_factor

``` r
for(i in 1:5){
  # make predictions on test data
  assign(paste0("new_lm",i,"_predict"), predict(get(paste0("new_lm",i)),test))
  # compute MAPE
  print(MAPE(get(paste0("new_lm",i,"_predict")), get(paste0("S",i,"_t"))))
}
```

    ## [1] 0.9839391
    ## [1] 0.9379633
    ## [1] 0.9491658
    ## [1] 0.9555869
    ## [1] 0.9422026

#### 8. Compute the values of the predicted demands

``` r
# prediction using model (3) when PriceB1_7=1
for(i in 1:5){
  assign(paste0("PriceB",i,"_7"),1) #PriceB1_7, when i=1
}

for (i in 1:5){
  PriceB1_7=1 #overwrite with case PriceB1_7=1
  assign(paste0("PriceB",i,"_6"),1) #PriceB1_6, when i=1
  assign(paste0("PriceB",i,"_5"),1) #PriceB1_5, when i=1
  
  assign(paste0("a",i,"_0"),coef(get(paste0("lm",i)))[1]) #a1_0, when i=1
  a2=coef(get(paste0("lm",i)))[2]
  trend=coef(get(paste0("lm",i)))[14]
  
  assign(paste0("b",i,"_0"),as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[15]))) #b1_0, when i=1
  assign(paste0("b",i,"_1"),as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[16]))) #b1_1, when i=1
  assign(paste0("b",i,"_2"),as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[17]))) #b1_2, when i=1
  
  s1=as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[18])) #s1
  s2=as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[19])) #s2
  s3=as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[20])) #s3
  s4=as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[21])) #s4
  
  #assign(paste0("S",i,"_t * PriceB",ifelse(i+1>5,i+1-5,i+1)),as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[18]))) #s2_1, when i=1
  #assign(paste0("S",i,"_t * PriceB",ifelse(i+2>5,i+2-5,i+2)),as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[19]))) #s3_1, when i=1
  #assign(paste0("S",i,"_t * PriceB",ifelse(i+3>5,i+3-5,i+3)),as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[20]))) #s4_1, when i=1
  #assign(paste0("S",i,"_t * PriceB",ifelse(i+4>5,i+4-5,i+4)),as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[21]))) #s5_1, when i=1
  
  print(exp(get(paste0("a",i,"_0")) #a1_0, when i=1
            +a2
            +trend*7
            +get(paste0("b",i,"_0"))*log(get(paste0("PriceB",i,"_7"))) #b1_0*log(PriceB1_7), when i=1
            +get(paste0("b",i,"_1"))*log(get(paste0("PriceB",i,"_6"))) #b1_1*log(PriceB1_6), when i=1
            +get(paste0("b",i,"_2"))*log(get(paste0("PriceB",i,"_5"))) #b1_2*log(PriceB1_5), when i=1
            +s1*get(paste0("PriceB",ifelse(i+1>5,i+1-5,i+1),"_7")) #s1*PriceB2_7, when i=1
            +s2*get(paste0("PriceB",ifelse(i+2>5,i+2-5,i+2),"_7")) #s2*PriceB3_7, when i=1
            +s3*get(paste0("PriceB",ifelse(i+3>5,i+3-5,i+3),"_7")) #s3*PriceB4_7, when i=1
            +s4*get(paste0("PriceB",ifelse(i+4>5,i+4-5,i+4),"_7")))) #s4*PriceB5_7, when i=1
}
```

    ## (Intercept) 
    ##    80.54661 
    ## (Intercept) 
    ##    68.55911 
    ## (Intercept) 
    ##    47.34695 
    ## (Intercept) 
    ##    99.48815 
    ## (Intercept) 
    ##    171.8678

``` r
# prediction using model (3) when PriceB1_7=0.7
for(i in 1:5){
  assign(paste0("PriceB",i,"_7"),1) #PriceB1_7, when i=1
}

for (i in 1:5){
  PriceB1_7=0.7 #overwrite with case PriceB1_7=0.7
  assign(paste0("PriceB",i,"_6"),1) #PriceB1_6, when i=1
  assign(paste0("PriceB",i,"_5"),1) #PriceB1_5, when i=1
  
  assign(paste0("a",i,"_0"),coef(get(paste0("lm",i)))[1]) #a1_0, when i=1
  a2=coef(get(paste0("lm",i)))[2]
  trend=coef(get(paste0("lm",i)))[14]
  
  assign(paste0("b",i,"_0"),as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[15]))) #b1_0, when i=1
  assign(paste0("b",i,"_1"),as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[16]))) #b1_1, when i=1
  assign(paste0("b",i,"_2"),as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[17]))) #b1_2, when i=1
  
  s1=as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[18])) #s1
  s2=as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[19])) #s2
  s3=as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[20])) #s3
  s4=as.numeric(sub("^.+) ", "", coef(get(paste0("lm",i)))[21])) #s4
  
  print(exp(get(paste0("a",i,"_0")) #a1_0, when i=1
            +a2
            +trend*7
            +get(paste0("b",i,"_0"))*log(get(paste0("PriceB",i,"_7"))) #b1_0*log(PriceB1_7), when i=1
            +get(paste0("b",i,"_1"))*log(get(paste0("PriceB",i,"_6"))) #b1_1*log(PriceB1_6), when i=1
            +get(paste0("b",i,"_2"))*log(get(paste0("PriceB",i,"_5"))) #b1_2*log(PriceB1_5), when i=1
            +s1*get(paste0("PriceB",ifelse(i+1>5,i+1-5,i+1),"_7")) #s1*PriceB2_7, when i=1
            +s2*get(paste0("PriceB",ifelse(i+2>5,i+2-5,i+2),"_7")) #s2*PriceB3_7, when i=1
            +s3*get(paste0("PriceB",ifelse(i+3>5,i+3-5,i+3),"_7")) #s3*PriceB4_7, when i=1
            +s4*get(paste0("PriceB",ifelse(i+4>5,i+4-5,i+4),"_7")))) #s4*PriceB5_7, when i=1
}
```

    ## (Intercept) 
    ##    275.9135 
    ## (Intercept) 
    ##    55.82486 
    ## (Intercept) 
    ##    36.57359 
    ## (Intercept) 
    ##    83.73608 
    ## (Intercept) 
    ##     131.184

From the above estimation, we can see that the demand of setting the
price of brand 1 lower will increase the demand significantly from 80.5
to 276.0, while the effect to other brands’ demand are negative and of
relatively lower magnitude - demand of brands 2-5 will drop for a range
of 10.8 to 40.8. Overall, the net demand change for all brands is
positive for a net +115.4, i.e. promoting brand 1 gives a boost of
demand from consumers overall.

#### 9. Compare the estimated parameters from Questions 6 and 7. What is the difference between the 5 brands from Question 6 and the 5 brands from Question 7? Please, discuss.

### Optimization Formulation

#### 10. What types of objectives would you consider? Please discuss.

#### 11. How can one represent such a restriction using linear constraints and binary variables?

#### 12. How can you model this business rule as a linear constraint with binary variables?

#### 13. Please, formulate the optimization problem with the constraint on the limitation of the number of promotions to be at most 4

#### 14. What tests would you conduct in order to check that the suggested prices are robust? Please discuss.

#### 15. Should the promotions of the different items be at the same time, or at different times? Please discuss the various factors that may affect the answer to this question.
