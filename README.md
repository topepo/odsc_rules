# odsc_rules

## R Packages:


Notes and code for the workshop "Rule-Based Models for Regression and Classification” presented at the 2016 Open Data Science Conference in Boston. 

The packages are used to create the majority of the slides:

 * [`C50`](https://cran.r-project.org/package=C50) 
 * [`caret`](http://topepo.github.io/caret/)
 * [`Cubist`](https://cran.r-project.org/package=Cubist)
 * [`ggmap`](https://cran.r-project.org/package=ggmap)
 * [`inTrees`](https://cran.r-project.org/package=inTrees)
 * [`pROC`](https://cran.r-project.org/package=pROC)
 * [`randomForest`](https://cran.r-project.org/package=randomForest)
 * [`RCurl`](https://cran.r-project.org/package=RCurl) 

Other packages are needed to create all the slides, especially to compare against the other models. The `sessionInfo` from when I ran the code to create the slides is:

```r
> sessionInfo()
R Under development (unstable) (2016-04-08 r70447)
Platform: x86_64-apple-darwin13.4.0 (64-bit)
Running under: OS X 10.10.5 (Yosemite)

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
 [1] parallel  splines   grid      stats     graphics  grDevices utils    
 [8] datasets  methods   base     

other attached packages:
 [1] car_2.1-2                       reshape2_1.4.1                 
 [3] doMC_1.3.4                      iterators_1.0.8                
 [5] foreach_1.4.3                   Hmisc_3.17-3                   
 [7] Formula_1.2-1                   ggmap_2.6.1                    
 [9] RWeka_0.4-26                    kernlab_0.9-24                 
[11] ranger_0.4.0                    gbm_2.1.1                      
[13] survival_2.38-3                 partykit_1.0-5                 
[15] rpart_4.1-10                    Cubist_0.0.18                  
[17] pROC_1.8                        MASS_7.3-45                    
[19] AppliedPredictiveModeling_1.1-6 caret_6.0-69                   
[21] ggplot2_2.1.0                   lattice_0.20-33                
[23] knitr_1.12.3                   

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.4         png_0.1-7           digest_0.6.9       
 [4] plyr_1.8.3          acepack_1.3-3.3     MatrixModels_0.4-1 
 [7] stats4_3.4.0        evaluate_0.8.3      highr_0.5.1        
[10] RWekajars_3.7.13-1  RgoogleMaps_1.2.0.7 minqa_1.2.4        
[13] geosphere_1.5-1     SparseM_1.7         nloptr_1.0.4       
[16] Matrix_1.2-4        labeling_0.3        proto_0.3-10       
[19] lme4_1.1-11         stringr_1.0.0       foreign_0.8-66     
[22] munsell_0.4.3       mgcv_1.8-12         nnet_7.3-12        
[25] gridExtra_2.2.1     CORElearn_1.47.1    codetools_0.2-14   
[28] nlme_3.1-126        gtable_0.2.0        magrittr_1.5       
[31] formatR_1.3         scales_0.4.0        stringi_1.0-1      
[34] mapproj_1.2-4       sp_1.2-2            latticeExtra_0.6-28
[37] rjson_0.2.15        RColorBrewer_1.1-2  tools_3.4.0        
[40] RJSONIO_1.3-0       maps_3.1.0          jpeg_0.1-8         
[43] pbkrtest_0.4-6      colorspace_1.2-6    cluster_2.0.3      
[46] rJava_0.9-8         quantreg_5.21
```

## Data

Two main data sets are used to compute the models. One is in the [`caret`](http://topepo.github.io/caret/) package and the other is a credit scoring dataset from Gaston Sanchez by way of [this blog post](http://www.milanor.net/blog/cross-validation-for-predictive-analytics-using-r/) by Sergio Venturini:

```r
library(RCurl)
url <- "https://raw.githubusercontent.com/gastonstat/CreditScoring/master/CleanCreditScoring.csv"
cs_data <- getURL(url)
cs_data <- read.csv(textConnection(cs_data))
## Remove some predictors that are discrete versions of existing fields 
cs_data <- cs_data[, !grepl("R$", names(cs_data))]
```
