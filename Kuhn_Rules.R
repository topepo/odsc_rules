###################################################################
## R code for the ODSC Workshop "Rule-Based Models for Regression 
## and Classification" by Max Kuhn

###################################################################
## Load Regression Data

library(caret)
data(Sacramento)
str(Sacramento)

###################################################################
## Split into training and test sets

set.seed(955)
in_train <- createDataPartition(log10(Sacramento$price), p = .8, list = FALSE)

training <- Sacramento[ in_train,]
testing  <- Sacramento[-in_train,]
nrow(training)
nrow(testing)

###################################################################
## Plot the training and test set locations

library(ggmap)

ca_map <- qmap("folsom ca",
               color = "bw",
               legend = "topleft", 
               darken = 0, zoom = 10)

ca_map +
  geom_point(data = testing, aes(x = longitude, y = latitude), col = "red", alpha = .5, size = 3) +
  geom_point(data = training, aes(x = longitude, y = latitude), col = "blue", alpha = .3, size = 3) 

###################################################################
## Create intial model trees/rules

library(RWeka)

mt1 <- M5P(log10(price) ~ ., data = training, control = Weka_control(M = 15))
mt2 <- M5P(log10(price) ~ ., data = training, control = Weka_control(M = 15, N = TRUE))

###################################################################
## Setup resampling indicators for model tuning across models

test_results <- data.frame(price = log10(testing$price))
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5,
                     returnResamp = "final",
                     verboseIter = FALSE)

###################################################################
## Tune the rule- and tree-based models

set.seed(139)
mt_tune <- train(x = training[, -7], y = log10(training$price),
                 method = "M5", 
                 tuneGrid = expand.grid(rules = "Yes",
                                        pruned = c("No", "Yes"),
                                        smoothed = c("No", "Yes")),
                 trControl = ctrl)
set.seed(139)
mt_tune_dv <- train(log10(price) ~ ., data = training,
                    method = "M5", 
                    tuneGrid = expand.grid(rules = "Yes",
                                           pruned = c("No", "Yes"),
                                           smoothed = c("No", "Yes")),
                    trControl = ctrl)

###################################################################
## Fit the intial model rules using cubist

library(Cubist)
cb <- cubist(x = training[, -7], y = log10(training$price))

###################################################################
## Plot the slopes and coverage for the model

dotplot(cb)
dotplot(cb, what = "coefs")

###################################################################
## Examine the data from Rule #3

r3_data <- subset(training, beds > 2 & 	sqft <= 1712 & 
                    zip %in% c("z95621", "z95673", "z95817", "z95820", 
                               "z95822", "z95826", "z95828", "z95842",
                               "z95843"))

(mod1 <- lm(log10(price) ~ beds+baths, data = r3_data))

(mod2 <- lm(log10(price) ~ beds+baths+sqft, data = r3_data))

(mod3 <- lm(log10(price) ~ beds+baths+sqft+longitude, data = r3_data))

## the VIFs are not too bad:
library(car)
vif(mod3)
## fold increase in standard error:
sqrt(vif(mod3))

ggplot(training, aes(x = sqft, y = price, color = factor(beds))) + 
  geom_point() + geom_smooth(method = lm, se = FALSE) + 
  theme(legend.position = "top")

ggplot(r3_data, aes(x = sqft, y = price, color = factor(baths))) + 
  geom_point() + geom_smooth(method = lm, se = FALSE) + 
  theme(legend.position = "top")

###################################################################
## Process subsequent models using parallel processing. doMC is 
## specific to linux and OS X. Try doParallel for windows

# library(doMC)
# registerDoMC(cores=11)

###################################################################
## Cubist with committees. Tune the cubist model trying dummy 
## variables and factor encodings

set.seed(139)
cb_tune_dv <- train(log10(price) ~ ., data = training,
                    method = "cubist", 
                    tuneGrid = expand.grid(committees = c(1:35),
                                           neighbors = c(0, 1, 3, 5, 7, 9)),
                    trControl = ctrl)
set.seed(139)
cb_tune <- train(x = training[, -7], y = log10(training$price),
                 method = "cubist", 
                 tuneGrid = expand.grid(committees = c(1:35),
                                        neighbors = c(0, 1, 3, 5, 7, 9)),
                 trControl = ctrl)

test_results$cb <- predict(cb_tune, testing)
test_results$cb_dv <- predict(cb_tune_dv, testing)


###################################################################
## Here are some other model fits that will be used in comparisons

set.seed(139)
rp_tune <- train(x = training[, -7], y = log10(training$price),
                 method = "rpart", 
                 tuneLength = 15,
                 trControl = ctrl)

set.seed(139)
rp_tune_dv <- train(log10(price) ~ ., data = training,
                    method = "rpart", 
                    tuneLength = 15,
                    trControl = ctrl)
test_results$rp <- predict(rp_tune, testing)
test_results$rp_dv <- predict(rp_tune_dv, testing)

set.seed(139)
rf_tune <- train(x = training[, -7], y = log10(training$price),
                 method = "ranger", 
                 tuneGrid = data.frame(mtry = 1:8),
                 trControl = ctrl,
                 num.trees = 1500)

set.seed(139)
rf_tune_dv <- train(log10(price) ~ ., data = training,
                    method = "ranger", 
                    tuneGrid = data.frame(mtry = 1:8),
                    trControl = ctrl,
                    num.trees = 1500)

test_results$rf <- predict(rf_tune, testing)
test_results$rf_dv <- predict(rf_tune_dv, testing)

gbm_grid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                        n.trees = seq(100, 1000, by = 100),
                        shrinkage = c(0.01, 0.001),
                        n.minobsinnode = 10)

gbm_tune <- train(x = training[, -7], y = log10(training$price),
                  method = "gbm",
                  preProc = "zv",
                  verbose = FALSE,
                  tuneGrid = gbm_grid,
                  trControl = ctrl)

gbm_tune_dv <- train(log10(price) ~ ., data = training,
                     method = "gbm",
                     preProc = "zv",
                     verbose = FALSE,
                     tuneGrid = gbm_grid,
                     trControl = ctrl)

test_results$gbm <- predict(gbm_tune, testing)
test_results$gbm_dv <- predict(gbm_tune_dv, testing)

set.seed(139)
mars_tune <- train(log10(price) ~ ., data = training,
                   method = "earth",
                   preProc = "zv",
                   tuneGrid = expand.grid(degree = 1:2, nprune = 2:30),
                   trControl = ctrl)
test_results$mars <- predict(mars_tune, testing)

set.seed(139)
knn_tune_dv <- train(log10(price) ~ ., data = training,
                     method = "kknn", 
                     preProc = c("center", "scale", "zv"),
                     tuneGrid = expand.grid(kmax = 1:10,
                                            distance = 2,
                                            kernel = c("rectangular", "triangular", 
                                                       "cos", "gaussian", "rank")),
                     trControl = ctrl)
test_results$knn <- predict(knn_tune_dv, testing)

ctrl_r <- ctrl
ctrl_r$search  <- "random"
set.seed(139)
svm_tune_dv <- train(log10(price) ~ ., data = training,
                     method = "svmRadial",
                     preProc = c("center", "scale", "zv"),
                     tuneLength = 30,
                     trControl = ctrl_r)

set.seed(139)
svmp_tune_dv <- train(log10(price) ~ ., data = training,
                      method = "svmPoly",
                      preProc = c("center", "scale", "zv"),
                      tuneLength = 30,
                      trControl = ctrl_r)
test_results$svmR <- predict(svm_tune_dv, testing)
test_results$svmP <- predict(svmp_tune_dv, testing)

###################################################################
## Download classification data

library(RCurl)
url <- "https://raw.githubusercontent.com/gastonstat/CreditScoring/master/CleanCreditScoring.csv"
cs_data <- getURL(url)
cs_data <- read.csv(textConnection(cs_data))
## Remove some predictors that are discrete versions of existing fields 
cs_data <- cs_data[, !grepl("R$", names(cs_data))]

str(cs_data)

###################################################################
## split into training and test

set.seed(1987)
inTrain <- createDataPartition(cs_data$Status, p = .8, list = FALSE)

credit_train <- cs_data[ inTrain,]
credit_test  <- cs_data[-inTrain,]

table(credit_test$Status)/nrow(credit_test)
table(cs_data$Status)/nrow(cs_data)

###################################################################
## For C5.0 trees and rules

library(C50)

c5_tree  <- C5.0(Status ~ ., data = credit_train)
c5_rules <- C5.0(Status ~ ., data = credit_train, rules = TRUE)

summary(c5_rules)

###################################################################
## C5.0 test set results

library(pROC)
c5_rules_pred <- predict(c5_rules, credit_test, type = "prob")
c5_rules_roc <- roc(credit_test$Status, c5_rules_pred[, "good"])
c5_rules_roc

plot(c5_rules_roc, type = "s")

###################################################################
## Boosted C5.0 rules

set.seed(1658)
rule_boost <- train(Status ~ ., data = credit_train,
                    method = "C5.0",
                    metric = "ROC",
                    tuneGrid = data.frame(trials = c(1:10, 20, 30, 50), 
                                          model = "rules", 
                                          winnow = FALSE),
                    trControl = trainControl(method = "repeatedcv", 
                                             repeats = 5, 
                                             classProbs = TRUE,
                                             summaryFunction = twoClassSummary))

ggplot(rule_boost)

###################################################################
## Convert a bagged CART tree to rules

library(inTrees)
set.seed(910)
bag_fit <- randomForest(x = credit_train[,-1], 
                        y = credit_train$Status,
                        mtry = ncol(credit_train) - 1,
                        ntree = 25) # make larger for RF
bag_fit_list <- RF2List(bag_fit)  
bag_rules <- extractRules(bag_fit_list,
                          X = credit_train[,-1], 
                          maxdepth = 10, 
                          ntree = 25) 
bag_rules[1]

rule_metric <- getRuleMetric(bag_rules, X = credit_train[,-1], 
                             target = credit_train$Status) 
pruned <- pruneRule(rule_metric, X = credit_train[,-1], 
                    target = credit_train$Status) 
filtered <- selectRuleRRF(pruned, X = credit_train[,-1], 
                          target = credit_train$Status) 
bag_rule_mod <-  buildLearner(filtered, X = credit_train[,-1], 
                              target = credit_train$Status)
applyLearner(bag_rule_mod, credit_test[1:4,-1])
