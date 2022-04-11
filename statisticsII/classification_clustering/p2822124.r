#' ---
#' title: "R Notebook"
#' output: html_notebook
#' ---
#' 
#' ## Libraries
#' 
#' )
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("readxl",
                 "tidyr",
                 "dplyr",
                 "janitor",
                 "psych",
                 "mice",
                 "VIM",
                 "ggplot2",
                 "kableExtra",
                 "apaTables",
                 dependencies = TRUE)

remotes::install_github("rempsyc/rempsyc")

#' 
## ----message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(plyr)
library("readxl")
library("tidyr")
library(dplyr)
library(janitor)
library(psych)
library(mice)
library(VIM)
library(ggplot2)
library(apaTables)
library(rempsyc)

#' 
#' ## Data
#' 
#' ### Download
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
url <- "https://auebgr-my.sharepoint.com/:x:/g/personal/p2822124_aueb_gr/EVGwW_HkW79KlRFsbszoEAMBep3wzTddjFolnobolw643Q?e=VyNMjH&download=1"
destfile <- "bank_data.xls"
download.file(url, destfile, mode="wb")

#' 
#' ### Consume
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data <- read_excel(destfile)

head(data)
dim(data)

#' 
#' ### Clean
#' 
#' #### Duplicates
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data <- distinct(data)
dim(data)

#' 
#' #### Column names
#' 
#' Clean column names using janitor
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data <- clean_names(data)
colnames(data)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(data)

#' 
#' -   `duration` not important - it fully explains the variable we are trying to predict.
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data$duration <- NULL

#' 
#' ### Prepare data
#' 
#' #### Numeric vars
#' 
#' -   Create an index for the numeric variables.
#' -   Use the index to create a frame with the numeric vars.
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data.num_index <- sapply(data, class) == "numeric"
data.numeric <- data[, data.num_index]
data.numeric <- lapply(data.numeric, as.numeric)
data.numeric <- as.data.frame(data.numeric)
str(data.numeric)

#' 
#' -   Check for NAs
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library: mice
md.pattern(data.numeric)

#' 
#' #### non-numeric vars
#' 
#' -   Create an index for the non-numeric variables.
#' -   Use the index to create a frame with the non-numeric vars.
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data.categorical <- as.data.frame(data[, !data.num_index])

data.categorical$education[data.categorical$education == "unknown"] <- NA
data.categorical$default[data.categorical$default == "unknown"] <- NA
data.categorical$housing[data.categorical$housing == "unknown"] <- NA
data.categorical$loan[data.categorical$loan == "unknown"] <- NA
data.categorical$marital[data.categorical$marital == "unknown"] <- NA
data.categorical$job[data.categorical$job == "unknown"] <- NA

# short day_of_week for consistency
colnames(data.categorical)[which(names(data.categorical) == "day_of_week")] <- "dow"

data.categorical <- lapply(data.categorical, as.factor)
data.categorical <- as.data.frame(data.categorical)
str(data.categorical)


#' 
#' -   Check for NAs
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# there are a lot of obserbations with NA
# let's plot them
# install.packages("VIM")

# check categorical variables for NAs
md.pattern(data.categorical, plot = F)

#' 
#' -   Plot Missing values
#' 
## ----message=FALSE, warning=FALSE, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------
mice_plot <- aggr(data.categorical, col = c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE, sortCombs = T,
                  labels=names(data.categorical), cex.axis=.8,
                  gap=3, ylab=c("Missing data","Pattern"), 
                  digits = 2, prop=F, only.miss=F)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

capture.output(plot(mice_plot,
                    col = c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE, sortCombs = T,
                    labels=names(data.categorical), cex.axis=.8,
                    gap=3, ylab=c("Missing data","Pattern"), 
                    digits = 2, prop=F, only.miss=F), file = tempfile())

#' 
#' It seems like the 21% of 'default' values variable is NA.
#' 
#' Others affect a smaller percentage of the dataset but there are quite a few.
#' 
#' ### Dataset setup
#' 
#' We are going to bind the split dataset and use imputation
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data <- cbind(data.numeric, data.categorical)

#' 
#' ### Imputation by mice
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
init = mice(data, maxit=0)
meth = init$method
predM = init$predictorMatrix

#specifying the suitable methods for each variable
meth[c("loan", "default", "housing")]="logreg" 
meth[c("job", "marital", "education")]="polyreg"

#running multiple imputation # m= 5
imputed_data = mice(data, method=meth, predictorMatrix=predM, m=5)
data <- complete(imputed_data)

#' 
#' Verify that unknowns have been replaced.
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
md.pattern(data, plot = F)

#' 
#' Keep the `data.categorical` frame in-sync
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# replace completed columns in the source data set
data.categorical$loan <- data$loan
data.categorical$default <- data$default
data.categorical$housing <- data$housing
data.categorical$job <- data$job
data.categorical$marital <- data$marital
data.categorical$education <- data$education

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv2(data, "imputed.csv")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
imputed <- read.csv2("imputed.csv")[,-1]
head(imputed)

#' 
#' ## EDA
#' 
#' ### `pdays`
#' 
#' We see that pdays has values on 999 and we consider that as an unknown value. We are going to factorize the variable
#' 
#' We now change it to a factor with meaningful context.
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data.categorical$pdays <- as.factor(data$pdays)
data.numeric$pdays <- NULL
summary(data.categorical$pdays)

levels(data.categorical$pdays)

# 999 is a valid value, which means this was the first time contact with the customer
length(ordered(unique(data$pdays)))
# Reducing the levels of this variable to 3.

levels(data.categorical$pdays)[1:10] <- "Contacted_in_first_10days"
levels(data.categorical$pdays)[2:11] <-"Contacted_after_10days"
levels(data.categorical$pdays)[3] <- "First_time_contacted"

data$pdays <- data.categorical$pdays

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)

ggplot(data.categorical, aes(x=pdays, fill=pdays )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") + ggtitle("Previous days count per category")+
  theme(plot.title = element_text(hjust = 0.5))

#' 
#' Response Rate is significantly higher for prospects that have been contacted within 27 days as compared to first time contacts\
#' Number of prospects under each category
#' 
#' #### Education
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
levels(data.categorical$education)
plot(data.categorical$education)

#' 
#' Reducing the levels of education variable
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
levels(data.categorical$education)[c(1:3,5)] <- "Primary_Education"
levels(data.categorical$education)[2] <- "Secondary_Education"
levels(data.categorical$education)[c(3:4)]<- "Tertiary_Education"
summary(data.categorical$education)
data$education <- data.categorical$education

ggplot(data.categorical, aes(x=education, fill=education)) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") + ggtitle("Total contacts per education level")+
  theme(plot.title = element_text(hjust = 0.5))

#' 
#' #### Age
#' 
#' Outlier treatement in Age
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
quantile(data$age,seq(0,1,0.01))

boxplot(data$age)

#' 
#' Box plot shows quiet a no of outliers.
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(data$age)
# Binning the age variable and store it into "binning.age" for the purpose of analysis.
data$age <- as.factor(cut(data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 100)))
data.categorical$age <- data$age
data.numeric$age <- NULL

#' 
#' #### previous
#' 
#' "previous" means, number of contacts performed before this campaign
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(data.numeric$previous)
unique(data.numeric$previous)

#' 
#' Max = 5, converting this variable to factor
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data.categorical$previous <- as.factor(data.numeric$previous)
data.numeric$previous <- NULL
levels(data.categorical$previous)

levels(data.categorical$previous)[1]<-"Never contacted"
levels(data.categorical$previous)[2:4] <- "Less_than_or_3_times"
levels(data.categorical$previous)[3:6] <- "More than_3_times"

summary(data.categorical$previous)
ggplot(data.categorical, aes(x=previous, fill=previous )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") + ggtitle("Previous contacts per category")+
  theme(plot.title = element_text(hjust = 0.5))
data$previous <- data.categorical$previous

#' 
#' #### month
#' 
#' cut the months into intervals and label the levels
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
term <- cut(x = as.integer(data.categorical$month), breaks = c(0,3, 6, 9, 12), labels = c("Q1", "Q2", "Q3", "Q4"))
# paste 'term' and 'year' together
data.categorical$quarter <- as.factor(paste0(term))
table(ordered(data.categorical$quarter))
data$quarter <- data.categorical$quarter

data.categorical$month <- NULL
data$month <- NULL

#' 
#' Draw the level of each variable
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
lapply(data.categorical, levels)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x=campaign, fill=campaign )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") + ggtitle("Previous contacts per category")+
  theme(plot.title = element_text(hjust = 0.5))

table(data$campaign)

boxplot(data$campaign)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("caret")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret)

#' 
#' Look for zero variance/near zero variance & save results
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

nzv <- nearZeroVar(data, saveMetrics= TRUE)  #pdays has an NZV with a freqRatio of 117.633333 & a percentUnique of 0.052
nzv

#' 
#' remove nzv
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data.categorical <- data.categorical[, which(!colnames(data.categorical) %in% c("pdays", "default")), drop = F]
#data.categorical$subscribed <- imputed_data$subscribed
data <- cbind(data.numeric, data.categorical, imputed_data$subscribed)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(data))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(t(describe(data.numeric)), 2)

#' 
#' ## Univariate
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
require(Hmisc)
hist.data.frame(data.numeric, rugs = TRUE)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot_numerics_height <- function(i, frame) {
  plot(table(frame[, i]) / n, type = "h",
       xlim = range(frame[, i]) + c(-1, 1),
       main = names(frame)[i],
       ylab = "Relative frequency")
}

par(mfrow = c(3, 3));
n <- nrow(data.numeric)
lapply(1:ncol(data.numeric), plot_numerics_height, frame = data.numeric)

#' 
#' bar plots
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(data.categorical)

par(mfrow=c(3, 3))
lapply(data.categorical, function(x) barplot(table(x)))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("pastecs")


#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(pastecs)
stat.desc(data.numeric, basic=F)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(corrgram)
corrgram(data.numeric)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(psych)
# install.packages("GPArotation")
library(GPArotation)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cor(data.numeric)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p <- dim(data.numeric)[2]
for (i in 1:p){
  hist(data.numeric[,i], main=names(data.numeric)[i], probability=TRUE, xlab=names(data.numeric)[i])
  lines(density(data.numeric[,i]), col=2)
  index <- seq(min(data.numeric[,i]), max(data.numeric[,i]), length.out=100)
  ynorm <- dnorm(index, mean=mean(data.numeric[,i]), sd(data.numeric[,i]))
  lines(index, ynorm, col=3, lty=3, lwd=3)
}

#' 
#' ### Categorical
#' 
#' categorical - factors
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(gridExtra)
library(ggplot2)
library(RColorBrewer)
# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
plot_categorical_var <- function(i, frame){
  return(ggplot(frame, aes(x = as.factor(frame[,i]), fill =as.factor(frame[,i]))) +
           geom_bar() + coord_flip()+
           scale_fill_brewer(palette = "Paired") +
           ggtitle(names(frame)[i]) +
           xlab("") +
           theme(legend.position = "none"))
  
}
categorical_plots <- lapply(1:length(data.categorical), plot_categorical_var, frame = data.categorical)

library(cowplot)
cowplot::plot_grid(plotlist = categorical_plots, ncol = 3)

#' 
#' ### paired viz
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("sjPlot", "nloptr")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables
# install.packages("sjPlot", "nloptr");
library(corrplot)
library(sjPlot)
library(nortest)
lillie_fors_out <- data.frame(id=names(data.numeric), p_value=lillie.test(data.numeric[,i])$p.value)

lillie_fors_out

lapply(data.numeric, lillie.test)
# no normality on the numeric variables# don't use pearson correlation coefficientplot(data.numeric)par(mfrow=c(1,1))

#' 
#' hoef D test
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("Hmisc")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(Hmisc)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hoeffd.testing <- hoeffd(as.matrix(data.numeric))
hoeffd.testing$P

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(psych)
pairs.panels(data[, c(1:8,17)])
pairs.panels(mydata[, c(9:21)])

ggplot(bankdata, aes(job,education)) + geom_count(color='red') + theme_classic() + 
  ggtitle('job by education frequency') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(performance)

#' 
#' ### Data Partition
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(data$subscribed, p = .7, 
                                  list = FALSE, 
                                  times = 1)

dataTrain <- data[ trainIndex,]
dataTest  <- data[-trainIndex,]

#' 
#' ### Multicollinearity
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(car)
mbin=glm(subscribed~., family=binomial, data=dataTrain)
car::vif(mbin)

mbin.wo.poutcome =glm(subscribed~.-poutcome, family=binomial, data=dataTrain)
car::vif(mbin.wo.poutcome)

mbin.wo.poutcome_euribor =glm(subscribed~.-poutcome- euribor3m, family=binomial, data=dataTrain)
car::vif(mbin.wo.poutcome_euribor)
summary(mbin.wo.poutcome_euribor)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages('equatiomatic')

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(equatiomatic)
gsub("operatorname", "mathrm", extract_eq(mbin.wo.poutcome_euribor, use_coefs = T, show_distribution = T, wrap = T), fixed = T)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
formula <- subscribed~.-poutcome- euribor3m

#' 
#' We are now run Lasso glmnet
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("glmnet")
library(glmnet)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
x_matrix = model.matrix(~., dataTrain %>% select(-c("subscribed", "poutcome", "euribor3m")))
lambdas <- 10 ^ seq(8,-4,length=250)

lasso.cv <- cv.glmnet(x_matrix,dataTrain$subscribed, alpha=1, lambda=lambdas, family="binomial", type.measure='auc')
plot(lasso.cv)

lasso.1se.coefs <- coef(lasso.cv, s = "lambda.1se")
# save off only the significant variables
lasso.accepted_cols <-names(lasso.1se.coefs[which(lasso.1se.coefs[,'s1'] != 0),])[c(-1)]
lasso.accepted_cols

lasso.model <- glm(subscribed ~ age + campaign + pdays + previous + emp_var_rate +
                     cons_conf_idx + nr_employed + job + contact + quarter,
                   family=binomial, data = dataTrain)

summary(lasso.model)
car::vif(lasso.model)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gsub("operatorname", "mathrm", extract_eq(lasso.model, use_coefs = T, show_distribution = T, wrap = T), fixed = T)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
lasso_formula = subscribed ~ age + campaign + pdays + previous + 
    emp_var_rate + cons_conf_idx + nr_employed + job + contact + 
    quarter

#' 
#' ## Î-fold cross validation
#' 
#' ### Train/test
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

train_control <- trainControl(method = "cv",
                              number = 5,
                              classProbs = TRUE,
                              savePredictions = TRUE,
                              summaryFunction = twoClassSummary)

#' 
#' ### Setup Metrics
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
methods <- c('logistic', 'naiveBayes', 'tree', 'Random Forest')
accuracy <- matrix(data=NA, ncol= 1, nrow = length(methods))
ari <- matrix(data=NA, ncol= 1, nrow = length(methods))
roc <- matrix(data=NA, ncol= 1, nrow = length(methods))
sens <- matrix(data=NA, ncol= 1, nrow = length(methods))
spec <- matrix(data=NA, ncol= 1, nrow = length(methods))
rownames(accuracy) <- rownames(ari) <- rownames(roc) <- rownames(sens) <- rownames(spec) <- methods
i <- 1

#' 
#' ### Logit
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logit_train <- train(lasso_formula, data = dataTrain, method = "glm", family = "binomial", trControl = train_control)

logit_train_fitted <- predict(logit_train, dataTrain, type = "prob")
logit_train_fitted_pr <- ifelse(logit_train_fitted[,"yes"] > 0.5,"yes","no")
table(dataTrain$subscribed, logit_train_fitted_pr)

logit_test_fitted <- predict(logit_train, dataTest, type = "prob")
logit_test_fitted_pr <- ifelse(logit_test_fitted[,"yes"] > 0.5,"yes","no")
table(dataTest$subscribed, logit_test_fitted_pr)
print(logit_train)
ggplot(logit_train)

#' 
#' -   Report
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("MLeval")
library(MLeval)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
accuracy['logistic',i] <- sum(dataTest[,'subscribed'] == logit_test_fitted_pr)/dim(dataTest)[1]

ari['logistic',i] <- adjustedRandIndex(logit_test_fitted_pr, dataTest[,'subscribed'])

roc['logistic', i] <- logit_train$results$ROC

spec['logistic', i] <- logit_train$results$Spec
sens['logistic', i] <- logit_train$results$Sens

logistic_conf <- confusionMatrix(as.factor(logit_test_fitted_pr), dataTest$subscribed, positive = "yes")
as.table(logistic_conf)

#' 
#' -   Roc
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logit_evalm <- evalm(logit_train)

#' 
#' ### Naive-Bayes
#' 
#' bayes_train_fitted \<- predict(bayes_train, dataTrain, type = "prob")
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("e1071")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(e1071)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bayes_train <- train(formula, data = dataTrain, method = "naive_bayes", trControl = train_control)

bayes_train_fitted <- predict(bayes_train, dataTrain, type = "prob")

bayes_test_fitted <- predict(bayes_train, dataTest, type = "prob")
bayes_test_fitted_pr <- ifelse(bayes_test_fitted[,"yes"] > 0.5,"yes","no")
table(dataTest$subscribed, bayes_test_fitted_pr)

print(bayes_train)

#' 
#' -   Report
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
accuracy['naiveBayes',i] <- sum(dataTest[,'subscribed'] == bayes_test_fitted_pr)/dim(dataTest)[1]

ari['naiveBayes',i] <- adjustedRandIndex(bayes_test_fitted_pr, dataTest[,'subscribed'])
bayes_results <- bayes_train$results[bayes_train$results$usekernel == TRUE, ]
roc['naiveBayes', i] <- bayes_results$ROC

spec['naiveBayes', i] <- bayes_results$Spec
sens['naiveBayes', i] <- bayes_results$Sens


naivebayes_conf <- confusionMatrix(as.factor(bayes_test_fitted_pr), dataTest$subscribed, positive = "yes")
as.table(naivebayes_conf)

#' 
#' -   Roc
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logit_evalm <- evalm(bayes_train)

#' 
#' ### Tree
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("rpart")
library(rpart)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tree_train <- train(formula, data = dataTrain, method = "rpart", trControl = train_control)

tree_train_fitted <- predict(tree_train, dataTrain, type = "prob")
tree_test_fitted <- predict(tree_train, dataTest, type = "prob")
tree_test_fitted_pr <- ifelse(tree_test_fitted[,"yes"] > 0.5,"yes","no")
table(dataTest$subscribed, tree_test_fitted_pr)

print(tree_train)

#' 
#' -   Reporting
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
accuracy['tree',i] <- sum(dataTest[,'subscribed'] == tree_test_fitted_pr)/dim(dataTest)[1]

ari['tree',i] <- adjustedRandIndex(tree_test_fitted_pr, dataTest[,'subscribed'])

roc['tree', i] <- tree_train$results[1,]$ROC

spec['tree', i] <- tree_train$results[1,]$Spec
sens['tree', i] <- tree_train$results[1,]$Sens

tree_conf <- confusionMatrix(as.factor(tree_test_fitted_pr), dataTest$subscribed,positive='yes')
as.table(tree_conf)

tree_rox <- evalm(tree_train)

roc_imp2 <- varImp(tree_train, scale = FALSE)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("rattle")
library(rattle)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fancyRpartPlot(tree_train$finalModel, caption="")

#' 
#' #### RRforest (Not reported)
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("randomForest")
library(randomForest)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RF_train <- train(formula, data = dataTrain, method = "rf", trControl = train_control)

RF_train_fitted <- predict(RF_train, dataTrain, type = "prob")
RF_test_fitted <- predict(RF_train, dataTest, type = "prob")
RF_test_fitted_pr <- ifelse(RF_test_fitted[,"yes"] > 0.5,"yes","no")
table(dataTest$subscribed, RF_test_fitted_pr)

print(RF_train)

#' 
#' -   Report
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
accuracy['Random Forest',i] <- sum(dataTest[,'subscribed'] == RF_test_fitted_pr)/dim(dataTest)[1]

ari['Random Forest',i] <- adjustedRandIndex(RF_test_fitted_pr, dataTest[,'subscribed'])

roc['Random Forest', i] <- RF_train$results$ROC

spec['Random Forest', i] <- RF_train$results$Spec
sens['Random Forest', i] <- RF_train$results$Sens



#' 
#' ### Model Evaluation
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("mclust")

#' 
#' ## Part II
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

clustering_data <- data[, c("age", "job", "marital", "education", "default", "housing", "loan", "campaign", "previous", "pdays", "poutcome", "subscribed")]
dim(clustering_data)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(clustering_data, aes(previous, ..count..)) + geom_bar(aes(fill = pdays), position = "dodge")

ggplot(clustering_data, 
       aes(x = previous, 
           fill = pdays)) + 
  geom_bar(position = "stack")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(clustering_data, 
       aes(x = poutcome, 
           fill = pdays)) + 
  geom_bar(position = "stack")

ggplot(clustering_data, aes(poutcome, ..count..)) + geom_bar(aes(fill = previous), position = "dodge")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(clustering_data, 
       aes(x = poutcome, 
           fill = previous)) + 
  geom_bar(position = "stack")

ggplot(clustering_data, aes(poutcome, ..count..)) + geom_bar(aes(fill = previous), position = "dodge")

#' 
#' \
#' \
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("factoextra")
library(factoextra)

#' 
#' <https://www.r-bloggers.com/2016/06/clustering-mixed-data-types-in-r/>
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("cluster")
library(cluster)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Remove college name before clustering
sample_clust <- sample_n(clustering_data, 17000)
clust_subscribed <- sample_clust[, "subscribed"]
prop.table(table(clust_subscribed))

sample_clust <- sample_clust[, -ncol(sample_clust)]

gower_dist <- daisy(sample_clust,
                    metric = "gower")
# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"
summary(gower_dist)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pairs(sample_clust)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pam_fit <- pam(gower_dist, diss = TRUE, k = 7)
pam_results <- cbind(sample_clust, clust_subscribed)  %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("Rtsne")
library(Rtsne) # for t-SNE plot

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------#
# kMedoids Clustering - Internal Evaluation - Silhouette
#-----------------------------------------------------------------------------#
model_silhouette = silhouette(pam_fit$clustering, as.matrix(gower_dist))#den to bgazei gia megalo pinaka 
#model_silhouette

mean_sil=mean(model_silhouette[,3])
mean_sil

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#library(cluster)

plot(model_silhouette, main="ward", border=NA)


#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(stats)
pmfeatures<-data.frame(sample_clust, as.factor(pam_fit$clustering))
colnames(pmfeatures)[12] <- "cluster"
m1.mnv<-manova(cbind(poutcome, campaign)~cluster, data=pmfeatures)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sample_clust[pam_fit$medoids, ]
sample_clust2 = subset(sample_clust, select=-c(loan, default, pdays))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gower_dist2 <- daisy(sample_clust2,
                    metric = "gower")
# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"
#summary(gower_dist)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculate silhouette width for many k using PAM
sil_width2 <- c(NA)
for(i in 2:10){
  
  pam_fit2 <- pam(gower_dist2,
                 diss = TRUE,
                 k = i)
  
  sil_width2[i] <- pam_fit2$silinfo$avg.width
  
}
# Plot sihouette width (higher is better)
plot(1:10, sil_width2,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width2)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pam_fit2 <- pam(gower_dist2, diss = TRUE, k = 7)
pam_results2 <- cbind(sample_clust2, clust_subscribed)  %>%
  mutate(cluster = pam_fit2$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sample_clust2[pam_fit2$medoids, ]

#' 
#' ### Hierarchical clustering (not used)
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

hfit <- hclust(gower_dist, method = "complete")
plot(hfit)
rect.hclust(hfit, k = 6, border ="green")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hclust <- cutree(hfit, k=6)
plot(hclust)
dist2<-dist(sample_clust, method = 'manhattan')

plot(silhouette(cutree(hfit, k = 7), as.matrix(gower_dist)), main="complete", border=NA)

#' 
#' \
