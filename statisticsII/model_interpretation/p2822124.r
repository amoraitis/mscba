install.packages("readxl")
library("readxl")
library(tidyr)
library(dplyr)

data <- read_excel("E:/msc/project I  2021-2022.xls")
# TODO: clean
data1 <- read_excel("E:/msc/project I  2021-2022.xls")
table(data1$pdays)
data <- na.omit(data)
data <- drop_na(data)
dim(data)

data <- distinct(data)
dim(data)


# install.packages("janitor")
library(janitor)

data <- clean_names(data)
colnames(data)

str(data)

# duration not important - explain into report

data$duration <- NULL

## numeric frame

require(psych)

data.num_index <- sapply(data, class) == "numeric"
data.numeric <- data[, data.num_index]
data.numeric <- lapply(data.numeric, as.numeric)
data.numeric <- as.data.frame(data.numeric)
str(data.numeric)

# install MICE and check for NAs
# install.packages("mice")
library(mice)

md.pattern(data.numeric)
# numerics are OK.

## categorical frame

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


# there are a lot of obserbations with NA
# let's plot them
# install.packages("VIM")

# check categorical variables for NAs
md.pattern(data.categorical, plot = F)


# Missingness Plots
library(VIM)
mice_plot <- aggr(data.categorical, col = c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE, sortCombs = T,
                  labels=names(data.categorical), cex.axis=.8,
                  gap=3, ylab=c("Missing data","Pattern"), 
                  digits = 2, prop=F, only.miss=F)

capture.output(plot(mice_plot,
                    col = c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE, sortCombs = T,
                    labels=names(data.categorical), cex.axis=.8,
                    gap=3, ylab=c("Missing data","Pattern"), 
                    digits = 2, prop=F, only.miss=F), file = tempfile())
# it seems like the 21% of 'default' values variable is NA.
# others affect a smaller percent of the dataset but there are quite a few.

#we are going to bind the split dataset and use imputation
data <- cbind(data.numeric, data.categorical)
#imputation by mice
#converting the variables into factors

init = mice(data, maxit=0)
meth = init$method
predM = init$predictorMatrix

#specifying the suitable methods for each variable
meth[c("loan", "default", "housing")]="logreg" 
meth[c("job", "marital", "education")]="polyreg"

#running multiple imputation
imputed_data = mice(data, method=meth, predictorMatrix=predM, m=5)
data <- complete(imputed_data)
# verify that unknowns have been replaced.
md.pattern(data, plot = F)
# replace completed columns in the source data set
data.categorical$loan <- data$loan
data.categorical$default <- data$default
data.categorical$housing <- data$housing
data.categorical$job <- data$job
data.categorical$marital <- data$marital
data.categorical$education <- data$education


#===== Variable : pdays

#converting to factor type

data.categorical$pdays <- as.factor(data.numeric$pdays)
data.numeric$pdays <- NULL

summary(data.categorical$pdays)

levels(data.categorical$pdays)

# 999 is a valid value, which means this was the first time contact with the customer
length(ordered(unique(data$pdays)))
# Reducing the levels of this variable to 3.

levels(data.categorical$pdays)[1:10] <- "Contacted_in_first_10days"
levels(data.categorical$pdays)[2:11] <-"Contacted_after_10days"
levels(data.categorical$pdays)[3] <- "First_time_contacted"

library(ggplot2)
# Also,lets see the response rate of each levels. 
unique(data.numeric$pdays)
ggplot(data.categorical, aes(x=pdays, fill=pdays )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") + ggtitle("Previous days count per category")+
  theme(plot.title = element_text(hjust = 0.5))

# Response Rate is significantly higher for prospects that have been contacted within 27 days as compared to first time contacts

# Number of prospects under each category

table(data$pdays)
data$pdays <- data.categorical$pdays

#===== Variable : Education
# Let's see the education variables
levels(data.categorical$education)
plot(data.categorical$education)

# Reducing the levels of education variable

levels(data.categorical$education)[c(1:3,5)] <- "Primary_Education"
levels(data.categorical$education)[2] <- "Secondary_Education"
levels(data.categorical$education)[c(3:4)]<- "Tertiary_Education"
summary(data.categorical$education)
data$education <- data.categorical$education

ggplot(data.categorical, aes(x=education, fill=education )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") + ggtitle("Total contacts per education level")+
  theme(plot.title = element_text(hjust = 0.5))

#===== Variable : Age

# Outlier treatement in Age 
quantile(data$age,seq(0,1,0.01))
boxplot(data$age)

# Box plot shows quiet a no of outliers.

summary(data$age)
# Binning the age variable and store it into "binning.age" for the purpose of analysis.
data$age <- as.factor(cut(data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 100)))
data.categorical$age <- data$age
data.numeric$age <- NULL

#===== Variable : previous
# "previous" means, number of contacts performed before this campaign

summary(data.numeric$previous)
unique(data.numeric$previous)
# Max = 5, converting this variable to factor

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
#===== Variable : month
# cut the months into intervals and label the levels
term <- cut(x = as.integer(data.categorical$month), breaks = c(0,3, 6, 9, 12), labels = c("Q1", "Q2", "Q3", "Q4"))
# paste 'term' and 'year' together
data.categorical$quarter <- as.factor(paste0(term))
table(ordered(data.categorical$quarter))
data$quarter <- data.categorical$quarter

data.categorical$month <- NULL
data$month <- NULL

ggplot(data.categorical, aes(x=quarter, fill=quarter )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") + ggtitle("Contacts per quarter")+
  theme(plot.title = element_text(hjust = 0.5))

lapply(data.categorical, levels)

library(caret)

# look for zero variance/near zero variance & save results
nzv <- nearZeroVar(data, saveMetrics= TRUE)  #pdays has an NZV with a freqRatio of 117.633333 & a percentUnique of 0.052
nzv
# remove nzv
data.categorical <- data.categorical[, which(!colnames(data.categorical) %in% c("pdays", "default")), drop = F]
data.categorical$subscribed <- imputed_data$subscribed
data <- cbind(data.numeric, data.categorical)

sum(is.na(data))

round(t(describe(data.numeric)), 2)

# univariate 

require(Hmisc)
hist.data.frame(data.numeric, rugs = TRUE)

plot_numerics_height <- function(i, frame) {
  plot(table(frame[, i]) / n, type = "h",
       xlim = range(frame[, i]) + c(-1, 1),
       main = names(frame)[i],
       ylab = "Relative frequency")
}

par(mfrow = c(3, 3));
n <- nrow(data.numeric)
lapply(1:ncol(data.numeric), plot_numerics_height, frame = data.numeric)


# bar plots
dim(data.categorical)

par(mfrow=c(3, 3))

lapply(data.categorical, function(x) barplot(table(x)))


# install.packages("pastecs")
library(pastecs)
stat.desc(data.numeric, basic=F)

library(corrgram)
corrgram(data.numeric)

library(psych)
# install.packages("GPArotation")
library(GPArotation)
cor(data.numeric)

p <- dim(data.numeric)[2]
for (i in 1:p){
  hist(data.numeric[,i], main=names(data.numeric)[i], probability=TRUE, xlab=names(data.numeric)[i])
  lines(density(data.numeric[,i]), col=2)
  index <- seq(min(data.numeric[,i]), max(data.numeric[,i]), length.out=100)
  ynorm <- dnorm(index, mean=mean(data.numeric[,i]), sd(data.numeric[,i]))
  lines(index, ynorm, col=3, lty=3, lwd=3)
}

### categorical
# categorical - factors
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

## paired viz

# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
# install.packages("sjPlot", "nloptr"); install.packages("nloptr")
library(corrplot)
library(sjPlot)
library(nortest)
lillie_fors_out <- data.frame(id=names(data.numeric), p_value=lillie.test(data.numeric[,i])$p.value)
lillie_fors_out
lapply(data.numeric, lillie.test)
# no normality on the numeric variables
# don't use pearson correlation coefficient
plot(data.numeric)
par(mfrow=c(1,1))

## hoef D test

library(Hmisc)
hoeffd.testing <- hoeffd(as.matrix(data.numeric))
hoeffd.testing$P


# 

library(psych)
pairs.panels(data[, c(1:8,17)])
pairs.panels(mydata[, c(9:21)])

ggplot(bankdata, aes(job,education)) + geom_count(color='red') + theme_classic() + 
  ggtitle('job by education frequency') + theme(axis.text.x = element_text(angle = 90, hjust = 1))



library(performance)


mbin=glm(subscribed~., family=binomial, data=data)
summary(mbin)
with(mbin, pchisq(deviance, df.residual, lower.tail = FALSE))
car::vif(mbin)

mbin.wo.poutcome =glm(subscribed~.-poutcome, family=binomial, data=data)
summary(mbin.wo.poutcome)
car::vif(mbin.wo.poutcome)

mbin.wo.poutcome_euribor =glm(subscribed~.-poutcome- euribor3m, family=binomial, data=data)
summary(mbin.wo.poutcome_euribor)
car::vif(mbin.wo.poutcome_euribor)

mbin.wo.poutcome_euribor_empvarrate =glm(subscribed~.-poutcome- euribor3m - emp_var_rate, family=binomial, data=data)
summary(mbin.wo.poutcome_euribor_empvarrate)
car::vif(mbin.wo.poutcome_euribor_empvarrate)


model.full <- mbin.wo.poutcome_euribor_empvarrate

overdisp_ratio <- deviance(model.full)/df.residual(model.full)
measures <- rbind(measures, c("Full Model", AIC(model.full), BIC(model.full),
                              as.numeric(r2(model.full))[1],
                              overdisp_ratio))

# install.packages("equatiomatic")
library(equatiomatic)
gsub("operatorname", "mathrm", extract_eq(model.full, use_coefs = T, show_distribution = T, wrap = T), fixed = T)
######## BIC
######### Starting with the full model, find the model with the MIN BIC
#######

bic_proc <- step(model.full, direction='both',k=log(nrow(data)),trace=F)
summary(bic_proc)


model.from_bic <- glm(formula =subscribed ~ campaign + pdays + previous + cons_conf_idx + 
                        nr_employed + contact + quarter, family = binomial, 
                      data = data)

summary(model.from_bic)

r2<-r2(model.from_bic)
as.numeric(r2$R2_Tjur)[1]
overdisp_ratio <- deviance(model.from_bic)/df.residual(model.from_bic)
measures <- data.frame(Process=c("BIC"), AIC=c(AIC(model.from_bic)),
                       BIC=c(BIC(model.from_bic)),
                       "Tjur's R2"=as.numeric(r2(model.from_bic))[1],
                       "Overdispersion Ratio"=overdisp_ratio)
AIC(model.from_bic)
car::vif(model.from_bic)
check_collinearity(model.from_bic)

with(model.from_bic, pchisq(deviance, df.residual, lower.tail = FALSE))
confint(model.from_bic)
exp(cbind(OR = coef(model.from_bic), confint(model.from_bic)))

gsub("operatorname", "mathrm", extract_eq(model.from_bic, use_coefs = T, show_distribution = TRUE, wrap = TRUE), fixed = T)

###### AIC
##### Find the model with MIN AIC
aic_proc <- step(model.full, direction='both',trace=F)
summary(aic_proc)

model.from_aic <- glm(formula = subscribed ~ age + campaign + pdays + previous + 
                        cons_conf_idx + nr_employed + job + education + contact + 
                        dow + quarter, family = binomial, data = data)

summary(model.from_aic)
overdisp_ratio <- deviance(model.from_aic)/df.residual(model.from_aic)
measures <- rbind(measures, c("AIC", AIC(model.from_aic), BIC(model.from_aic),
                  as.numeric(r2(model.from_aic))[1],
                  overdisp_ratio))


AIC(model.from_aic)
car::vif(model.from_aic)

gsub("operatorname", "mathrm", extract_eq(model.from_bic, use_coefs = T, show_distribution = TRUE, wrap = F), fixed = T)

## 
library(lmtest)
report(lrtest(model.from_bic))
compare.bic.aic <- anova(model.from_aic, model.from_bic, test ="LR")
report(compare.bic.aic)
#### Lasso
#### take it one step further

library(gglasso)
sparse.matrix <- model.matrix(model.full)[,-1]

# install.packages("glmnet")
library(glmnet)
lasso <- cv.glmnet(sparse.matrix, data$subscribed, alpha =1, family=binomial)

lasso.1se.coefs <- coef(lasso, s = "lambda.1se")
# save off only the significant variables
lasso.accepted_cols <-names(lasso.1se.coefs[which(lasso.1se.coefs[,'s1'] != 0),])[c(-1)]
lasso.accepted_cols

lasso.model <- glm(subscribed ~ age + campaign + pdays + previous +
                     cons_conf_idx + nr_employed + job + education + contact + quarter,
                   family=binomial, data = data)

summary(lasso.model)
car::vif(lasso.model)
AIC(lasso.model)

overdisp_ratio <- deviance(lasso.model)/df.residual(lasso.model)
measures <- rbind(measures, c("Lasso", AIC(lasso.model), BIC(lasso.model),
                            as.numeric(r2(lasso.model))[1],
                  overdisp_ratio))

gsub("operatorname", "mathrm", extract_eq(lasso.model, use_coefs = T, show_distribution = TRUE, wrap = TRUE), fixed = T)


# Run AIC on LASSO

lasso_aic_proc <- step(lasso.model, direction='both', trace = F)
summary(lasso_aic_proc)

lasso.model.with_aic <- glm(formula = subscribed ~ age + campaign + pdays + previous + 
                              cons_conf_idx + nr_employed + job + education + contact + 
                              quarter, 
                            family = binomial, data = data)

summary(lasso.model.with_aic)

measures <- rbind(measures, c("LASSO then AIC", AIC(lasso.model.with_aic), BIC(lasso.model.with_aic), R2=as.numeric(r2(lasso.model.with_aic))[1]))

# Run BIC on LASSO

lasso_bic_proc <- step(lasso.model, direction='both', trace = F, k=log(nrow(data)))
summary(lasso_bic_proc)

lasso.model.with_bic <- glm(formula = subscribed ~ campaign + pdays + previous + cons_conf_idx + 
                              nr_employed + contact + quarter, 
                            family = binomial, data = data)

summary(lasso.model.with_bic)

measures <- rbind(measures, c("LASSO then BIC", AIC(lasso.model.with_bic), BIC(lasso.model.with_bic), R2=as.numeric(r2(lasso.model.with_bic))[1]))


anova(model.from_bic, lasso.model, model.from_aic,  model.full, test="LRT")

lasso.model$formula
model.from_aic$formula
model.from_bic$formula
lasso.model.with_aic$formula
lasso.model.with_bic$formula

report_performance(lasso.model, ci_method="wald")
report_performance(model.from_aic, ci_method="wald")
report_performance(model.from_bic, ci_method="wald")
report_statistics(model.from_bic, ci_method="wald")
report(model.from_bic, ci_method="wald")
# install.packages("report")
#devtools::install_github("easystats/report")
library(report)

remotes::install_github("easystats/report") # You only need to do that once
library("report")


##############Reporting ##############
# install.packages("stargazer")
library(stargazer)
stargazer(data.numeric, type = 'html', out = 'out.html')


report(model.from_aic, ci_method="wald")
