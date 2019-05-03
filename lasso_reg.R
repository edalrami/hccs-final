library(dplyr)
library(ggplot2)
library(glmnet)
X_train = read.csv("X_train.csv")
X_test = read.csv("X_test.csv")
y_train = read.csv("y_train.csv", header=FALSE)
colnames(y_train) = c("post_total_cost")
y_test = read.csv("y_test.csv", header=FALSE)
colnames(y_test) = c("post_total_cost")
X = rbind(X_train, X_test)
y = rbind(y_train, y_test)
colnames(y) = c('post_total_cost')

#Check normality of target variable
qqnorm(y_train$post_total_cost, pch = 1, frame = FALSE)

#normality condition is violated. Transform the data
qqnorm(log(y_train$post_total_cost), pch = 1, frame = FALSE)
qqline(log(y_train$post_total_cost), col = "steelblue", lwd = 2)


#Fit the lasso model to the data
grid = 10^seq(10,-2,length=100)

#X_train = cbind(X_train, y_train)
#colnames(X_train)
#any(is.na(X_train))

X_tr = model.matrix( ~ ., X_train)

lasso.mod = glmnet(X_tr, as.matrix(log(y_train)), alpha=1, lambda = grid)

set.seed(1)
cv.out = cv.glmnet(X_tr, as.matrix(log(y_train)), alpha=1 )
plot(cv.out)

#
#X_test = cbind(X_test, y_test)
X_ts = model.matrix( ~ ., X_test)

(bestlam = cv.out$lambda.min)

lasso.pred = predict(lasso.mod, s = bestlam, newx = X_ts)
mean((lasso.pred-y_test$post_total_cost)^2)


#X = cbind(X, y) 
X_ = model.matrix( ~ ., X)

out = glmnet(X_,as.matrix(log(y)),alpha=1, lambda = grid)
lasso.coef = predict(out, type="coefficients", s=bestlam)
features = lasso.coef

features = data.frame(name = features@Dimnames[[1]][features@i + 1], coefficient = features@x)

##Go through each row and determine if a value is zero
row_sub = apply(features, 1, function(row) all(row !=0 ))
##Subset coefficient names to get selected features for linear regression model
selected_features = features[row_sub,]$name
model_coef = features[row_sub,]$coefficient


##########################################################
#FEATURE SELETION FOR CLASSIFICATION - HYPOTHESIS TESTING
##########################################################

cat_names = read.csv("cat_names.csv", header=TRUE)
cont_names = read.csv("cont_names.csv", header=TRUE)

categorical <- as.character(unlist(cat_names[1,]))
categorical

continuous <- as.character(unlist(cont_names[1,]))
continuous

d2 = read.csv("projectTrain.csv")

#Subset for diabetic patients only

d2 = d2[d2$drug_class == '*ANTIDIABETICS*',]



getChi <- function(var, d){
  ##Return the chi-test hypothesis result
  #
  #Parameters: 
  #d: Dataframe containing cont and cat data
  #var: The column that we'll be doing chi-test on
  #
  #Returns: A list containing the var name and the pvalue
  t = d%>%
    select('pdc_80_flag', toString(var)) %>%
    table() %>%
    chisq.test()
  return(list(var , t$p.value))
}



chi_results = lapply(categorical, getChi, d2)
chi_results

chi_df = as.data.frame(matrix(unlist(chi_results), nrow = length(unlist(chi_results[1]))))
chi_df = t(chi_df)
colnames(chi_df) = c("var", "pval")
chi_df = as.data.frame(chi_df)

chi_df$var = as.character(chi_df$var)
chi_df$pval = as.numeric(as.character(chi_df$pval))

#Selected Categorical features for classification
selected_cat_features = chi_df[(chi_df$pval < 0.05),]
dim(selected_cat_features)

###################################################################
#USE THESE FEATURES FOR KNN, NAIVE BAYES AND LOGISTIC REGRESSION
###################################################################

#Selected features for logistic regression
selected_features2 = chi_df[(chi_df$pval < 0.05),]$var
selected_features2

selected_features2 = as.array(selected_features2) 

#### THESE ARE THE FEATURES TO USE FOR CLASSIFICATION
selected_features2



#Separate the categorical and cont data
x_cats = d2[,which(colnames(d2) %in% selected_features2)]
x_cont = d2[,which(colnames(d2) %in% continuous)]
pdc_80_flag = d2$pdc_80_flag

#Factorize categoricals
x_factors = model.matrix(pdc_80_flag ~ ., data=x_cats)[,-1]
dim(x_factors)

library(car)



#Get continuous features through hypothesis testing
getCont <- function(var, data){
   flag1 = data[data$pdc_80_flag == 1,]
   flag0 = data[data$pdc_80_flag == 0,]
   temp = as.data.frame(data[var])
   colnames(temp) = c("val")
   #Check for normality when pdc_80_flag is 1 and 0
   n1 = shapiro.test(flag1[,c(var)])
   n0 = shapiro.test(flag0[,c(var)])
   
   #If normality exists 
   if((n1$p.value > 0.05) & (n0$p.value > 0.05)){
     
     #Check for equal variance
     levene_ = leveneTest(temp$val ~ as.factor(data$pdc_80_flag))
     if(levene_$`Pr(>F)`[1] < 0.05){
       res = t.test(temp$val ~ data$pdc_80_flag, mu = 0, alternative = 'two.sided', conf = 0.95, var.eq = TRUE)
     }else{

       res = t.test(temp$val ~ data$pdc_80_flag, mu = 0, alternative = 'two.sided', conf = 0.95, var.eq = FALSE)
     }
     
   #If normality test doesnt pass then check medians
   }else{
     
     #Used for median comparison. Use if data is skewed
     res = wilcox.test(temp$val ~ data$pdc_80_flag, conf.int = T)
     
   }
  return(list(var, res$p.value))

}

test_stats_res = lapply(continuous, getCont, d2)


test_res = as.data.frame(matrix(unlist(test_stats_res), nrow = length(unlist(test_stats_res[1]))))
test_res = t(test_res)
colnames(test_res) = c("var", "pval")
test_res = as.data.frame(test_res)

test_res$var = as.character(test_res$var)
test_res$pval = as.numeric(as.character(test_res$pval))

selected_cont_features = test_res[(test_res$pval < 0.05),]
dim(selected_cont_features)
selected_cont_features






#x_ = as.matrix(data.frame(x_cont, x_factors))
#dim(x_)

#glmmod = glmnet(x_, y=as.factor(pdc_80_flag), alpha=1, family = "binomial")

#plot(glmmod, xvar = "lambda")

#glmmod

#coef(glmmod)[,10]

